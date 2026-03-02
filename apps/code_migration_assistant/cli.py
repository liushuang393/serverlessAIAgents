"""Code Migration Assistant CLI.

契約実行モード（run）:
    python -m apps.code_migration_assistant.cli run \
      --input /path/to/input.json \
      --output /path/to/output.json \
      --events /path/to/events.ndjson

入力 JSON 例:
{
  "task_id": "task-123",
  "source_path": "/tmp/sample.cbl",
  "output_root": "/tmp/migration_output",
  "fast_mode": true,
  "migration_type": "cobol-to-java",
  "model": "claude-opus-4-6",
  "options": {}
}
"""

from __future__ import annotations

import argparse
import asyncio
import json
import sys
import tempfile
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.migration_studio.pipeline.project import COBOLProject


_PASS_DECISIONS = {"PASSED", "KNOWN_LEGACY"}
_NODE_STAGE_MAP = {
    "migration.analyze_code": "analyzer",
    "migration.design_architecture": "designer",
    "migration.transform_code": "transformer",
    "migration.synthesize_tests": "test_generator",
    "migration.verify_diff": "verifier",
    "migration.evaluate_quality": "quality_gate",
}


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def _append_event(path: Path, event: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(event, ensure_ascii=False) + "\n")


def _normalize_stream_event(
    *,
    program_name: str,
    raw_event: dict[str, Any],
) -> dict[str, Any] | None:
    """Engine 内部イベントを Studio 互換イベントへ変換する."""
    raw_event_type = str(raw_event.get("event") or raw_event.get("event_type") or "")

    if raw_event_type == "node_start":
        stage = _NODE_STAGE_MAP.get(str(raw_event.get("node", "")))
        if stage is None:
            return None
        return {
            "type": "stage_start",
            "stage": stage,
            "program_name": program_name,
            "message": f"{stage} 実行中...",
        }

    if raw_event_type == "node_complete":
        node_name = str(raw_event.get("node", ""))
        if node_name == "migration_pipeline":
            return None
        stage = _NODE_STAGE_MAP.get(node_name)
        if stage is None:
            return None
        event: dict[str, Any] = {
            "type": "stage_complete",
            "stage": stage,
            "program_name": program_name,
        }
        result = raw_event.get("result")
        if isinstance(result, dict) and stage == "quality_gate":
            decision = result.get("decision")
            if isinstance(decision, str):
                event["decision"] = decision
        return event

    if raw_event_type == "approval_required":
        return {
            "type": "hitl_required",
            "stage": "designer",
            "program_name": program_name,
            "request_id": raw_event.get("request_id"),
            "reason": raw_event.get("reason", ""),
            "context": raw_event.get("context", {}),
        }

    return None


async def _run_engine_for_program(
    *,
    source_code: str,
    task_id: str,
    program_name: str,
    artifacts_dir: Path,
    fast_mode: bool,
    migration_type: str,
    options: dict[str, Any],
    on_event: Any,
) -> dict[str, Any]:
    """単一プログラムを実行し、進捗イベントをコールバックへ渡す."""
    engine = CodeMigrationEngine(migration_type=migration_type)
    await engine._initialize()

    inputs = {
        "source_code": source_code,
        "task_id": task_id,
        "module": program_name,
        "artifacts_dir": str(artifacts_dir),
        "fast_mode": fast_mode,
        "options": options,
    }

    final_result: dict[str, Any] | None = None
    async for raw_event in engine._execute_stream(inputs):
        if not isinstance(raw_event, dict):
            continue
        normalized = _normalize_stream_event(program_name=program_name, raw_event=raw_event)
        if normalized is not None:
            await on_event(normalized)

        if (
            str(raw_event.get("event", "")) == "node_complete"
            and str(raw_event.get("node", "")) == "migration_pipeline"
        ):
            result_obj = raw_event.get("result")
            if isinstance(result_obj, dict):
                final_result = result_obj

    if final_result is None:
        return {
            "success": False,
            "quality_gate": {"decision": "ENV_ISSUE"},
            "error": "pipeline_result_missing",
            "artifact_paths": {},
        }
    return final_result


def _extract_program_result(
    *,
    program_name: str,
    result: dict[str, Any],
) -> dict[str, Any]:
    """Engine 結果から CLI 契約のプログラム結果へ正規化する."""
    quality_gate = result.get("quality_gate")
    decision = "ENV_ISSUE"
    if isinstance(quality_gate, dict):
        candidate = quality_gate.get("decision")
        if isinstance(candidate, str) and candidate:
            decision = candidate

    check_result = result.get("check_result")
    confidence = 0.0
    if isinstance(check_result, dict):
        confidence_raw = check_result.get("confidence")
        if isinstance(confidence_raw, (float, int)):
            confidence = float(confidence_raw)

    artifact_paths = result.get("artifact_paths")
    if not isinstance(artifact_paths, dict):
        artifact_paths = {}

    return {
        "program_name": program_name,
        "success": bool(result.get("success", False)),
        "decision": decision,
        "class_name": str(result.get("class_name", "MigratedProgram")),
        "target_code": str(result.get("target_code", "")),
        "iterations": int(result.get("iterations", 1)),
        "quality_score": round(confidence * 100.0, 2),
        "artifact_paths": artifact_paths,
        "raw_result": result,
    }


def _resolve_final_decision(program_results: list[dict[str, Any]]) -> str:
    for item in program_results:
        decision = str(item.get("decision", "ENV_ISSUE"))
        if decision not in _PASS_DECISIONS:
            return decision
    return "PASSED"


async def run_contract_payload(
    payload: dict[str, Any],
    *,
    on_event: Any,
) -> tuple[dict[str, Any], int]:
    """JSON 入力を受け取り契約実行する."""
    source_path_raw = payload.get("source_path")
    if not isinstance(source_path_raw, str) or not source_path_raw:
        return {"success": False, "error": "source_path is required"}, 2

    source_path = Path(source_path_raw).resolve()
    if not source_path.exists():
        return {"success": False, "error": f"source_path not found: {source_path}"}, 2

    output_root_raw = payload.get("output_root", "migration_output")
    if not isinstance(output_root_raw, str) or not output_root_raw:
        return {"success": False, "error": "output_root must be a non-empty string"}, 2
    output_root = Path(output_root_raw).resolve()
    output_root.mkdir(parents=True, exist_ok=True)

    task_id_raw = payload.get("task_id")
    task_id = str(task_id_raw) if isinstance(task_id_raw, str) and task_id_raw else f"task-{uuid.uuid4().hex[:12]}"
    fast_mode = bool(payload.get("fast_mode", True))

    migration_type_raw = payload.get("migration_type", "cobol-to-java")
    migration_type = str(migration_type_raw) if isinstance(migration_type_raw, str) else "cobol-to-java"

    model = payload.get("model")
    options_raw = payload.get("options", {})
    options: dict[str, Any] = options_raw if isinstance(options_raw, dict) else {}
    if isinstance(model, str) and model:
        options.setdefault("model", model)

    run_output_dir = output_root / task_id
    run_output_dir.mkdir(parents=True, exist_ok=True)

    program_results: list[dict[str, Any]] = []
    try:
        with tempfile.TemporaryDirectory(prefix="cma_cli_") as work_dir_str:
            project = COBOLProject(source=source_path, work_dir=Path(work_dir_str))
            project.setup()
            cobol_files = project.get_cobol_files()
            if not cobol_files:
                return {"success": False, "error": "COBOL files not found in source_path"}, 2

            for cobol_file in cobol_files:
                program_name = cobol_file.program_name
                program_task_id = f"{task_id}-{program_name.lower()}"
                program_artifacts_dir = run_output_dir / program_name
                program_artifacts_dir.mkdir(parents=True, exist_ok=True)

                await on_event(
                    {
                        "type": "stage_start",
                        "stage": "pipeline",
                        "program_name": program_name,
                        "message": f"移行パイプライン開始: {program_name}",
                    }
                )

                result = await _run_engine_for_program(
                    source_code=cobol_file.content,
                    task_id=program_task_id,
                    program_name=program_name,
                    artifacts_dir=program_artifacts_dir,
                    fast_mode=fast_mode,
                    migration_type=migration_type,
                    options=options,
                    on_event=on_event,
                )
                normalized_result = _extract_program_result(program_name=program_name, result=result)
                program_results.append(normalized_result)

                await on_event(
                    {
                        "type": "stage_complete",
                        "stage": "pipeline",
                        "program_name": program_name,
                        "decision": normalized_result["decision"],
                    }
                )

    except Exception as exc:
        await on_event({"type": "error", "stage": None, "message": str(exc)})
        return {"success": False, "error": str(exc)}, 2

    success = all(
        bool(item.get("success")) and str(item.get("decision")) in _PASS_DECISIONS
        for item in program_results
    )
    final_decision = _resolve_final_decision(program_results)
    artifact_paths = {
        str(item["program_name"]): dict(item.get("artifact_paths", {}))
        for item in program_results
    }
    report_path = ""
    for item in program_results:
        paths = item.get("artifact_paths", {})
        if isinstance(paths, dict):
            candidate = paths.get("report")
            if isinstance(candidate, str) and candidate:
                report_path = candidate
                break

    summary = {
        "success": success,
        "task_id": task_id,
        "decision": final_decision,
        "output_dir": str(run_output_dir),
        "program_results": program_results,
        "artifact_paths": artifact_paths,
        "report_path": report_path,
        "error": None,
    }

    await on_event(
        {
            "type": "complete",
            "stage": "pipeline",
            "program_name": program_results[0]["program_name"] if len(program_results) == 1 else "MULTI",
            "program_names": [str(item["program_name"]) for item in program_results],
            "decision": final_decision,
            "output_dir": str(run_output_dir),
            "version": 1,
        }
    )

    if success:
        return summary, 0
    return summary, 1


async def migrate_cobol_file(file_path: str) -> dict[str, Any]:
    """既存互換用の単一ファイル移行ヘルパー."""
    payload = {
        "task_id": f"legacy-{uuid.uuid4().hex[:8]}",
        "source_path": str(Path(file_path).resolve()),
        "output_root": str((Path.cwd() / "migration_output").resolve()),
        "fast_mode": True,
        "migration_type": "cobol-to-java",
        "options": {},
    }

    async def _ignore_event(_event: dict[str, Any]) -> None:
        return None

    summary, _ = await run_contract_payload(payload, on_event=_ignore_event)
    program_results = summary.get("program_results", [])
    first = program_results[0] if isinstance(program_results, list) and program_results else {}

    java_code = str(first.get("target_code", ""))
    return {
        "success": bool(summary.get("success", False)),
        "class_name": str(first.get("class_name", "MigratedProgram")),
        "score": float(first.get("quality_score", 0.0)),
        "iterations": int(first.get("iterations", 1)),
        "is_acceptable": bool(summary.get("success", False)),
        "java_code": java_code,
        "feedback": [],
        "errors": [] if summary.get("success", False) else [str(summary.get("error", "migration failed"))],
    }


async def _run_contract_command(args: argparse.Namespace) -> int:
    input_path = Path(args.input).resolve()
    output_path = Path(args.output).resolve()
    events_path = Path(args.events).resolve()

    if not input_path.exists():
        _write_json(output_path, {"success": False, "error": f"input file not found: {input_path}"})
        return 2

    try:
        payload_raw = json.loads(input_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        _write_json(output_path, {"success": False, "error": f"invalid input json: {exc}"})
        return 2
    if not isinstance(payload_raw, dict):
        _write_json(output_path, {"success": False, "error": "input json root must be object"})
        return 2

    events_path.parent.mkdir(parents=True, exist_ok=True)
    if events_path.exists():
        events_path.unlink()

    async def _event_writer(event: dict[str, Any]) -> None:
        _append_event(events_path, event)

    summary, exit_code = await run_contract_payload(payload_raw, on_event=_event_writer)
    _write_json(output_path, summary)
    return exit_code


async def _run_migrate_command(args: argparse.Namespace) -> int:
    source_path = Path(args.source).resolve()
    output_root = Path(args.output).resolve()

    payload = {
        "task_id": f"cli-{uuid.uuid4().hex[:8]}",
        "source_path": str(source_path),
        "output_root": str(output_root),
        "fast_mode": bool(args.fast),
        "migration_type": "cobol-to-java",
        "model": args.model,
        "options": {},
    }

    async def _print_event(event: dict[str, Any]) -> None:
        event_type = str(event.get("type", ""))
        stage = str(event.get("stage", ""))
        message = str(event.get("message", event.get("decision", "")))
        if event_type in {"stage_start", "stage_complete"}:
            print(f"[{event_type}] {stage} {message}")
        elif event_type == "complete":
            print(f"[complete] decision={event.get('decision')} output={event.get('output_dir')}")
        elif event_type == "error":
            print(f"[error] {event.get('message')}", file=sys.stderr)

    summary, exit_code = await run_contract_payload(payload, on_event=_print_event)
    if summary.get("success"):
        print(json.dumps(summary, ensure_ascii=False, indent=2))
    else:
        print(json.dumps(summary, ensure_ascii=False, indent=2), file=sys.stderr)
    return exit_code


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="code_migration_assistant",
        description="Code Migration Assistant CLI",
    )
    subparsers = parser.add_subparsers(dest="command")

    run_parser = subparsers.add_parser(
        "run",
        help="JSON入出力契約で非対話実行する",
    )
    run_parser.add_argument("--input", required=True, help="入力JSONファイル")
    run_parser.add_argument("--output", required=True, help="出力JSONファイル")
    run_parser.add_argument("--events", required=True, help="NDJSONイベント出力ファイル")

    migrate_parser = subparsers.add_parser(
        "migrate",
        help="単体CLI実行（人間向け）",
    )
    migrate_parser.add_argument("source", help="COBOLファイル/zip/ディレクトリ")
    migrate_parser.add_argument(
        "--output",
        "-o",
        default="migration_output",
        help="成果物出力ディレクトリ",
    )
    migrate_parser.add_argument(
        "--fast",
        action="store_true",
        help="高速モード（実行比較をスキップ）",
    )
    migrate_parser.add_argument(
        "--model",
        default="claude-opus-4-6",
        help="使用モデル識別子（オプション情報として保存）",
    )

    return parser


async def _async_main(argv: list[str] | None = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)

    if args.command == "run":
        return await _run_contract_command(args)
    if args.command == "migrate":
        return await _run_migrate_command(args)

    parser.print_help()
    return 2


def main(argv: list[str] | None = None) -> None:
    exit_code = asyncio.run(_async_main(argv))
    raise SystemExit(exit_code)


if __name__ == "__main__":
    main()
