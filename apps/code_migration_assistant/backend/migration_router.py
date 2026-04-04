"""Migration UI API ルーター.

エンドポイント一覧:
  POST /api/migrate/upload              - COBOLファイル/zipアップロード
  GET  /api/migrate/{task_id}/stream    - SSE進捗ストリーム
  POST /api/migrate/{task_id}/hitl      - HITL応答送信
  GET  /api/migrate/{task_id}/status    - タスクステータス取得
  GET  /api/migrate/{task_id}/backlog   - Backlog 全体取得
  GET  /api/migrate/{task_id}/backlog/tasks/{backlog_task_id} - Backlog単体取得
  GET  /api/migrate/{task_id}/download  - 成果物zipダウンロード
"""

from __future__ import annotations

import asyncio
import json
import logging
import shutil
import tempfile
import time
from pathlib import Path
from typing import TYPE_CHECKING, Annotated, Any

from fastapi import APIRouter, Depends, File, HTTPException, Query, Request, UploadFile
from fastapi.responses import FileResponse, StreamingResponse
from pydantic import BaseModel

from apps.code_migration_assistant.adapters.factory import AdapterFactory
from apps.code_migration_assistant.backend.migration_task_store import (
    HITLRequest as PendingHITLRequest,
)
from apps.code_migration_assistant.backend.migration_task_store import (
    TaskStatus,
    TaskStore,
    get_task_store,
)
from apps.code_migration_assistant.cobol_project import COBOLFile, COBOLProject
from apps.code_migration_assistant.engine import CodeMigrationEngine


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/migrate", tags=["migration"])

# 出力ルートディレクトリ（リポジトリルートからの絶対パス）
_DEFAULT_OUTPUT_ROOT = Path(__file__).parent.parent.parent.parent / "migration_output"

# バックグラウンドタスクの強参照セット（GCによる早期キャンセル防止）
_background_tasks: set[asyncio.Task[Any]] = set()
_STAGE_NODE_MAP: dict[str, str] = {
    "migration.analyze_code": "analysis",
    "migration.extract_business_semantics": "business_semantics",
    "migration.design_architecture": "design",
    "migration.transform_code": "transform",
    "migration.synthesize_tests": "tests",
    "migration.verify_diff": "diff",
    "migration.evaluate_quality": "quality",
    "migration.apply_fix": "fix",
    "migration.generate_report": "report",
}


class UploadResponse(BaseModel):
    """アップロードAPIレスポンス."""

    task_id: str
    stream_url: str
    file_count: int
    program_names: list[str]


class HITLSubmitRequest(BaseModel):
    """HITL応答リクエスト."""

    request_id: str
    approved: bool
    comment: str = ""
    modifications: dict[str, Any] = {}


class TaskStatusResponse(BaseModel):
    """タスクステータスレスポンス."""

    task_id: str
    status: str
    current_stage: str | None
    download_available: bool
    error: str | None


# ---------- ヘルパー関数 ----------


def _get_output_root() -> Path:
    """出力ルートディレクトリを返す（テスト時は上書き可能）."""

    return _DEFAULT_OUTPUT_ROOT


def _approval_bridge_path(task_id: str, request_id: str) -> Path:
    """cma_cli 承認応答の bridge file path を返す."""
    return _get_output_root() / "_runtime" / task_id / "approvals" / f"{request_id}.json"


def _build_timeline_event(
    *,
    stage: str,
    title: str,
    detail: str,
    status: str,
    executor: str | None,
    reason: str | None = None,
) -> dict[str, Any]:
    """フロント投影向け timeline event を生成する."""
    return {
        "type": "timeline",
        "event_type": "timeline",
        "stage": stage,
        "title": title,
        "detail": detail,
        "status": status,
        "executor": executor,
        "reason": reason,
        "timestamp": time.time(),
    }


def _resolve_stage_plan(stage: str) -> tuple[str | None, list[str]]:
    """ステージの実行器情報を返す."""
    factory = AdapterFactory()
    try:
        plan = factory.get_stage_execution_plan("cobol-to-java", stage)
    except ValueError:
        return None, []
    return plan.default_executor, list(plan.fallback_conditions)


def _normalize_engine_event(
    *,
    program_name: str,
    raw_event: dict[str, Any],
) -> list[dict[str, Any]]:
    """Engine イベントを migration SSE/timeline 契約へ変換する."""
    events: list[dict[str, Any]] = []
    event_name = str(raw_event.get("event", ""))
    node_name = str(raw_event.get("node", ""))
    stage = _STAGE_NODE_MAP.get(node_name)
    executor, fallback_conditions = _resolve_stage_plan(stage or "")

    if event_name == "node_start" and stage is not None:
        events.append(
            {
                "type": "stage_start",
                "stage": stage,
                "program_name": program_name,
                "message": f"{stage} 実行中...",
            }
        )
        events.append(
            _build_timeline_event(
                stage=stage,
                title=f"{program_name} / {stage}",
                detail="ステージを開始しました",
                status="running",
                executor=executor,
                reason="default_route",
            )
        )
        return events

    if event_name == "node_complete" and stage is not None:
        result_raw = raw_event.get("result")
        decision = ""
        if isinstance(result_raw, dict):
            decision_raw = result_raw.get("decision")
            if isinstance(decision_raw, str):
                decision = decision_raw
        events.append(
            {
                "type": "stage_complete",
                "stage": stage,
                "program_name": program_name,
                "decision": decision,
            }
        )
        events.append(
            _build_timeline_event(
                stage=stage,
                title=f"{program_name} / {stage}",
                detail="ステージを完了しました",
                status="complete",
                executor=executor,
                reason="completed",
            )
        )
        return events

    if str(raw_event.get("event_type", "")) == "approval_required":
        context_raw = raw_event.get("context", {})
        context = context_raw if isinstance(context_raw, dict) else {}
        unknowns_raw = context.get("design_unknowns", [])
        unknowns = [str(item) for item in unknowns_raw if item is not None]
        stage_name = str(context.get("stage", "design"))
        events.append(
            {
                "type": "hitl_required",
                "stage": stage_name,
                "program_name": program_name,
                "request_id": raw_event.get("request_id"),
                "reason": raw_event.get("reason", ""),
                "question": raw_event.get("reason", ""),
                "context": context,
                "unknowns": unknowns,
            }
        )
        events.append(
            _build_timeline_event(
                stage=stage_name,
                title=f"{program_name} / human review",
                detail=str(raw_event.get("reason", "")),
                status="waiting",
                executor=executor,
                reason="approval_required",
            )
        )
        return events

    if str(raw_event.get("event_type", "")) == "approval_submitted":
        approved = bool(raw_event.get("approved", False))
        events.append(
            _build_timeline_event(
                stage="design",
                title=f"{program_name} / human review",
                detail="承認応答を受理しました" if approved else "拒否応答を受理しました",
                status="complete" if approved else "error",
                executor="human",
                reason="approval_submitted",
            )
        )
        return events

    if str(raw_event.get("event_type", "")) == "approval_timeout":
        events.append(
            _build_timeline_event(
                stage="design",
                title=f"{program_name} / human review",
                detail="承認待機がタイムアウトしました",
                status="error",
                executor="human",
                reason="approval_timeout",
            )
        )
        if fallback_conditions:
            events.append(
                {
                    "type": "retry_decision",
                    "stage": "design",
                    "decision": "fallback_to_native",
                    "reason": "approval_timeout",
                    "fallback_conditions": fallback_conditions,
                }
            )
        return events

    return events


async def _run_engine_pipeline(
    *,
    task_id: str,
    cobol_files: list[COBOLFile],
    tmp_dir: Path,
    output_root: Path,
    fast_mode: bool,
    model: str,
    store: TaskStore,
) -> None:
    """CodeMigrationEngine を唯一 runtime として実行する."""
    del model
    await store.update_status(task_id, TaskStatus.RUNNING)

    run_root = output_root / task_id
    run_root.mkdir(parents=True, exist_ok=True)
    approvals_dir = _get_output_root() / "_runtime" / task_id / "approvals"
    approvals_dir.mkdir(parents=True, exist_ok=True)

    for cobol_file in cobol_files:
        engine = CodeMigrationEngine(migration_type="cobol-to-java")
        await engine._initialize()
        program_root = run_root / cobol_file.program_name
        runtime_inputs: dict[str, Any] = {
            "source_code": cobol_file.content,
            "task_id": task_id,
            "module": cobol_file.program_name,
            "migration_type": "cobol-to-java",
            "artifacts_dir": str(program_root),
            "fast_mode": fast_mode,
            "options": {
                "verification_mode": "fast" if fast_mode else "strict",
                "approval_bridge_dir": str(approvals_dir),
            },
        }
        final_result: dict[str, Any] | None = None

        async for raw_event in engine._execute_stream(runtime_inputs):
            normalized_events = _normalize_engine_event(program_name=cobol_file.program_name, raw_event=raw_event)
            for event in normalized_events:
                await store.push_event(task_id, event)
                stage_raw = event.get("stage")
                if isinstance(stage_raw, str) and stage_raw:
                    await store.update_status(task_id, TaskStatus.RUNNING, current_stage=stage_raw)
                if str(event.get("type", "")) == "hitl_required":
                    task_obj = await store.get(task_id)
                    if task_obj is not None:
                        unknowns_raw = event.get("unknowns", [])
                        unknowns = [str(item) for item in unknowns_raw if item is not None]
                        task_obj.pending_hitl = PendingHITLRequest(
                            request_id=str(event.get("request_id", "")),
                            stage=str(event.get("stage", "")),
                            artifact=event.get("context", {}) if isinstance(event.get("context"), dict) else {},
                            unknowns=unknowns,
                            question=str(event.get("question", "")),
                            response_event=asyncio.Event(),
                        )

            if (
                str(raw_event.get("event", "")) == "node_complete"
                and str(raw_event.get("node", "")) == "migration_pipeline"
            ):
                result_obj = raw_event.get("result")
                if isinstance(result_obj, dict):
                    final_result = result_obj

        if final_result is None:
            msg = f"engine result missing for {cobol_file.program_name}"
            raise RuntimeError(msg)

        evidence_packets = final_result.get("artifact_paths", {})
        if isinstance(evidence_packets, dict):
            await store.push_event(
                task_id,
                {
                    "type": "evidence",
                    "stage": "report",
                    "program_name": cobol_file.program_name,
                    "summary": "成果物を更新しました",
                    "artifact_paths": {str(key): str(value) for key, value in evidence_packets.items()},
                },
            )

    zip_path = Path(shutil.make_archive(str(run_root / "download"), "zip", root_dir=str(run_root)))
    await store.set_download_path(task_id, zip_path)
    await store.update_status(task_id, TaskStatus.COMPLETE, current_stage="quality")
    await store.push_event(
        task_id,
        {
            "type": "complete",
            "stage": "pipeline",
            "message": "engine runtime complete",
        },
    )
    shutil.rmtree(tmp_dir, ignore_errors=True)


async def _run_pipeline_background(
    task_id: str,
    source_path: Path,
    cobol_files: list[COBOLFile],
    tmp_dir: Path,
    output_root: Path,
    fast_mode: bool,
    model: str,
    store: TaskStore,
) -> None:
    """バックグラウンドでパイプラインを実行し、イベントをキューに投入する."""

    try:
        del source_path
        await _run_engine_pipeline(
            task_id=task_id,
            cobol_files=cobol_files,
            tmp_dir=tmp_dir,
            output_root=output_root,
            fast_mode=fast_mode,
            model=model,
            store=store,
        )
    except Exception as exc:
        logger.exception("パイプライン実行エラー: %s", exc)
        await store.update_status(task_id, TaskStatus.ERROR, error_message=str(exc))
        await store.push_event(task_id, {"type": "error", "stage": None, "message": str(exc)})
    finally:
        await store.close_events(task_id)
        shutil.rmtree(tmp_dir, ignore_errors=True)


async def _sse_generator(
    task_id: str,
    store: TaskStore,
    last_event_id: int = 0,
) -> AsyncIterator[str]:
    """SSEイベントを生成するジェネレータ.

    再接続時は last_event_id より後のイベントをリプレイしてから
    キューをドレインする。各イベントには SSE id: フィールドを付与する。
    """

    task = await store.get(task_id)
    if task is None:
        yield "data: " + json.dumps({"type": "error", "message": "タスクが存在しません"}) + "\n\n"
        return

    if last_event_id > 0:
        missed = await store.get_events_since(task_id, last_event_id)
        for event_id, event_data in missed:
            yield f"id: {event_id}\ndata: " + json.dumps(event_data, ensure_ascii=False) + "\n\n"

    while True:
        try:
            item = await asyncio.wait_for(task.events.get(), timeout=30.0)
        except TimeoutError:
            yield ": ping\n\n"
            continue

        if item is None:  # sentinel
            break

        event_id, event_data = item
        yield f"id: {event_id}\ndata: " + json.dumps(event_data, ensure_ascii=False) + "\n\n"


# ---------- エンドポイント ----------


@router.post("/upload", response_model=UploadResponse)
async def upload_cobol(
    file: Annotated[UploadFile, File(description="COBOLファイルまたはzipアーカイブ")],
    fast: bool = Query(default=True, description="実行比較スキップ（Java/gnucobol不要）"),
    model: str = Query(default="claude-opus-4-6", description="使用するClaudeモデルID"),
    store: TaskStore = Depends(get_task_store),
) -> UploadResponse:
    """COBOLファイルをアップロードして移行タスクを開始する."""

    if not file.filename:
        raise HTTPException(status_code=400, detail="ファイル名が指定されていません")

    allowed_exts = {".cbl", ".cob", ".cobol", ".zip"}
    suffix = Path(file.filename).suffix.lower()
    if suffix not in allowed_exts:
        raise HTTPException(
            status_code=400,
            detail=f"対応外のファイル形式: {suffix}。{allowed_exts} のみ対応しています",
        )

    tmp_dir = Path(tempfile.mkdtemp(prefix="migration_upload_"))
    try:
        upload_path = tmp_dir / Path(file.filename).name
        content = await file.read()
        upload_path.write_bytes(content)

        project = COBOLProject(upload_path, tmp_dir)
        project.setup()
        cobol_files = project.get_cobol_files()
        if not cobol_files:
            shutil.rmtree(tmp_dir, ignore_errors=True)
            raise HTTPException(
                status_code=422,
                detail="COBOLファイルが見つかりません（.cbl/.cob/.cobol）",
            )

        task = await store.create()
        output_root = _get_output_root()
        output_root.mkdir(parents=True, exist_ok=True)

        bg_task = asyncio.create_task(
            _run_pipeline_background(
                task_id=task.task_id,
                source_path=upload_path,
                cobol_files=cobol_files,
                tmp_dir=tmp_dir,
                output_root=output_root,
                fast_mode=fast,
                model=model,
                store=store,
            )
        )
        _background_tasks.add(bg_task)
        bg_task.add_done_callback(_background_tasks.discard)

        return UploadResponse(
            task_id=task.task_id,
            stream_url=f"/api/migrate/{task.task_id}/stream",
            file_count=len(cobol_files),
            program_names=[entry.program_name for entry in cobol_files],
        )

    except HTTPException:
        shutil.rmtree(tmp_dir, ignore_errors=True)
        raise
    except Exception as exc:
        shutil.rmtree(tmp_dir, ignore_errors=True)
        logger.exception("アップロード処理エラー: %s", exc)
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{task_id}/stream")
async def stream_events(
    task_id: str,
    request: Request,
    store: TaskStore = Depends(get_task_store),
) -> StreamingResponse:
    """SSEでパイプライン進捗をストリームする."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    try:
        last_event_id = int(request.headers.get("Last-Event-ID", "0"))
    except ValueError:
        last_event_id = 0

    return StreamingResponse(
        _sse_generator(task_id, store, last_event_id),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "X-Accel-Buffering": "no",
        },
    )


@router.post("/{task_id}/hitl")
async def submit_hitl(
    task_id: str,
    body: HITLSubmitRequest,
    store: TaskStore = Depends(get_task_store),
) -> dict[str, str]:
    """HITL（Human-In-The-Loop）確認結果を送信する."""
    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    bridge_path = _approval_bridge_path(task_id, body.request_id)
    bridge_path.parent.mkdir(parents=True, exist_ok=True)
    bridge_path.write_text(
        json.dumps(
            {
                "approved": body.approved,
                "comment": body.comment,
                "modifications": body.modifications,
                "approver": "operator",
            },
            ensure_ascii=False,
            indent=2,
        ),
        encoding="utf-8",
    )

    success = await store.submit_hitl_response(
        task_id=task_id,
        request_id=body.request_id,
        response={
            "approved": body.approved,
            "comment": body.comment,
            "modifications": body.modifications,
        },
    )
    if not success:
        raise HTTPException(
            status_code=409,
            detail="対応するHITL要求が存在しないか、request_idが一致しません",
        )
    return {"status": "resumed"}


@router.get("/{task_id}/status", response_model=TaskStatusResponse)
async def get_status(
    task_id: str,
    store: TaskStore = Depends(get_task_store),
) -> TaskStatusResponse:
    """タスクの現在ステータスを取得する."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")
    return TaskStatusResponse(**task.to_status_dict())


@router.get("/{task_id}/backlog")
async def get_backlog(task_id: str) -> dict[str, Any]:
    """Backlog state を返す."""
    backlog_path = _get_output_root() / task_id / "backlog" / "backlog.json"
    if not backlog_path.exists():
        raise HTTPException(status_code=404, detail="Backlog not found")
    try:
        payload = json.loads(backlog_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise HTTPException(status_code=500, detail=f"Invalid backlog json: {exc}") from exc
    if not isinstance(payload, dict):
        raise HTTPException(status_code=500, detail="Invalid backlog payload")
    return payload


@router.get("/{task_id}/backlog/tasks/{backlog_task_id}")
async def get_backlog_task(task_id: str, backlog_task_id: str) -> dict[str, Any]:
    """Backlog 内の単一 task を返す."""
    backlog = await get_backlog(task_id)
    tasks = backlog.get("tasks", [])
    if not isinstance(tasks, list):
        raise HTTPException(status_code=500, detail="Invalid backlog tasks")
    for task in tasks:
        if not isinstance(task, dict):
            continue
        if str(task.get("task_id", "")) == backlog_task_id:
            return task
    raise HTTPException(status_code=404, detail="Backlog task not found")


@router.get("/{task_id}/download")
async def download_result(
    task_id: str,
    store: TaskStore = Depends(get_task_store),
) -> FileResponse:
    """移行成果物のzipをダウンロードする."""

    task = await store.get(task_id)
    if task is None:
        raise HTTPException(status_code=404, detail=f"タスクが存在しません: {task_id}")

    if task.status != TaskStatus.COMPLETE:
        raise HTTPException(
            status_code=409,
            detail=f"タスクがまだ完了していません: {task.status.value}",
        )

    if task.download_path is None or not task.download_path.exists():
        raise HTTPException(status_code=404, detail="ダウンロードファイルが見つかりません")

    return FileResponse(
        path=str(task.download_path),
        media_type="application/zip",
        filename=task.download_path.name,
    )
