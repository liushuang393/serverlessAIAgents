"""Migration UI 実行アダプタ.

UI は実行責務を保持せず、CMA CLI を呼び出して進捗イベントを中継する。
"""

from __future__ import annotations

import asyncio
import json
import shutil
import sys
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


@dataclass
class ExecutionConfig:
    """実行設定."""

    source_path: Path
    output_root: Path
    fast_mode: bool
    model: str
    options: dict[str, Any]


@dataclass
class ExecutionResult:
    """実行結果."""

    success: bool
    decision: str
    output_dir: Path | None
    summary: dict[str, Any]
    error: str | None = None


@dataclass
class _ExecutionHandle:
    """内部実行ハンドル."""

    task_id: str
    process: asyncio.subprocess.Process
    input_path: Path
    output_path: Path
    events_path: Path


class ExecutionAdapter:
    """実行アダプタ抽象."""

    async def start(self, task_id: str, config: ExecutionConfig) -> None:
        raise NotImplementedError

    def stream_events(self, task_id: str) -> AsyncIterator[dict[str, Any]]:
        raise NotImplementedError

    async def await_result(self, task_id: str) -> ExecutionResult:
        raise NotImplementedError


class CmaCliExecutionAdapter(ExecutionAdapter):
    """CMA CLI（run契約）を実行するアダプタ."""

    def __init__(self, runtime_root: Path) -> None:
        self._runtime_root = runtime_root
        self._runtime_root.mkdir(parents=True, exist_ok=True)
        self._handles: dict[str, _ExecutionHandle] = {}

    async def start(self, task_id: str, config: ExecutionConfig) -> None:
        runtime_dir = self._runtime_root / task_id
        runtime_dir.mkdir(parents=True, exist_ok=True)

        input_path = runtime_dir / "input.json"
        output_path = runtime_dir / "output.json"
        events_path = runtime_dir / "events.ndjson"

        payload = {
            "task_id": task_id,
            "source_path": str(config.source_path),
            "output_root": str(config.output_root),
            "fast_mode": config.fast_mode,
            "model": config.model,
            "options": config.options,
            "migration_type": "cobol-to-java",
        }
        input_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
        if events_path.exists():
            events_path.unlink()
        if output_path.exists():
            output_path.unlink()

        cmd = [
            sys.executable,
            "-m",
            "apps.code_migration_assistant.cli",
            "run",
            "--input",
            str(input_path),
            "--output",
            str(output_path),
            "--events",
            str(events_path),
        ]
        process = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        self._handles[task_id] = _ExecutionHandle(
            task_id=task_id,
            process=process,
            input_path=input_path,
            output_path=output_path,
            events_path=events_path,
        )

    async def stream_events(self, task_id: str) -> AsyncIterator[dict[str, Any]]:
        handle = self._handles.get(task_id)
        if handle is None:
            yield {"type": "error", "stage": None, "message": f"task not started: {task_id}"}
            return

        cursor = 0
        while True:
            if handle.events_path.exists():
                with handle.events_path.open("r", encoding="utf-8") as file:
                    file.seek(cursor)
                    lines = file.readlines()
                    cursor = file.tell()
                for line in lines:
                    text = line.strip()
                    if not text:
                        continue
                    try:
                        event = json.loads(text)
                    except json.JSONDecodeError:
                        yield {
                            "type": "error",
                            "stage": None,
                            "message": f"invalid NDJSON event: {text[:120]}",
                        }
                        continue
                    if isinstance(event, dict):
                        yield event

            if handle.process.returncode is not None:
                if handle.events_path.exists():
                    with handle.events_path.open("r", encoding="utf-8") as file:
                        file.seek(cursor)
                        lines = file.readlines()
                        cursor = file.tell()
                    for line in lines:
                        text = line.strip()
                        if not text:
                            continue
                        try:
                            event = json.loads(text)
                        except json.JSONDecodeError:
                            yield {
                                "type": "error",
                                "stage": None,
                                "message": f"invalid NDJSON event: {text[:120]}",
                            }
                            continue
                        if isinstance(event, dict):
                            yield event
                break

            await asyncio.sleep(0.2)

    async def await_result(self, task_id: str) -> ExecutionResult:
        handle = self._handles.get(task_id)
        if handle is None:
            return ExecutionResult(
                success=False,
                decision="ENV_ISSUE",
                output_dir=None,
                summary={},
                error=f"task not started: {task_id}",
            )

        stdout_raw, stderr_raw = await handle.process.communicate()
        _ = stdout_raw

        if not handle.output_path.exists():
            stderr_text = stderr_raw.decode("utf-8", errors="replace") if stderr_raw else ""
            return ExecutionResult(
                success=False,
                decision="ENV_ISSUE",
                output_dir=None,
                summary={},
                error=f"output json not found. stderr={stderr_text.strip()}",
            )

        try:
            summary_raw = json.loads(handle.output_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            return ExecutionResult(
                success=False,
                decision="ENV_ISSUE",
                output_dir=None,
                summary={},
                error=f"invalid output json: {exc}",
            )

        if not isinstance(summary_raw, dict):
            return ExecutionResult(
                success=False,
                decision="ENV_ISSUE",
                output_dir=None,
                summary={},
                error="output json root must be object",
            )

        output_dir = None
        output_dir_raw = summary_raw.get("output_dir")
        if isinstance(output_dir_raw, str) and output_dir_raw:
            output_dir = Path(output_dir_raw)

        decision_raw = summary_raw.get("decision", "ENV_ISSUE")
        decision = str(decision_raw) if isinstance(decision_raw, str) else "ENV_ISSUE"
        success = bool(summary_raw.get("success", False))
        if handle.process.returncode not in (0, 1) and success:
            success = False
            decision = "ENV_ISSUE"

        return ExecutionResult(
            success=success,
            decision=decision,
            output_dir=output_dir,
            summary=summary_raw,
            error=str(summary_raw.get("error")) if summary_raw.get("error") else None,
        )

    @staticmethod
    def create_download_package(output_dir: Path, task_id: str) -> Path:
        """実行出力ディレクトリ全体を zip 化する."""
        timestamp = datetime.now(UTC).strftime("%Y%m%d_%H%M%S")
        zip_base = output_dir.parent / f"download_{task_id}_{timestamp}"
        return Path(shutil.make_archive(str(zip_base), "zip", root_dir=str(output_dir)))
