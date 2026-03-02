"""ExecutionAdapter テスト."""

from __future__ import annotations

import asyncio
import json
from pathlib import Path
from typing import Any

import pytest
from apps.migration_studio.backend.execution_adapter import CmaCliExecutionAdapter, ExecutionConfig


class _FakeProcess:
    """テスト用 subprocess.Process 互換."""

    def __init__(self) -> None:
        self.returncode: int | None = None
        self._completed = asyncio.Event()

    def mark_done(self, code: int) -> None:
        self.returncode = code
        self._completed.set()

    async def communicate(self) -> tuple[bytes, bytes]:
        await self._completed.wait()
        return b"", b""


@pytest.mark.asyncio
async def test_cma_cli_adapter_streams_events_and_result(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    """NDJSONイベントを中継し、結果JSONを読み取れる."""
    fake_process = _FakeProcess()

    async def _fake_create_subprocess_exec(*cmd: str, **kwargs: Any) -> _FakeProcess:
        _ = kwargs
        output_path = Path(cmd[cmd.index("--output") + 1])
        events_path = Path(cmd[cmd.index("--events") + 1])

        async def _writer() -> None:
            await asyncio.sleep(0.05)
            events_path.write_text(
                "\n".join(
                    [
                        json.dumps({"type": "stage_start", "stage": "analyzer"}),
                        json.dumps({"type": "complete", "stage": "pipeline", "decision": "PASSED"}),
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            await asyncio.sleep(0.05)
            output_path.write_text(
                json.dumps(
                    {
                        "success": True,
                        "decision": "PASSED",
                        "output_dir": str(tmp_path / "output" / "task-1"),
                        "error": None,
                    }
                ),
                encoding="utf-8",
            )
            fake_process.mark_done(0)

        asyncio.create_task(_writer())
        return fake_process

    monkeypatch.setattr(asyncio, "create_subprocess_exec", _fake_create_subprocess_exec)

    adapter = CmaCliExecutionAdapter(tmp_path / "runtime")
    await adapter.start(
        "task-1",
        ExecutionConfig(
            source_path=tmp_path / "sample.cbl",
            output_root=tmp_path / "output",
            fast_mode=True,
            model="claude-opus-4-6",
            options={},
        ),
    )

    events = [event async for event in adapter.stream_events("task-1")]
    result = await adapter.await_result("task-1")

    assert events[0]["type"] == "stage_start"
    assert events[-1]["type"] == "complete"
    assert result.success is True
    assert result.decision == "PASSED"


@pytest.mark.asyncio
async def test_cma_cli_adapter_invalid_ndjson_emits_error(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    """不正NDJSON行を検出した場合、errorイベントを返す."""
    fake_process = _FakeProcess()

    async def _fake_create_subprocess_exec(*cmd: str, **kwargs: Any) -> _FakeProcess:
        _ = kwargs
        output_path = Path(cmd[cmd.index("--output") + 1])
        events_path = Path(cmd[cmd.index("--events") + 1])

        async def _writer() -> None:
            await asyncio.sleep(0.05)
            events_path.write_text(
                "\n".join(
                    [
                        "{invalid json}",
                        json.dumps({"type": "stage_start", "stage": "analyzer"}),
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            await asyncio.sleep(0.05)
            output_path.write_text(
                json.dumps(
                    {
                        "success": False,
                        "decision": "ENV_ISSUE",
                        "output_dir": str(tmp_path / "output" / "task-2"),
                        "error": "invalid event",
                    }
                ),
                encoding="utf-8",
            )
            fake_process.mark_done(2)

        asyncio.create_task(_writer())
        return fake_process

    monkeypatch.setattr(asyncio, "create_subprocess_exec", _fake_create_subprocess_exec)

    adapter = CmaCliExecutionAdapter(tmp_path / "runtime")
    await adapter.start(
        "task-2",
        ExecutionConfig(
            source_path=tmp_path / "sample.cbl",
            output_root=tmp_path / "output",
            fast_mode=True,
            model="claude-opus-4-6",
            options={},
        ),
    )

    events = [event async for event in adapter.stream_events("task-2")]
    assert any(event.get("type") == "error" for event in events)
