"""Migration router compatibility tests."""

from __future__ import annotations

import asyncio
import time
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.backend import migration_router as router_module
from apps.code_migration_assistant.backend.migration_task_store import HITLRequest


def _disable_auth(monkeypatch: Any) -> None:
    del monkeypatch


def test_hitl_writes_bridge_and_resumes(monkeypatch: Any, tmp_path: Path) -> None:
    """HITL 応答は bridge file に永続化され、pending request を再開する."""
    _disable_auth(monkeypatch)
    monkeypatch.setattr(router_module, "_get_output_root", lambda: tmp_path / "output")

    store = router_module.get_task_store()
    store._tasks.clear()

    task = asyncio.run(store.create())
    task.pending_hitl = HITLRequest(
        request_id="req-1",
        stage="design",
        artifact={},
        unknowns=["missing rule"],
        question="confirm",
    )

    response = asyncio.run(
        router_module.submit_hitl(
            task.task_id,
            router_module.HITLSubmitRequest(
                request_id="req-1",
                approved=True,
                comment="ok",
                modifications={},
            ),
            store,
        )
    )
    assert response["status"] == "resumed"
    bridge_path = tmp_path / "output" / "_runtime" / task.task_id / "approvals" / "req-1.json"
    assert bridge_path.exists()


def test_upload_uses_engine_runtime_when_configured(monkeypatch: Any, tmp_path: Path) -> None:
    """upload は direct engine runtime を呼び出す."""
    _disable_auth(monkeypatch)

    store = router_module.get_task_store()
    store._tasks.clear()
    router_module._background_tasks.clear()

    called: dict[str, bool] = {"engine": False}

    async def _fake_run_engine_pipeline(
        *,
        task_id: str,
        cobol_files: list[Any],
        tmp_dir: Path,
        output_root: Path,
        fast_mode: bool,
        model: str,
        store: Any,
    ) -> None:
        del cobol_files, tmp_dir, fast_mode, model
        called["engine"] = True
        await store.push_event(task_id, {"type": "stage_start", "stage": "analyzer", "message": "start"})
        await store.push_event(task_id, {"type": "complete", "stage": "pipeline", "decision": "PASSED"})
        zip_path = output_root / f"{task_id}.zip"
        zip_path.parent.mkdir(parents=True, exist_ok=True)
        zip_path.write_bytes(b"zip-bytes")
        await store.set_download_path(task_id, zip_path)
        await store.update_status(task_id, router_module.TaskStatus.COMPLETE)

    monkeypatch.setattr(router_module, "_run_engine_pipeline", _fake_run_engine_pipeline)
    monkeypatch.setattr(router_module, "_get_output_root", lambda: tmp_path / "output")

    class _UploadFile:
        filename = "sample.cbl"

        async def read(self) -> bytes:
            return b"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n"

    upload = asyncio.run(
        router_module.upload_cobol(
            file=_UploadFile(),
            fast=True,
            model="claude-opus-4-6",
            store=store,
        )
    )
    task_id = str(upload.task_id)

    status_payload: dict[str, Any] = {}
    for _ in range(40):
        status_payload = asyncio.run(router_module.get_status(task_id, store)).model_dump(mode="json")
        if status_payload.get("status") == "complete":
            break
        time.sleep(0.05)

    assert status_payload.get("status") == "complete"
    assert bool(status_payload.get("download_available")) is True
    assert called["engine"] is True
