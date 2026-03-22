"""Migration router compatibility tests."""

from __future__ import annotations

import time
from typing import TYPE_CHECKING, Any

from apps.code_migration_assistant.backend import app as backend_app
from apps.code_migration_assistant.backend import migration_router as router_module
from fastapi.testclient import TestClient


if TYPE_CHECKING:
    from pathlib import Path


def _disable_auth(monkeypatch: Any) -> None:
    monkeypatch.setattr(backend_app._auth_guard, "is_auth_required", lambda: False)
    monkeypatch.setattr(backend_app._auth_guard, "should_protect_http_path", lambda _path: False)


def test_hitl_returns_501_in_cma_cli_backend(monkeypatch: Any) -> None:
    """cma_cli バックエンドでは HITL API が未サポートである."""
    _disable_auth(monkeypatch)
    monkeypatch.setenv("MIGRATION_EXECUTION_BACKEND", "cma_cli")

    with TestClient(backend_app.app) as client:
        response = client.post(
            "/api/migrate/task-123/hitl",
            json={
                "request_id": "req-1",
                "approved": True,
                "comment": "ok",
                "modifications": {},
            },
        )
    assert response.status_code == 501
    assert "未サポート" in response.json()["detail"]


def test_upload_uses_legacy_backend_when_configured(monkeypatch: Any, tmp_path: Path) -> None:
    """legacy_engine 指定時に legacy 経路が呼び出される."""
    _disable_auth(monkeypatch)
    monkeypatch.setenv("MIGRATION_EXECUTION_BACKEND", "legacy_engine")

    store = router_module.get_task_store()
    store._tasks.clear()  # type: ignore[attr-defined]
    router_module._background_tasks.clear()

    called: dict[str, bool] = {"legacy": False}

    async def _fake_run_legacy_pipeline(
        *,
        task_id: str,
        cobol_files: list[Any],
        output_root: Path,
        fast_mode: bool,
        model: str,
        store: Any,
    ) -> None:
        del cobol_files, fast_mode, model
        called["legacy"] = True
        await store.push_event(task_id, {"type": "stage_start", "stage": "analyzer", "message": "start"})
        await store.push_event(task_id, {"type": "complete", "stage": "pipeline", "decision": "PASSED"})
        zip_path = output_root / f"{task_id}.zip"
        zip_path.parent.mkdir(parents=True, exist_ok=True)
        zip_path.write_bytes(b"zip-bytes")
        await store.set_download_path(task_id, zip_path)

    monkeypatch.setattr(router_module, "_run_legacy_pipeline", _fake_run_legacy_pipeline)
    monkeypatch.setattr(router_module, "_get_output_root", lambda: tmp_path / "output")

    with TestClient(backend_app.app) as client:
        upload = client.post(
            "/api/migrate/upload",
            files={
                "file": ("sample.cbl", b"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n", "text/plain")
            },
            params={"fast": "true", "model": "claude-opus-4-6"},
        )
        assert upload.status_code == 200
        task_id = str(upload.json()["task_id"])

        status_payload: dict[str, Any] = {}
        for _ in range(40):
            status_resp = client.get(f"/api/migrate/{task_id}/status")
            assert status_resp.status_code == 200
            status_payload = status_resp.json()
            if status_payload.get("status") == "complete":
                break
            time.sleep(0.05)

        assert status_payload.get("status") == "complete"
        assert bool(status_payload.get("download_available")) is True
        assert called["legacy"] is True
