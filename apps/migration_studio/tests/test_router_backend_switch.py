"""Router の実行バックエンド切替テスト."""

from __future__ import annotations

import json
import time
from pathlib import Path
from typing import Any

from apps.migration_studio.backend.app import create_app
from apps.migration_studio.backend.execution_adapter import ExecutionConfig, ExecutionResult
from fastapi.testclient import TestClient


def test_hitl_returns_501_in_cma_cli_backend(monkeypatch) -> None:
    """cma_cli バックエンドでは HITL API が未サポートである."""
    monkeypatch.setenv("MIGRATION_EXECUTION_BACKEND", "cma_cli")
    client = TestClient(create_app())

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


class _FakeCmaCliExecutionAdapter:
    """router 統合検証用のフェイク CMA Adapter."""

    def __init__(self, runtime_root: Path) -> None:
        self._runtime_root = runtime_root
        self._runtime_root.mkdir(parents=True, exist_ok=True)
        self._configs: dict[str, ExecutionConfig] = {}

    async def start(self, task_id: str, config: ExecutionConfig) -> None:
        self._configs[task_id] = config

    async def stream_events(self, task_id: str):
        _ = task_id
        yield {"type": "stage_start", "stage": "analyzer", "message": "start"}
        yield {"type": "stage_complete", "stage": "analyzer", "decision": "PASSED"}
        yield {
            "type": "complete",
            "stage": "pipeline",
            "program_name": "SAMPLE",
            "decision": "PASSED",
            "version": 1,
        }

    async def await_result(self, task_id: str) -> ExecutionResult:
        config = self._configs[task_id]
        output_dir = config.output_root / task_id
        output_dir.mkdir(parents=True, exist_ok=True)
        (output_dir / "report.md").write_text("# report", encoding="utf-8")
        return ExecutionResult(
            success=True,
            decision="PASSED",
            output_dir=output_dir,
            summary={
                "success": True,
                "decision": "PASSED",
                "output_dir": str(output_dir),
            },
            error=None,
        )

    @staticmethod
    def create_download_package(output_dir: Path, task_id: str) -> Path:
        _ = output_dir
        zip_path = Path(output_dir.parent) / f"{task_id}.zip"
        zip_path.write_bytes(b"zip-bytes")
        return zip_path


def test_upload_stream_status_download_with_cma_cli(
    monkeypatch,
    tmp_path: Path,
) -> None:
    """最小E2E: upload -> stream -> status -> download が成立する."""
    monkeypatch.setenv("MIGRATION_EXECUTION_BACKEND", "cma_cli")

    from apps.migration_studio.backend import router as router_module

    monkeypatch.setattr(router_module, "CmaCliExecutionAdapter", _FakeCmaCliExecutionAdapter)
    monkeypatch.setattr(router_module, "_get_output_root", lambda: tmp_path / "output")

    sample = Path(__file__).parent / "fixtures" / "sample.cbl"

    with TestClient(create_app()) as client:
        upload = client.post(
            "/api/migrate/upload",
            files={"file": ("sample.cbl", sample.read_bytes(), "text/plain")},
            params={"fast": "true", "model": "claude-opus-4-6"},
        )
        assert upload.status_code == 200
        body = upload.json()
        task_id = str(body["task_id"])

        # バックグラウンド完了を待機
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

        stream_resp = client.get(f"/api/migrate/{task_id}/stream")
        assert stream_resp.status_code == 200
        events: list[dict[str, Any]] = []
        for line in stream_resp.text.splitlines():
            if line.startswith("data: "):
                events.append(json.loads(line[6:]))

        types = [str(item.get("type")) for item in events]
        assert "stage_start" in types
        assert "stage_complete" in types
        assert "complete" in types

        download = client.get(f"/api/migrate/{task_id}/download")
        assert download.status_code == 200
        assert download.headers["content-type"].startswith("application/zip")
        assert download.content == b"zip-bytes"
