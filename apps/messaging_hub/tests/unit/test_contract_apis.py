"""Messaging Hub 契約 API テスト."""

from __future__ import annotations

import shutil
import uuid
from pathlib import Path
from typing import Any

import httpx
import pytest
import pytest_asyncio

from apps.messaging_hub import main
from apps.messaging_hub.approval_manager import ApprovalManager
from apps.messaging_hub.execution_tracker import ExecutionTracker
from apps.messaging_hub.skills_manager import SkillsManager
from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore
from kernel.skills import RiskLevel


@pytest_asyncio.fixture()
async def client_with_state(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> tuple[httpx.AsyncClient, dict[str, str]]:
    """テスト用アプリ状態を初期化する."""

    async def _noop_async(*_args: Any, **_kwargs: Any) -> None:
        return None

    monkeypatch.setenv("MESSAGING_HUB_API_KEY", "test-key")
    monkeypatch.setattr(main, "_store", SQLiteMessagingHubStore(db_path=tmp_path / "messaging_hub_test.db"))
    monkeypatch.setattr(main, "_approval_manager", ApprovalManager(websocket_hub=main.hub))
    monkeypatch.setattr(main, "_execution_tracker", ExecutionTracker(websocket_hub=main.hub))
    monkeypatch.setattr(main, "_skills_manager", SkillsManager(gateway=main._skill_gateway, websocket_hub=main.hub))
    monkeypatch.setattr(main, "_active_step_events", {})
    monkeypatch.setattr(main, "_run_started_at", {})
    monkeypatch.setattr(main, "_assistant_cli_proposals", {})
    monkeypatch.setattr(main, "setup_platforms", _noop_async)
    monkeypatch.setattr(main, "start_background_tasks", _noop_async)
    monkeypatch.setattr(main.gateway, "shutdown", _noop_async)
    main._approval_manager.on_request(main._on_approval_request)
    main._approval_manager.on_decision(main._on_approval_decision)

    headers = {"x-api-key": "test-key"}
    await main._store.initialize()
    transport = httpx.ASGITransport(app=main.app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as client:
        yield client, headers


def _fake_assistant_result() -> dict[str, Any]:
    """テスト用 assistant 応答."""
    return {
        "summary": "テスト応答です",
        "headline": "テスト",
        "key_points": ["k1"],
        "actions": ["a1"],
        "risks": [],
        "intent": {"category": "task_execution"},
        "run_id": f"run_{uuid.uuid4().hex}",
    }


def _fake_assistant_result_for_chat() -> dict[str, Any]:
    """chat.postMessage 用の応答（summary が汎用でも answer を優先させる）."""
    return {
        "summary": "✅ タスク完了: general\n\n📌 処理済み: 0",
        "headline": "タスク完了",
        "key_points": ["処理済み: 0"],
        "actions": [],
        "risks": [],
        "intent": {"category": "general", "template": "general"},
        "run_id": f"run_{uuid.uuid4().hex}",
        "raw_results": {
            "answer": "私は Messaging Hub のアシスタントです。質問に回答できます。",
            "processed": 0,
        },
    }


@pytest.mark.asyncio
async def test_sr_chat_api_and_backward_routes(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """sr_chat API と既存 API の非破壊互換を確認."""
    client, headers = client_with_state

    async def _fake_process(*args: Any, **kwargs: Any) -> dict[str, Any]:
        return _fake_assistant_result()

    monkeypatch.setattr(main.assistant, "process", _fake_process)

    health = await client.get("/health")
    assert health.status_code == 200

    api_health = await client.get("/api/health")
    assert api_health.status_code == 200
    api_health_json = api_health.json()
    assert api_health_json["auth_required"] is True
    assert api_health_json["auth_env_var"] == "MESSAGING_HUB_API_KEY"
    assert api_health_json["auth_key_configured"] is True

    platforms = await client.get("/platforms", headers=headers)
    assert platforms.status_code == 200

    assistant_process = await client.post(
        "/assistant/process",
        headers=headers,
        json={"message": "hello", "user_id": "u1"},
    )
    assert assistant_process.status_code == 200
    assistant_json = assistant_process.json()
    assert assistant_json["ok"] is True
    assert isinstance(assistant_json["run_id"], str)

    auth_test = await client.post("/api/sr_chat/auth.test", headers=headers)
    assert auth_test.status_code == 200
    assert auth_test.json()["ok"] is True

    posted = await client.post(
        "/api/sr_chat/chat.postMessage",
        headers=headers,
        json={"text": "hello", "user_id": "u1", "conversation_id": "chat:test"},
    )
    assert posted.status_code == 200
    posted_json = posted.json()
    assert posted_json["ok"] is True
    assert posted_json["conversation_id"] == "chat:test"
    assert isinstance(posted_json["run_id"], str)

    conversations = await client.get("/api/sr_chat/conversations.list", headers=headers)
    assert conversations.status_code == 200
    assert conversations.json()["total"] >= 1

    history = await client.get("/api/sr_chat/conversations.history?conversation_id=chat:test", headers=headers)
    assert history.status_code == 200
    assert history.json()["total"] >= 2

    export = await client.get(
        "/api/sr_chat/export?conversation_id=chat:test&format=json",
        headers=headers,
    )
    assert export.status_code == 200
    export_json = export.json()
    assert export_json["ok"] is True
    assert export_json["format"] == "json"
    assert export_json["total_messages"] >= 2
    assert export_json["filename"].endswith(".json")
    assert isinstance(export_json["data"], str)


@pytest.mark.asyncio
async def test_sr_chat_post_message_prefers_direct_answer_text(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """汎用 summary より raw_results.answer を優先して返すことを確認."""
    client, headers = client_with_state

    async def _fake_process(*args: Any, **kwargs: Any) -> dict[str, Any]:
        return _fake_assistant_result_for_chat()

    monkeypatch.setattr(main.assistant, "process", _fake_process)

    posted = await client.post(
        "/api/sr_chat/chat.postMessage",
        headers=headers,
        json={"text": "你是谁", "user_id": "u1", "conversation_id": "chat:test-reply"},
    )
    assert posted.status_code == 200
    payload = posted.json()
    assert payload["ok"] is True
    assert payload["message"]["text"] == "私は Messaging Hub のアシスタントです。質問に回答できます。"


@pytest.mark.asyncio
async def test_approval_transition_and_endpoints(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
) -> None:
    """承認要求の pending -> approved 遷移を確認."""
    client, headers = client_with_state

    request_id, auto = await main._approval_manager.request_approval(
        skill_name="write_file",
        params={"path": "a.txt"},
        risk_level=RiskLevel.HIGH,
        user_id="tester",
    )
    assert auto is False

    pending = await client.get("/api/approvals/pending", headers=headers)
    assert pending.status_code == 200
    pending_ids = {item["id"] for item in pending.json()["requests"]}
    assert request_id in pending_ids

    approved = await client.post(
        f"/api/approvals/{request_id}/approve",
        headers=headers,
        json={"approver_id": "admin"},
    )
    assert approved.status_code == 200
    assert approved.json()["ok"] is True

    history = await client.get("/api/approvals/history?limit=20", headers=headers)
    assert history.status_code == 200
    status_by_id = {item["id"]: item["status"] for item in history.json()["requests"]}
    assert status_by_id.get(request_id) == "approved"

    restored = SQLiteMessagingHubStore(db_path=main._store.db_path)
    restored_approvals = await restored.list_approvals(limit=50)
    assert any(item["id"] == request_id and item["status"] == "approved" for item in restored_approvals)


@pytest.mark.asyncio
async def test_execution_and_rollback_persistence(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
) -> None:
    """実行イベントと rollback_handle の永続化を確認."""
    client, headers = client_with_state
    run_id = f"run_{uuid.uuid4().hex}"
    step_id = f"step_{uuid.uuid4().hex}"

    await main._assistant_event_emitter(
        "RunStarted",
        {"run_id": run_id, "user_id": "tester", "message": "start"},
    )
    await main._assistant_event_emitter(
        "StepStarted",
        {
            "run_id": run_id,
            "step_id": step_id,
            "skill_name": "list_dir",
            "params": {"path": "."},
            "user_id": "tester",
        },
    )
    await main._assistant_event_emitter(
        "ToolExecuted",
        {
            "run_id": run_id,
            "step_id": step_id,
            "skill_name": "list_dir",
            "status": "success",
            "duration_ms": 12.5,
            "cost": {"duration_ms": 12.5, "token_estimate": 0},
            "risk_flags": [],
            "artifacts": [{"type": "file", "location": "tmp.txt"}],
            "rollback_handle": {"id": "rb_1", "kind": "none"},
        },
    )
    await main._assistant_event_emitter(
        "RunFinished",
        {"run_id": run_id, "user_id": "tester", "status": "completed"},
    )

    executions = await client.get("/api/executions?skill=list_dir", headers=headers)
    assert executions.status_code == 200
    assert executions.json()["total"] >= 1

    artifacts = await main._store.list_artifacts(run_id=run_id)
    artifact_types = {item["artifact_type"] for item in artifacts}
    assert "rollback_handle" in artifact_types

    store_path = main._store.db_path
    reopened = SQLiteMessagingHubStore(db_path=store_path)
    restored_runs = await reopened.list_run_records(limit=20)
    run_ids = {item["run_id"] for item in restored_runs}
    assert run_id in run_ids


@pytest.mark.asyncio
async def test_api_health_reports_missing_auth_key(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """api/health が認証キー未設定状態を返すこと."""
    client, _ = client_with_state
    monkeypatch.delenv("MESSAGING_HUB_API_KEY", raising=False)

    response = await client.get("/api/health")
    assert response.status_code == 200
    payload = response.json()
    assert payload["auth_required"] is True
    assert payload["auth_key_configured"] is False
    assert payload["status"] == "degraded"


@pytest.mark.asyncio
async def test_file_organizer_api_with_approval_flow(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
) -> None:
    """file-organizer API が dry_run と承認付き実行の両方を処理できること."""
    client, headers = client_with_state
    workspace_dir = Path.cwd() / "apps" / "messaging_hub" / "data" / f"test_file_organizer_{uuid.uuid4().hex}"
    workspace_dir.mkdir(parents=True, exist_ok=True)
    (workspace_dir / "doc.txt").write_text("hello", encoding="utf-8")
    (workspace_dir / "photo.jpg").write_bytes(b"\x00\x01")

    try:
        analyzed = await client.post(
            "/api/file-organizer/analyze",
            headers=headers,
            json={"path": str(workspace_dir), "recursive": True, "days_old": 30},
        )
        assert analyzed.status_code == 200
        analyzed_json = analyzed.json()
        assert analyzed_json["ok"] is True
        assert analyzed_json["total_files"] >= 2

        duplicates = await client.post(
            "/api/file-organizer/duplicates",
            headers=headers,
            json={"path": str(workspace_dir), "by_content": True},
        )
        assert duplicates.status_code == 200
        assert duplicates.json()["ok"] is True

        dry_run = await client.post(
            "/api/file-organizer/organize",
            headers=headers,
            json={"path": str(workspace_dir), "dry_run": True, "user_id": "u1"},
        )
        assert dry_run.status_code == 200
        dry_json = dry_run.json()
        assert dry_json["ok"] is True
        assert dry_json["dry_run"] is True
        assert dry_json["total_actions"] > 0

        requires = await client.post(
            "/api/file-organizer/organize",
            headers=headers,
            json={"path": str(workspace_dir), "dry_run": False, "user_id": "u1"},
        )
        assert requires.status_code == 200
        requires_json = requires.json()
        assert requires_json["requires_approval"] is True
        request_id = str(requires_json["request_id"])

        approved = await client.post(
            f"/api/approvals/{request_id}/approve",
            headers=headers,
            json={"approver_id": "admin"},
        )
        assert approved.status_code == 200
        assert approved.json()["ok"] is True

        executed = await client.post(
            "/api/file-organizer/organize",
            headers=headers,
            json={
                "path": str(workspace_dir),
                "dry_run": False,
                "user_id": "u1",
                "approval_request_id": request_id,
            },
        )
        assert executed.status_code == 200
        executed_json = executed.json()
        assert executed_json["ok"] is True
        assert executed_json["dry_run"] is False
        assert (workspace_dir / "Documents" / "doc.txt").exists()
        assert (workspace_dir / "Images" / "photo.jpg").exists()
    finally:
        shutil.rmtree(workspace_dir, ignore_errors=True)
