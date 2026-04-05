"""Messaging Hub 契約 API テスト."""

from __future__ import annotations

import shutil
import uuid
from datetime import datetime
from pathlib import Path
from typing import Any

import httpx
import pytest
import pytest_asyncio

from apps.messaging_hub import main
from apps.messaging_hub.agents.business_advisor_agent import BusinessAdvisorOutput
from apps.messaging_hub.approval_manager import ApprovalManager
from apps.messaging_hub.execution_tracker import ExecutionTracker
from apps.messaging_hub.flight_watch import FlightOffer, FlightSearchResult, RankingWeights
from apps.messaging_hub.skills_manager import SkillsManager, WorkflowRunResult
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
    monkeypatch.setattr(main, "_platform_runtime_tasks", {})
    monkeypatch.setattr(main, "background_tasks", [])
    monkeypatch.setattr(main, "setup_platforms", _noop_async)
    monkeypatch.setattr(main, "start_background_tasks", _noop_async)
    monkeypatch.setattr(main.gateway, "shutdown", _noop_async)
    main._approval_manager.on_request(main._on_approval_request)
    main._approval_manager.on_decision(main._on_approval_decision)
    main._initialize_orchestration_services()

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

    tool_contracts = await client.get(
        "/api/tools/contracts?template_name=report&format=markdown",
        headers=headers,
    )
    assert tool_contracts.status_code == 200
    contracts_payload = tool_contracts.json()
    assert contracts_payload["total"] == 2
    assert contracts_payload["contracts"][0]["action_name"] == "create_report_draft"
    assert contracts_payload["contracts"][1]["action_name"] == "persist_report_draft"

    monitoring = await client.get("/api/orchestration/monitoring/summary", headers=headers)
    assert monitoring.status_code == 200
    monitoring_payload = monitoring.json()["summary"]
    assert monitoring_payload["total_sessions"] >= 1
    assert "by_profile" in monitoring_payload
    assert "unsafe_action_rate" in monitoring_payload

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
    assert isinstance(posted_json["execution_session_id"], str)
    assert posted_json["execution_session_id"].startswith("exec_")
    assert posted_json["execution_summary"]["feedback_count"] >= 1

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
async def test_assistant_process_includes_main_agent_execution_context(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """general path で main agent execution context を渡すこと."""
    client, headers = client_with_state
    captured: dict[str, Any] = {}

    async def _fake_process(message: str, *, user_id: str, context: dict[str, Any]) -> dict[str, Any]:
        captured["message"] = message
        captured["user_id"] = user_id
        captured["context"] = context
        return _fake_assistant_result()

    monkeypatch.setattr(main.assistant, "process", _fake_process)

    response = await client.post(
        "/assistant/process",
        headers=headers,
        json={"message": "Summarize my unread messages", "user_id": "u1"},
    )
    assert response.status_code == 200
    execution_context = captured["context"]["execution_context"]["main_agent"]
    assert execution_context["current_message"] == "Summarize my unread messages"
    assert execution_context["task_goal"] == "Summarize my unread messages"
    assert "policy_summary" in execution_context
    assert "open_actions" in execution_context


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


@pytest.mark.asyncio
async def test_orchestration_requires_clarification_and_resumes(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
) -> None:
    """機票条件不足時に clarification を返し、回答後に同一 task を再開すること."""
    client, headers = client_with_state

    created = await client.post(
        "/api/orchestration/tasks",
        headers=headers,
        json={
            "message": "请帮我监视便宜机票",
            "user_id": "u1",
            "conversation_id": "chat:flight",
            "required_capability": "flight_watch",
        },
    )
    assert created.status_code == 200
    created_json = created.json()
    assert created_json["status"] == "clarification_required"
    assert isinstance(created_json["task_id"], str)
    assert isinstance(created_json["clarification_ticket_id"], str)
    assert len(created_json["clarification_questions"]) >= 4
    assert created_json["execution_session_id"].startswith("exec_")
    assert created_json["execution_summary"]["checkpoint_count"] >= 1

    task_id = str(created_json["task_id"])
    resumed = await client.post(
        f"/api/orchestration/tasks/{task_id}/clarifications",
        headers=headers,
        json={
            "answers": {
                "origin": "HND",
                "destination": "LAX",
                "depart_window": "2026-05-01..2026-05-03",
                "return_window": "2026-05-10..2026-05-12",
                "create_watch": "yes",
            }
        },
    )
    assert resumed.status_code == 200
    resumed_json = resumed.json()
    assert resumed_json["status"] == "monitoring"
    assert resumed_json["result"]["search_result"]["offers"]
    assert resumed_json["result"]["subscription"]["subscription_id"].startswith("fws_")
    assert resumed_json["execution_session_id"].startswith("exec_")
    assert resumed_json["execution_summary"]["checkpoint_count"] >= 3

    task_detail = await client.get(f"/api/orchestration/tasks/{task_id}", headers=headers)
    assert task_detail.status_code == 200
    task_payload = task_detail.json()["task"]
    assert task_payload["status"] == "monitoring"
    assert task_payload["context"]["harness_plan"]["blueprint_id"] == "structured_monitoring.flight_watch"
    assert task_payload["context"]["harness_plan"]["provider_strategy"]["mode"] == "discover_first"
    assert "user_preferences" in task_payload["context"]["harness_plan"]["memory_plan"]["main_agent_keys"]
    assert task_payload["execution_session_id"].startswith("exec_")
    assert task_payload["execution_summary"]["action_count"] >= 2

    events = await client.get(f"/api/orchestration/tasks/{task_id}/events", headers=headers)
    assert events.status_code == 200
    event_types = {item["event_type"] for item in events.json()["events"]}
    assert "clarification.required" in event_types
    assert "flow.complete" in event_types

    execution = await client.get(f"/api/orchestration/tasks/{task_id}/execution", headers=headers)
    assert execution.status_code == 200
    execution_json = execution.json()["execution"]
    assert execution_json["execution_session"]["session_id"].startswith("exec_")
    assert execution_json["latest_checkpoint"]["stage"] in {"after_verification", "pre_mutation_subscription"}
    decision_steps = {item["decision"]["step"] for item in execution_json["decisions"]}
    assert "clarification_required" in decision_steps
    assert execution_json["feedback_summary"]["by_source"]["human"] >= 1

    await main._store.upsert_execution_event(
        {
            "id": f"evt_{uuid.uuid4().hex}",
            "run_id": task_id,
            "skill_name": "flight_watch.search",
            "params": {"origin": "HND", "destination": "LAX"},
            "status": "success",
            "started_at": "2026-05-01T00:00:00+00:00",
            "completed_at": "2026-05-01T00:00:01+00:00",
            "result": {"offers_found": 1},
            "user_id": "u1",
            "metadata": {"step_id": "flight_search"},
        }
    )

    replay = await client.get(f"/api/orchestration/tasks/{task_id}/replay", headers=headers)
    assert replay.status_code == 200
    replay_timeline = replay.json()["replay"]["timeline"]
    replay_kinds = {item["kind"] for item in replay_timeline}
    assert {"action_log", "decision", "checkpoint", "feedback", "agui_event", "execution_event"}.issubset(replay_kinds)


@pytest.mark.asyncio
async def test_orchestration_routes_existing_agent_and_gap_fills(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """既存 agent routing と gap fill artifact 生成の両方を確認する."""
    client, headers = client_with_state

    async def _fake_business_process(input_data: Any) -> BusinessAdvisorOutput:
        question = getattr(input_data, "question", "")
        return BusinessAdvisorOutput(
            advice=[{"title": "pricing", "content": "Focus on value-based pricing."}],
            selected_skills=["biz_pricing"],
            summary=f"Business advice for: {question}",
        )

    monkeypatch.setattr(main._business_advisor_agent, "process", _fake_business_process)

    routed = await client.post(
        "/api/orchestration/tasks",
        headers=headers,
        json={
            "message": "Help me decide SaaS pricing",
            "user_id": "u1",
            "required_capability": "business_advice",
        },
    )
    assert routed.status_code == 200
    routed_json = routed.json()
    assert routed_json["status"] == "completed"
    assert routed_json["result"]["agent"] == "BusinessAdvisorAgent"
    assert routed_json["result"]["harness_plan"]["workers"][0]["role"] == "main_agent"
    assert routed_json["execution_session_id"].startswith("exec_")

    gap_fill = await client.post(
        "/api/orchestration/tasks",
        headers=headers,
        json={
            "message": "Create a temporary specialist for travel reimbursement policy checks",
            "user_id": "u1",
            "required_capability": "travel_policy_review",
        },
    )
    assert gap_fill.status_code == 200
    gap_fill_json = gap_fill.json()
    assert gap_fill_json["generated_artifact"] is not None
    assert gap_fill_json["generated_artifact"]["status"] == "runtime_active"
    runtime_agent_name = gap_fill_json["result"]["runtime_agent"]
    assert isinstance(runtime_agent_name, str)
    assert gap_fill_json["execution_summary"]["artifact_refs"] == [gap_fill_json["generated_artifact"]["artifact_id"]]

    gap_fill_execution = await client.get(
        f"/api/orchestration/tasks/{gap_fill_json['task_id']}/execution",
        headers=headers,
    )
    assert gap_fill_execution.status_code == 200
    gap_fill_execution_json = gap_fill_execution.json()["execution"]
    assert gap_fill_execution_json["action_log_summary"]["by_type"]["create_runtime_artifact"] == 1

    async def _fake_run_workflow(
        workflow_id: str,
        params: dict[str, Any] | None = None,
        dry_run: bool = False,
    ) -> WorkflowRunResult:
        return WorkflowRunResult(
            workflow_id=workflow_id,
            run_id=f"run_{uuid.uuid4().hex}",
            status="success",
            step_results=[
                {
                    "step_id": "step_1",
                    "skill_name": "web_search",
                    "status": "success",
                    "result": {"query": params.get("task_description", "") if params else ""},
                    "dry_run": dry_run,
                }
            ],
            started_at=datetime.now(),
            completed_at=datetime.now(),
            error=None,
        )

    monkeypatch.setattr(main._skills_manager, "run_workflow", _fake_run_workflow)

    routed_runtime = await client.post(
        "/api/orchestration/tasks",
        headers=headers,
        json={
            "message": "Review our travel reimbursement policy exceptions",
            "user_id": "u1",
            "required_capability": "travel_policy_review",
        },
    )
    assert routed_runtime.status_code == 200
    routed_runtime_json = routed_runtime.json()
    assert routed_runtime_json["status"] == "completed"
    assert routed_runtime_json["result"]["agent"] == runtime_agent_name
    assert (
        routed_runtime_json["result"]["output"]["runtime_artifact_id"]
        == gap_fill_json["generated_artifact"]["artifact_id"]
    )
    assert routed_runtime_json["execution_summary"]["decision_count"] >= 1


@pytest.mark.asyncio
async def test_capability_route_includes_specialist_and_verifier_execution_context(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """capability route で specialist/verifier context が残ること."""
    client, headers = client_with_state
    captured_payloads: list[dict[str, Any]] = []

    async def _fake_a2a_call(agent_name: str, payload: dict[str, Any]) -> dict[str, Any]:
        captured_payloads.append({"agent_name": agent_name, "payload": payload})
        return {"summary": f"handled by {agent_name}", "answer": "ok"}

    monkeypatch.setattr(
        main._orchestration_service._capability_router,
        "select_best_candidate",
        lambda _required_capability: {"name": "BusinessAdvisorAgent", "app_name": "messaging_hub"},
    )
    monkeypatch.setattr(main._orchestration_service._a2a_hub, "call", _fake_a2a_call)

    routed = await client.post(
        "/api/orchestration/tasks",
        headers=headers,
        json={
            "message": "Help me decide SaaS pricing",
            "user_id": "u1",
            "required_capability": "business_advice",
        },
    )
    assert routed.status_code == 200
    routed_json = routed.json()
    assert captured_payloads
    specialist = captured_payloads[0]["payload"]["execution_context"]["specialist"]
    assert specialist["required_capability"] == "business_advice"
    assert isinstance(specialist["allowed_tools"], list)

    replay = await client.get(
        f"/api/orchestration/tasks/{routed_json['task_id']}/replay",
        headers=headers,
    )
    assert replay.status_code == 200
    verifier_entries = [
        item
        for item in replay.json()["replay"]["timeline"]
        if item["kind"] == "feedback" and item["payload"]["title"] == "capability_route_verification"
    ]
    assert verifier_entries
    verifier_context = verifier_entries[0]["payload"]["metadata"]["verifier_context"]["verifier"]
    assert verifier_context["goal"] == "Help me decide SaaS pricing"
    assert verifier_context["verification_profile"] == "basic"


@pytest.mark.asyncio
async def test_generated_artifact_lifecycle_and_skill_generate_endpoint(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
) -> None:
    """生成物 lifecycle が approve 前 publish 不可であること."""
    client, headers = client_with_state

    generated = await client.post(
        "/api/skills/generate",
        headers=headers,
        json={
            "description": "Watch flight prices and notify users by email when a new low is found.",
            "examples": ["Monitor HND to LAX fares"],
            "requested_by": "u1",
        },
    )
    assert generated.status_code == 200
    generated_json = generated.json()
    artifact = generated_json["artifact"]
    artifact_id = str(artifact["artifact_id"])
    assert artifact["status"] == "runtime_active"

    pre_promote = await client.post(
        f"/api/generated-artifacts/{artifact_id}/promote",
        headers=headers,
    )
    assert pre_promote.status_code == 200
    assert pre_promote.json()["artifact"]["status"] == "runtime_active"

    approved = await client.post(
        f"/api/generated-artifacts/{artifact_id}/approve",
        headers=headers,
        json={"approver_id": "reviewer"},
    )
    assert approved.status_code == 200
    assert approved.json()["artifact"]["status"] == "approved"

    promoted = await client.post(
        f"/api/generated-artifacts/{artifact_id}/promote",
        headers=headers,
    )
    assert promoted.status_code == 200
    assert promoted.json()["artifact"]["status"] == "published"


@pytest.mark.asyncio
async def test_flight_watch_notification_deduplicates_same_price(
    client_with_state: tuple[httpx.AsyncClient, dict[str, str]],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """新しい最安値のみ通知し、同一価格では再通知しないこと."""
    client, headers = client_with_state

    created = await client.post(
        "/api/flight-watch/subscriptions",
        headers=headers,
        json={
            "user_id": "u1",
            "conversation_id": "chat:watch",
            "request": {
                "origin": "HND",
                "destination": "SFO",
                "depart_window": {"start_date": "2026-06-01", "end_date": "2026-06-03"},
                "return_window": {"start_date": "2026-06-10", "end_date": "2026-06-12"},
                "provider": "fake",
                "poll_interval_hours": 1,
            },
        },
    )
    assert created.status_code == 200
    subscription = created.json()["subscription"]
    subscription_id = str(subscription["subscription_id"])

    async def _discounted_search(_request: Any) -> FlightSearchResult:
        offer = FlightOffer(
            offer_id="drop_offer",
            provider="fake",
            origin="HND",
            destination="SFO",
            depart_date="2026-06-01",
            return_date="2026-06-10",
            price=99.0,
            currency="USD",
            total_duration_minutes=640,
            stops=0,
            carrier="SkyJet",
            red_eye=False,
            airport_change=False,
            layover_minutes=[],
        )
        return FlightSearchResult(
            offers=[offer],
            ranking_weights=RankingWeights(),
            provider_used="fake",
            recommended_offer=offer,
        )

    monkeypatch.setattr(main._flight_watch_service, "search", _discounted_search)

    stored = await main._store.get_flight_watch_subscription(subscription_id)
    assert stored is not None
    stored["next_check_at"] = "2000-01-01T00:00:00+00:00"
    await main._store.upsert_flight_watch_subscription(stored)

    first_tick = await main._flight_watch_service.check_due_subscriptions()
    assert first_tick["notified"] == 1

    deliveries = await main._store.list_notification_deliveries(subscription_id=subscription_id)
    assert len(deliveries) == 1

    stored_again = await main._store.get_flight_watch_subscription(subscription_id)
    assert stored_again is not None
    stored_again["next_check_at"] = "2000-01-01T00:00:00+00:00"
    await main._store.upsert_flight_watch_subscription(stored_again)

    second_tick = await main._flight_watch_service.check_due_subscriptions()
    assert second_tick["notified"] == 0

    deliveries_after = await main._store.list_notification_deliveries(subscription_id=subscription_id)
    assert len(deliveries_after) == 1
