"""Code Migration backend command and event protocol tests."""

from __future__ import annotations

import asyncio
import contextlib
import json
from pathlib import Path
from typing import Any

import pytest
from fastapi import BackgroundTasks
from fastapi import HTTPException
from starlette.requests import Request

from agentflow.hitl.types import ApprovalRequest
from agentflow.integrations.context_bridge import FlowContext, SourceSystemType
from apps.code_migration_assistant.backend import app as backend_app
from apps.code_migration_assistant.engine import CodeMigrationEngine


def _make_runtime(task_id: str, artifacts_dir: Path) -> backend_app.TaskRuntime:
    flow_context = FlowContext(
        session_id=f"session-{task_id}",
        user_id="tester",
        tenant_id="tenant-test",
        source_system="pytest",
        source_system_type=SourceSystemType.API,
        user_context={
            "role": "manager",
            "permissions": ["read", "write", "execute", "manage"],
        },
    )
    runtime = backend_app.TaskRuntime(
        task_id=task_id,
        engine=CodeMigrationEngine(),
        inputs={"source_code": "IDENTIFICATION DIVISION."},
        flow_context=flow_context,
        artifacts_dir=artifacts_dir,
    )
    backend_app.active_tasks[task_id] = runtime
    return runtime


def test_human_command_api_transitions_and_audit(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    task_id = "task-cmd"
    runtime = _make_runtime(task_id, tmp_path / task_id)

    approval_1 = ApprovalRequest(action="design_review", reason="approval required")
    runtime.engine._approval_flow._pending[approval_1.id] = approval_1

    approval_2 = ApprovalRequest(action="release_gate", reason="approval required")
    runtime.engine._approval_flow._pending[approval_2.id] = approval_2

    approve_resp = asyncio.run(
        backend_app.post_migration_command(
            task_id,
            backend_app.TaskCommandRequest(
                command="approve",
                request_id=approval_1.id,
                actor="architect",
                comment="looks good",
            ),
        )
    )
    assert approve_resp["status"] == "submitted"
    assert approve_resp["approved"] is True
    assert approve_resp["mode"] == "observer"
    assert approve_resp["applied"] is True
    assert approve_resp["effect_scope"] == "approval_flow"

    reject_resp = asyncio.run(
        backend_app.post_migration_command(
            task_id,
            backend_app.TaskCommandRequest(
                command="reject",
                request_id=approval_2.id,
                actor="reviewer",
                comment="needs redesign",
            ),
        )
    )
    assert reject_resp["status"] == "submitted"
    assert reject_resp["approved"] is False

    fact_payload = asyncio.run(
        backend_app.post_migration_command(
            task_id,
            backend_app.TaskCommandRequest(
                command="provide_business_fact",
                actor="analyst",
                fact={
                    "kind": "event",
                    "name": "受注確定",
                    "trigger": "CONFIRM",
                    "event_type": "business",
                },
            ),
        )
    )
    assert fact_payload["status"] == "accepted"
    assert fact_payload["feedback"]["command"] == "provide_business_fact"
    assert fact_payload["mode"] == "observer"
    assert fact_payload["applied"] is True
    assert fact_payload["effect_scope"] == "business_context"

    replan_payload = asyncio.run(
        backend_app.post_migration_command(
            task_id,
            backend_app.TaskCommandRequest(
                command="request_replan",
                actor="analyst",
                comment="add settlement event",
            ),
        )
    )
    assert replan_payload["status"] == "observed"
    assert replan_payload["mode"] == "observer"
    assert replan_payload["applied"] is False
    assert replan_payload["effect_scope"] == "planning"

    feedback_artifact_path = Path(fact_payload["feedback_artifact_path"])
    assert feedback_artifact_path.exists()

    assert len(runtime.command_history) == 4
    assert runtime.engine._human_facts
    assert runtime.observation_events_count == 4

    log_file = runtime.artifacts_dir / "logs" / "commands.jsonl"
    assert log_file.exists()
    lines = [json.loads(line) for line in log_file.read_text(encoding="utf-8").splitlines() if line]
    assert len(lines) == 4


def test_human_command_api_is_concurrency_safe(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    task_id = "task-cmd-concurrency"
    runtime = _make_runtime(task_id, tmp_path / task_id)

    async def _submit_fact(index: int) -> None:
        await backend_app.post_migration_command(
            task_id,
            backend_app.TaskCommandRequest(
                command="provide_business_fact",
                actor=f"analyst-{index}",
                fact={"kind": "rule", "name": f"rule-{index}"},
            ),
        )

    async def _run_concurrent_commands() -> None:
        await asyncio.gather(*[_submit_fact(index) for index in range(10)])

    asyncio.run(_run_concurrent_commands())

    assert len(runtime.command_history) == 10
    assert runtime.observation_events_count == 10


def test_command_result_event_is_streamed_without_polling(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    backend_app.task_websockets.clear()
    task_id = "task-command-event"
    _make_runtime(task_id, tmp_path / task_id)
    ws = _WsSpy()
    backend_app.task_websockets[task_id] = {ws}  # type: ignore[arg-type]

    try:
        asyncio.run(
            backend_app.post_migration_command(
                task_id,
                backend_app.TaskCommandRequest(
                    command="provide_business_fact",
                    actor="analyst",
                    fact={"kind": "rule", "name": "discount-threshold"},
                ),
            )
        )
    finally:
        backend_app.task_websockets.clear()

    event_types = [event.get("event_type") for event in ws.events]
    assert "command_result" in event_types
    command_event = next(event for event in ws.events if event.get("event_type") == "command_result")
    assert command_event["command"] == "provide_business_fact"
    assert command_event["status"] == "accepted"
    assert command_event["flow_id"] == task_id


def test_approval_command_streams_approval_and_command_events(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    backend_app.task_websockets.clear()
    task_id = "task-approval-event"
    runtime = _make_runtime(task_id, tmp_path / task_id)
    ws = _WsSpy()
    backend_app.task_websockets[task_id] = {ws}  # type: ignore[arg-type]

    pending = ApprovalRequest(action="design_review", reason="approval required")
    runtime.engine._approval_flow._pending[pending.id] = pending

    try:
        asyncio.run(
            backend_app.post_migration_command(
                task_id,
                backend_app.TaskCommandRequest(
                    command="approve",
                    request_id=pending.id,
                    actor="architect",
                    comment="approved",
                ),
            )
        )
    finally:
        backend_app.task_websockets.clear()

    event_types = [event.get("event_type") for event in ws.events]
    assert "command_result" in event_types
    assert "approval_submitted" in event_types


def test_event_protocol_is_always_agui_shaped() -> None:
    task_id = "task-event"

    node_start = backend_app._normalize_stream_event(
        task_id,
        {"event": "node_start", "node": "migration.analyze_code"},
    )
    assert node_start["event_type"] == "node.start"
    assert node_start["flow_id"] == task_id
    assert isinstance(node_start["timestamp"], float)
    assert node_start["contract_version"] == backend_app.CONTRACT_VERSION

    approval = backend_app._normalize_stream_event(
        task_id,
        {
            "event_type": "approval_submitted",
            "request_id": "req-1",
            "approved": True,
        },
    )
    assert approval["event_type"] == "approval_submitted"
    assert approval["flow_id"] == task_id
    assert isinstance(approval["timestamp"], float)
    assert approval["contract_version"] == backend_app.CONTRACT_VERSION

    supported_event_types = {
        "node.start",
        "node.complete",
        "flow.start",
        "flow.complete",
        "flow.error",
        "log",
        "approval_required",
        "approval_submitted",
        "approval_timeout",
        "command_result",
    }
    assert node_start["event_type"] in supported_event_types
    assert approval["event_type"] in supported_event_types


class _WsSpy:
    def __init__(self) -> None:
        self.events: list[dict[str, Any]] = []

    async def send_json(self, event: dict[str, Any]) -> None:
        self.events.append(event)


class _StubDistributedStore:
    def __init__(self, state: dict[str, Any] | None = None) -> None:
        self._state = state
        self.instance_id = "local-process"

    async def get_state(self, _task_id: str) -> dict[str, Any] | None:
        return self._state

    async def subscribe_events(self):
        if False:
            yield {}


class _OneShotDistributedStore(_StubDistributedStore):
    def __init__(self, payload: dict[str, Any]) -> None:
        super().__init__(None)
        self._payload = payload

    async def subscribe_events(self):
        yield self._payload


class _CommandBusDistributedStore(_StubDistributedStore):
    def __init__(self, state: dict[str, Any] | None = None) -> None:
        super().__init__(state)
        self.instance_id = "requester-process"
        self._results: dict[str, dict[str, Any]] = {}
        self.published_events: list[dict[str, Any]] = []

    async def publish_command(self, payload: dict[str, Any]) -> None:
        command_id = str(payload.get("command_id", ""))
        command = payload.get("command", {})
        command_name = str(command.get("command", "unknown"))
        self._results[command_id] = {
            "task_id": payload.get("task_id"),
            "command_id": command_id,
            "requester_instance_id": payload.get("requester_instance_id", self.instance_id),
            "responder_instance_id": "owner-process",
            "ok": True,
            "status_code": 200,
            "detail": None,
            "payload": {
                "status": "accepted",
                "task_id": payload.get("task_id"),
                "command": command_name,
                "mode": "observer",
                "applied": True,
                "effect_scope": "business_context",
            },
        }

    async def pop_command_result(self, command_id: str) -> dict[str, Any] | None:
        return self._results.pop(command_id, None)

    async def set_command_result(self, command_id: str, result: dict[str, Any]) -> None:
        self._results[command_id] = result

    async def subscribe_commands(self):
        if False:
            yield {}

    async def append_event(self, _task_id: str, _event: dict[str, Any]) -> None:
        return None

    async def publish_event(self, _task_id: str, event: dict[str, Any]) -> None:
        self.published_events.append(event)

    async def set_state(self, _task_id: str, _state: dict[str, Any]) -> None:
        return None


def test_send_ws_event_supports_multi_subscribers() -> None:
    task_id = "task-multi-ws"
    ws1 = _WsSpy()
    ws2 = _WsSpy()
    backend_app.task_websockets.clear()
    backend_app.task_websockets[task_id] = {ws1, ws2}  # type: ignore[arg-type]

    event = {"event_type": "log", "flow_id": task_id, "message": "ok"}
    asyncio.run(backend_app._send_ws_event(task_id, event))

    assert len(ws1.events) == 1
    assert len(ws2.events) == 1
    assert ws1.events[0]["event_type"] == "log"
    assert ws2.events[0]["event_type"] == "log"


def _build_request() -> Request:
    scope = {
        "type": "http",
        "method": "POST",
        "path": "/api/migration/execute",
        "headers": [],
    }
    return Request(scope)


def test_build_execution_inputs_maps_verification_mode() -> None:
    strict_request = backend_app.MigrationRequest(source_code="IDENTIFICATION DIVISION.")
    strict_inputs = backend_app._build_execution_inputs(
        "task-strict",
        strict_request,
        Path("/tmp/artifacts-strict"),
    )
    assert strict_inputs["options"]["verification_mode"] == "strict"
    assert strict_inputs["fast_mode"] is False

    fast_request = backend_app.MigrationRequest(
        source_code="IDENTIFICATION DIVISION.",
        options=backend_app.MigrationOptions(verification_mode="fast"),
    )
    fast_inputs = backend_app._build_execution_inputs(
        "task-fast",
        fast_request,
        Path("/tmp/artifacts-fast"),
    )
    assert fast_inputs["options"]["verification_mode"] == "fast"
    assert fast_inputs["fast_mode"] is True


def test_legacy_execute_endpoint_semantics_match_new_endpoint() -> None:
    backend_app.active_tasks.clear()
    request = _build_request()
    body = backend_app.MigrationRequest(source_code="IDENTIFICATION DIVISION.")

    execute_resp = asyncio.run(
        backend_app.execute_migration(
            request=request,
            body=body,
            background_tasks=BackgroundTasks(),
        )
    )
    start_resp = asyncio.run(
        backend_app.start_migration(
            request=request,
            body=body,
            background_tasks=BackgroundTasks(),
        )
    )

    assert execute_resp["status"] == start_resp["status"]
    assert execute_resp["ws_url"].startswith("/api/ws/")
    assert start_resp["ws_url"].startswith("/api/ws/")
    assert start_resp["deprecated"] is True
    assert start_resp["replacement"] == "/api/migration/execute"


def test_legacy_approval_endpoint_forwards_to_commands(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    task_id = "task-legacy-approval"
    runtime = _make_runtime(task_id, tmp_path / task_id)

    pending = ApprovalRequest(action="design_review", reason="approval required")
    runtime.engine._approval_flow._pending[pending.id] = pending

    response = asyncio.run(
        backend_app.submit_approval(
            task_id=task_id,
            request_id=pending.id,
            approval=backend_app.ApprovalRequest(approved=True, comment="legacy path"),
        )
    )

    assert response["status"] == "submitted"
    assert response["deprecated"] is True
    assert response["replacement"] == f"/api/migration/{task_id}/commands"


def test_get_state_falls_back_to_distributed_store() -> None:
    backend_app.active_tasks.clear()
    original_store = backend_app._redis_store
    backend_app._redis_store = _StubDistributedStore(
        {
            "task_id": "task-distributed",
            "status": "running",
            "contract_version": backend_app.CONTRACT_VERSION,
            "commands": [],
            "pending_approvals": [],
            "observation_events_count": 0,
            "capability_trace": [],
        }
    )
    try:
        state = asyncio.run(backend_app.get_migration_state("task-distributed"))
    finally:
        backend_app._redis_store = original_store

    assert state["task_id"] == "task-distributed"
    assert state["status"] == "running"


def test_commands_reject_when_task_owned_by_other_process() -> None:
    backend_app.active_tasks.clear()
    original_store = backend_app._redis_store
    backend_app._redis_store = _StubDistributedStore(
        {
            "task_id": "task-remote-owner",
            "status": "running",
            "owner_instance": "remote-instance",
        }
    )
    try:
        with pytest.raises(HTTPException) as exc_info:
            asyncio.run(
                backend_app.post_migration_command(
                    "task-remote-owner",
                    backend_app.TaskCommandRequest(command="provide_business_fact"),
                )
            )
    finally:
        backend_app._redis_store = original_store
    assert exc_info.value.status_code == 409


def test_commands_dispatch_through_redis_command_bus() -> None:
    backend_app.active_tasks.clear()
    original_store = backend_app._redis_store
    backend_app._redis_store = _CommandBusDistributedStore(
        {
            "task_id": "task-remote-owner",
            "status": "running",
            "owner_instance": "remote-instance",
        }
    )
    try:
        response = asyncio.run(
            backend_app.post_migration_command(
                "task-remote-owner",
                backend_app.TaskCommandRequest(
                    command="provide_business_fact",
                    actor="analyst",
                    fact={"kind": "rule", "name": "remote-rule"},
                ),
            )
        )
    finally:
        backend_app._redis_store = original_store

    assert response["status"] == "accepted"
    assert response["mode"] == "observer"
    assert response["applied"] is True
    assert response["effect_scope"] == "business_context"


def test_remote_command_request_executes_on_owner_runtime(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    task_id = "task-owner-runtime"
    runtime = _make_runtime(task_id, tmp_path / task_id)
    original_store = backend_app._redis_store
    store = _CommandBusDistributedStore(None)
    store.instance_id = "owner-process"
    backend_app._redis_store = store

    payload = {
        "task_id": task_id,
        "command_id": "cmd-owner-1",
        "requester_instance_id": "requester-process",
        "command": {
            "command": "provide_business_fact",
            "actor": "reviewer",
            "fact": {"kind": "event", "name": "ORDER_CONFIRMED"},
        },
    }
    try:
        asyncio.run(backend_app._handle_remote_command_request(payload))
        result = asyncio.run(store.pop_command_result("cmd-owner-1"))
    finally:
        backend_app._redis_store = original_store

    assert isinstance(result, dict)
    assert result["ok"] is True
    assert result["payload"]["status"] == "accepted"
    assert runtime.observation_events_count == 1
    assert len(runtime.command_history) == 1
    published_types = [event.get("event_type") for event in store.published_events]
    assert "command_result" in published_types


def test_redis_listener_forwards_remote_event_to_websocket() -> None:
    backend_app.active_tasks.clear()
    backend_app.task_websockets.clear()
    task_id = "task-redis-listener"
    ws = _WsSpy()
    backend_app.task_websockets[task_id] = {ws}  # type: ignore[arg-type]

    payload = {
        "task_id": task_id,
        "producer_id": "remote-process",
        "event": {"event_type": "log", "flow_id": task_id, "message": "from redis"},
    }

    original_store = backend_app._redis_store
    backend_app._redis_store = _OneShotDistributedStore(payload)
    try:
        async def _run_listener_once() -> None:
            listener = asyncio.create_task(backend_app._redis_event_listener())
            try:
                for _ in range(50):
                    if ws.events:
                        break
                    await asyncio.sleep(0.01)
            finally:
                listener.cancel()
                with contextlib.suppress(asyncio.CancelledError):
                    await listener

        asyncio.run(_run_listener_once())
    finally:
        backend_app._redis_store = original_store
        backend_app.task_websockets.clear()

    assert len(ws.events) == 1
    assert ws.events[0]["message"] == "from redis"
