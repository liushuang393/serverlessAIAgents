"""ヒューマンコマンド適用 / 分散コマンド委譲."""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from typing import Any

from fastapi import HTTPException
from pydantic import ValidationError

from apps.code_migration_assistant.backend.event_bus import emit_event, sync_runtime_state
from apps.code_migration_assistant.backend.models import TaskCommandRequest
from apps.code_migration_assistant.backend.task_runtime import (
    TaskRuntime,
    active_tasks,
    append_command_log,
    local_instance_id,
    normalize_state_payload,
    redis_store,
    write_feedback_artifact,
)

logger = logging.getLogger("migration_server")


async def apply_local_command(
    task_id: str,
    runtime: TaskRuntime,
    command: TaskCommandRequest,
) -> dict[str, Any]:
    """ローカル owner のタスクへヒューマンコマンドを適用する."""
    actor = command.actor or "operator"
    command_name = command.command.strip().lower()

    async with runtime.command_lock:
        now = time.time()
        response: dict[str, Any]
        extra_events: list[dict[str, Any]] = []

        if command_name in {"approve", "reject"}:
            if not command.request_id:
                raise HTTPException(status_code=422, detail="request_id is required")

            approved = command_name == "approve"
            submitted = await runtime.engine.approval_flow.submit_response(
                request_id=command.request_id,
                approved=approved,
                comment=command.comment,
                approver=actor,
                modifications=command.modifications,
            )
            if not submitted:
                raise HTTPException(status_code=409, detail="approval request not found")

            response = {
                "status": "submitted",
                "task_id": task_id,
                "command": command_name,
                "approved": approved,
                "mode": "observer",
                "applied": True,
                "effect_scope": "approval_flow",
            }
            extra_events.append(
                {
                    "event_type": "approval_submitted",
                    "request_id": command.request_id,
                    "approved": approved,
                    "comment": command.comment,
                    "actor": actor,
                }
            )

        elif command_name == "provide_business_fact":
            fact = command.fact or {}
            if not isinstance(fact, dict):
                raise HTTPException(status_code=422, detail="fact must be an object")

            human_facts = getattr(runtime.engine, "_human_facts", None)
            if not isinstance(human_facts, list):
                human_facts = []
                runtime.engine._human_facts = human_facts  # type: ignore[attr-defined]
            human_facts.append(fact)

            feedback_record = {
                "task_id": task_id,
                "timestamp": now,
                "actor": actor,
                "command": command_name,
                "fact": fact,
                "comment": command.comment,
            }
            feedback_path = write_feedback_artifact(runtime, feedback_record)

            response = {
                "status": "accepted",
                "task_id": task_id,
                "command": command_name,
                "mode": "observer",
                "applied": True,
                "effect_scope": "business_context",
                "feedback": {
                    "command": command_name,
                    "fact": fact,
                    "comment": command.comment,
                },
                "feedback_artifact_path": str(feedback_path),
            }

        elif command_name == "request_replan":
            response = {
                "status": "observed",
                "task_id": task_id,
                "command": command_name,
                "mode": "observer",
                "applied": False,
                "effect_scope": "planning",
                "comment": command.comment,
            }

        else:
            raise HTTPException(status_code=400, detail=f"unsupported command: {command_name}")

        record = {
            "timestamp": now,
            "task_id": task_id,
            "command": command_name,
            "request_id": command.request_id,
            "actor": actor,
            "comment": command.comment,
            "status": response.get("status"),
            "mode": response.get("mode"),
            "applied": response.get("applied"),
            "effect_scope": response.get("effect_scope"),
        }
        runtime.command_history.append(record)
        runtime.observation_events_count += 1
        append_command_log(runtime, record)

    await emit_event(
        task_id,
        {
            "event_type": "command_result",
            "command": command_name,
            "status": response.get("status", "unknown"),
            "mode": response.get("mode", "observer"),
            "applied": bool(response.get("applied", False)),
            "effect_scope": response.get("effect_scope", "unknown"),
            "actor": actor,
        },
    )
    for event in extra_events:
        await emit_event(task_id, event)

    await sync_runtime_state(runtime)
    return response


async def dispatch_remote_command(task_id: str, command: TaskCommandRequest) -> dict[str, Any]:
    """owner が別プロセスの場合、コマンドを分散バス経由で委譲する."""
    store = redis_store
    if store is None:
        raise HTTPException(status_code=409, detail="task is owned by another process")

    if not (hasattr(store, "publish_command") and hasattr(store, "pop_command_result")):
        raise HTTPException(status_code=409, detail="task is owned by another process")

    command_id = f"cmd-{uuid.uuid4().hex}"
    payload = {
        "task_id": task_id,
        "command_id": command_id,
        "requester_instance_id": local_instance_id(),
        "command": command.model_dump(exclude_none=True),
    }

    await store.publish_command(payload)

    deadline = time.monotonic() + 5.0
    while time.monotonic() < deadline:
        result = await store.pop_command_result(command_id)
        if isinstance(result, dict):
            if not bool(result.get("ok", False)):
                status_code_raw = result.get("status_code", 500)
                status_code = int(status_code_raw) if isinstance(status_code_raw, int) else 500
                detail = str(result.get("detail", "remote command failed"))
                raise HTTPException(status_code=status_code, detail=detail)

            payload_obj = result.get("payload")
            if isinstance(payload_obj, dict):
                return payload_obj
            raise HTTPException(status_code=502, detail="invalid remote payload")
        await asyncio.sleep(0.05)

    raise HTTPException(status_code=504, detail="remote command timeout")


async def resolve_distributed_state(task_id: str) -> dict[str, Any] | None:
    """分散ストアからタスク状態を解決する."""
    store = redis_store
    if store is None or not hasattr(store, "get_state"):
        return None
    state = await store.get_state(task_id)
    if isinstance(state, dict):
        return normalize_state_payload(state)
    return None


async def handle_remote_command_request(payload: dict[str, Any]) -> None:
    """他プロセスから到着したコマンド要求をローカル owner として処理する."""
    store = redis_store
    if store is None or not hasattr(store, "set_command_result"):
        return

    task_id = payload.get("task_id")
    command_id = payload.get("command_id")
    requester_instance_id = payload.get("requester_instance_id")
    command_payload = payload.get("command")

    if not isinstance(task_id, str) or not isinstance(command_id, str):
        return
    if not isinstance(requester_instance_id, str):
        return
    if requester_instance_id == local_instance_id():
        return

    runtime = active_tasks.get(task_id)
    if runtime is None:
        await store.set_command_result(
            command_id,
            {
                "task_id": task_id,
                "command_id": command_id,
                "requester_instance_id": requester_instance_id,
                "responder_instance_id": local_instance_id(),
                "ok": False,
                "status_code": 404,
                "detail": "task not found in owner runtime",
                "payload": None,
            },
        )
        return

    try:
        if not isinstance(command_payload, dict):
            raise HTTPException(status_code=422, detail="command payload must be object")
        command_req = TaskCommandRequest.model_validate(command_payload)
        cmd_response = await apply_local_command(task_id, runtime, command_req)
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": local_instance_id(),
            "ok": True,
            "status_code": 200,
            "detail": None,
            "payload": cmd_response,
        }
    except HTTPException as exc:
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": local_instance_id(),
            "ok": False,
            "status_code": exc.status_code,
            "detail": str(exc.detail),
            "payload": None,
        }
    except ValidationError as exc:
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": local_instance_id(),
            "ok": False,
            "status_code": 422,
            "detail": str(exc),
            "payload": None,
        }
    except Exception as exc:
        logger.exception("remote command handling failed")
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": local_instance_id(),
            "ok": False,
            "status_code": 500,
            "detail": str(exc),
            "payload": None,
        }

    await store.set_command_result(command_id, result)


async def redis_command_listener() -> None:
    """分散コマンドチャネルを購読し、owner として実行する."""
    store = redis_store
    if store is None or not hasattr(store, "subscribe_commands"):
        return

    async for cmd_payload in store.subscribe_commands():
        await handle_remote_command_request(cmd_payload)
