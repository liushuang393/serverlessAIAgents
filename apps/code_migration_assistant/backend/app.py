from __future__ import annotations

import asyncio
import json
import logging
import os
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.backend.knowledge_router import router as knowledge_router
from apps.code_migration_assistant.backend.migration_router import router as migration_ui_router
from apps.code_migration_assistant.backend.task_store import RedisTaskStore
from apps.code_migration_assistant.engine import CodeMigrationEngine
from fastapi import BackgroundTasks, FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field, ValidationError, field_validator

from agentflow.security.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("migration_server")

CONTRACT_VERSION = "2026-03-01"


class MigrationOptions(BaseModel):
    """移行実行オプション."""

    verification_mode: str = "strict"

    @field_validator("verification_mode")
    @classmethod
    def _normalize_verification_mode(cls, value: str) -> str:
        normalized = value.strip().lower()
        if normalized not in {"strict", "fast"}:
            return "strict"
        return normalized


class MigrationRequest(BaseModel):
    """移行開始リクエスト."""

    source_code: str
    migration_type: str = "cobol-to-java"
    options: MigrationOptions = Field(default_factory=MigrationOptions)
    expected_outputs: dict[str, Any] = Field(default_factory=dict)
    module: str | None = None


class ApprovalRequest(BaseModel):
    """Legacy approval API リクエスト."""

    approved: bool
    comment: str | None = None


class TaskCommandRequest(BaseModel):
    """ヒューマンコマンド API リクエスト."""

    command: str
    request_id: str | None = None
    actor: str | None = None
    comment: str | None = None
    fact: dict[str, Any] | None = None
    modifications: dict[str, Any] = Field(default_factory=dict)


@dataclass
class TaskRuntime:
    """実行中タスクのローカルランタイム状態."""

    task_id: str
    engine: CodeMigrationEngine
    inputs: dict[str, Any]
    flow_context: Any | None
    artifacts_dir: Path
    status: str = "running"
    result: dict[str, Any] | None = None
    error: str | None = None
    command_history: list[dict[str, Any]] = field(default_factory=list)
    observation_events_count: int = 0
    created_at: float = field(default_factory=time.time)
    command_lock: asyncio.Lock = field(default_factory=asyncio.Lock)

    def to_state(self, owner_instance: str | None) -> dict[str, Any]:
        """配布可能な状態スナップショットへ変換する."""
        capability_trace: list[dict[str, Any]] = []
        if isinstance(self.result, dict):
            trace = self.result.get("capability_trace")
            if isinstance(trace, list):
                capability_trace = [item for item in trace if isinstance(item, dict)]

        pending_approvals = _collect_pending_approvals(self.engine)

        return {
            "task_id": self.task_id,
            "status": self.status,
            "contract_version": CONTRACT_VERSION,
            "result": self.result,
            "error": self.error,
            "commands": list(self.command_history),
            "pending_approvals": pending_approvals,
            "observation_events_count": self.observation_events_count,
            "capability_trace": capability_trace,
            "owner_instance": owner_instance,
        }


def _resolve_cors_origins() -> list[str]:
    """CORS Origin を環境変数または安全なローカル既定値から解決する."""
    raw = os.getenv("CODE_MIGRATION_CORS_ORIGINS", "").strip()
    if raw:
        origins = [origin.strip() for origin in raw.split(",") if origin.strip()]
        if origins:
            return origins
    return ["http://localhost:3000", "http://localhost:5173", "http://localhost:5174"]


def _resolve_frontend_directory() -> Path:
    """静的フロントディレクトリを解決する."""
    frontend_dir = Path(__file__).resolve().parents[1] / "frontend"
    logger.info("Serving frontend from %s", frontend_dir)
    return frontend_dir


def _create_distributed_store() -> RedisTaskStore[dict[str, Any]] | None:
    """Redis 分散ストアを構築する（未設定なら None）."""
    redis_url = os.getenv("CODE_MIGRATION_REDIS_URL", "").strip()
    if not redis_url:
        return None
    return RedisTaskStore[dict[str, Any]](
        redis_url=redis_url,
        key_prefix="code_migration_assistant",
    )


_redis_store = _create_distributed_store()


def _load_app_config() -> dict[str, Any]:
    """app_config.json を読み込む."""
    return _auth_guard.load_app_config()


def _is_auth_required() -> bool:
    """認証契約が有効か判定する."""
    return _auth_guard.is_auth_required()


def _verify_api_key(incoming_key: str | None) -> None:
    """API キーを検証する."""
    _auth_guard.verify_api_key(incoming_key)


def _should_protect_http_path(path: str) -> bool:
    """HTTP パスに API キー保護が必要か判定する."""
    return _auth_guard.should_protect_http_path(path)


def _local_instance_id() -> str:
    """現在プロセスの識別子を返す."""
    store = _redis_store
    if store is None:
        return "local-process"
    instance = getattr(store, "instance_id", "local-process")
    return str(instance)


def _build_execution_inputs(task_id: str, body: MigrationRequest, artifacts_dir: Path) -> dict[str, Any]:
    """Engine 実行入力を契約形式で組み立てる."""
    verification_mode = body.options.verification_mode
    fast_mode = verification_mode == "fast"

    options: dict[str, Any] = {
        "verification_mode": verification_mode,
    }

    return {
        "source_code": body.source_code,
        "task_id": task_id,
        "module": body.module or "UNKNOWN",
        "migration_type": body.migration_type,
        "artifacts_dir": str(artifacts_dir),
        "expected_outputs": body.expected_outputs,
        "fast_mode": fast_mode,
        "options": options,
    }


def _collect_pending_approvals(engine: CodeMigrationEngine) -> list[dict[str, Any]]:
    """ApprovalFlow の pending request を JSON 化する."""
    pending: list[dict[str, Any]] = []
    try:
        for item in engine._approval_flow.get_pending_requests():
            pending.append(
                {
                    "id": getattr(item, "id", ""),
                    "action": getattr(item, "action", ""),
                    "reason": getattr(item, "reason", ""),
                    "context": getattr(item, "context", {}),
                }
            )
    except Exception:
        logger.debug("collect pending approvals failed", exc_info=True)
    return pending


def _normalize_state_payload(state: dict[str, Any]) -> dict[str, Any]:
    """分散ストアから取得した state を契約形に正規化する."""
    normalized = dict(state)
    normalized.setdefault("contract_version", CONTRACT_VERSION)
    normalized.setdefault("commands", [])
    normalized.setdefault("pending_approvals", [])
    normalized.setdefault("observation_events_count", 0)
    normalized.setdefault("capability_trace", [])
    normalized.setdefault("status", "unknown")
    normalized.setdefault("error", None)
    return normalized


def _normalize_stream_event(task_id: str, raw_event: dict[str, Any]) -> dict[str, Any]:
    """内部イベントを AG-UI 互換イベントへ正規化する."""
    event = dict(raw_event)

    raw_event_type = str(event.get("event_type") or "").strip()
    raw_event_name = str(event.get("event") or "").strip()

    event_map = {
        "node_start": "node.start",
        "node_complete": "node.complete",
        "flow_start": "flow.start",
        "flow_complete": "flow.complete",
        "flow_error": "flow.error",
    }

    if raw_event_type:
        event_type = event_map.get(raw_event_type, raw_event_type)
    else:
        event_type = event_map.get(raw_event_name, "log")

    timestamp_raw = event.get("timestamp")
    timestamp = float(timestamp_raw) if isinstance(timestamp_raw, (int, float)) else time.time()

    event.pop("event", None)
    event["event_type"] = event_type
    event["flow_id"] = task_id
    event["timestamp"] = timestamp
    event["contract_version"] = CONTRACT_VERSION

    return event


async def _send_ws_event(task_id: str, event: dict[str, Any]) -> None:
    """タスクに紐づく全 WebSocket へイベント送信する."""
    subscribers = task_websockets.get(task_id)
    if subscribers is None:
        return

    sockets = list(subscribers)
    stale: list[Any] = []
    for websocket in sockets:
        try:
            await websocket.send_json(event)
        except Exception:
            stale.append(websocket)

    for websocket in stale:
        subscribers.discard(websocket)
    if not subscribers:
        task_websockets.pop(task_id, None)


async def _append_distributed_event(task_id: str, event: dict[str, Any]) -> None:
    """分散ストアへイベント履歴を記録し、他プロセスへ配信する."""
    store = _redis_store
    if store is None:
        return

    try:
        if hasattr(store, "append_event"):
            await store.append_event(task_id, event)
        if hasattr(store, "publish_event"):
            await store.publish_event(task_id, event)
    except Exception:
        logger.warning("distributed event publish failed", exc_info=True)


async def _emit_event(task_id: str, raw_event: dict[str, Any]) -> None:
    """イベントを正規化し、WebSocket と分散ストアへ配信する."""
    normalized = _normalize_stream_event(task_id, raw_event)
    await _send_ws_event(task_id, normalized)
    await _append_distributed_event(task_id, normalized)


async def _sync_runtime_state(runtime: TaskRuntime) -> None:
    """ランタイム状態を分散ストアへ同期する."""
    store = _redis_store
    if store is None or not hasattr(store, "set_state"):
        return

    try:
        await store.set_state(runtime.task_id, runtime.to_state(owner_instance=_local_instance_id()))
    except Exception:
        logger.warning("distributed state sync failed", exc_info=True)


def _append_command_log(runtime: TaskRuntime, record: dict[str, Any]) -> None:
    """コマンド監査ログを JSONL へ追記する."""
    log_path = runtime.artifacts_dir / "logs" / "commands.jsonl"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("a", encoding="utf-8") as fp:
        fp.write(json.dumps(record, ensure_ascii=False, default=str) + "\n")


def _write_feedback_artifact(runtime: TaskRuntime, record: dict[str, Any]) -> Path:
    """human feedback を成果物として保存する."""
    feedback_dir = runtime.artifacts_dir / "human_feedback"
    feedback_dir.mkdir(parents=True, exist_ok=True)
    filename = f"feedback_{int(time.time() * 1000)}_{uuid.uuid4().hex[:8]}.json"
    artifact_path = feedback_dir / filename
    artifact_path.write_text(
        json.dumps(record, ensure_ascii=False, indent=2, default=str),
        encoding="utf-8",
    )
    return artifact_path


async def _apply_local_command(
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
            submitted = await runtime.engine._approval_flow.submit_response(
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
                runtime.engine._human_facts = human_facts
            human_facts.append(fact)

            feedback_record = {
                "task_id": task_id,
                "timestamp": now,
                "actor": actor,
                "command": command_name,
                "fact": fact,
                "comment": command.comment,
            }
            feedback_path = _write_feedback_artifact(runtime, feedback_record)

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
        _append_command_log(runtime, record)

    await _emit_event(
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
        await _emit_event(task_id, event)

    await _sync_runtime_state(runtime)
    return response


async def _dispatch_remote_command(task_id: str, command: TaskCommandRequest) -> dict[str, Any]:
    """owner が別プロセスの場合、コマンドを分散バス経由で委譲する."""
    store = _redis_store
    if store is None:
        raise HTTPException(status_code=409, detail="task is owned by another process")

    if not (hasattr(store, "publish_command") and hasattr(store, "pop_command_result")):
        raise HTTPException(status_code=409, detail="task is owned by another process")

    command_id = f"cmd-{uuid.uuid4().hex}"
    payload = {
        "task_id": task_id,
        "command_id": command_id,
        "requester_instance_id": _local_instance_id(),
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


async def _resolve_distributed_state(task_id: str) -> dict[str, Any] | None:
    """分散ストアからタスク状態を解決する."""
    store = _redis_store
    if store is None or not hasattr(store, "get_state"):
        return None
    state = await store.get_state(task_id)
    if isinstance(state, dict):
        return _normalize_state_payload(state)
    return None


async def _handle_remote_command_request(payload: dict[str, Any]) -> None:
    """他プロセスから到着したコマンド要求をローカル owner として処理する."""
    store = _redis_store
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
    if requester_instance_id == _local_instance_id():
        return

    runtime = active_tasks.get(task_id)
    if runtime is None:
        await store.set_command_result(
            command_id,
            {
                "task_id": task_id,
                "command_id": command_id,
                "requester_instance_id": requester_instance_id,
                "responder_instance_id": _local_instance_id(),
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
        response = await _apply_local_command(task_id, runtime, command_req)
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": _local_instance_id(),
            "ok": True,
            "status_code": 200,
            "detail": None,
            "payload": response,
        }
    except HTTPException as exc:
        result = {
            "task_id": task_id,
            "command_id": command_id,
            "requester_instance_id": requester_instance_id,
            "responder_instance_id": _local_instance_id(),
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
            "responder_instance_id": _local_instance_id(),
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
            "responder_instance_id": _local_instance_id(),
            "ok": False,
            "status_code": 500,
            "detail": str(exc),
            "payload": None,
        }

    await store.set_command_result(command_id, result)


async def _redis_event_listener() -> None:
    """分散イベントチャネルを購読し、ローカル WebSocket へ中継する."""
    store = _redis_store
    if store is None or not hasattr(store, "subscribe_events"):
        return

    async for payload in store.subscribe_events():
        task_id = payload.get("task_id")
        producer_id = payload.get("producer_id")
        event = payload.get("event")
        if not isinstance(task_id, str) or not isinstance(event, dict):
            continue
        if isinstance(producer_id, str) and producer_id == _local_instance_id():
            continue
        await _send_ws_event(task_id, event)


async def _redis_command_listener() -> None:
    """分散コマンドチャネルを購読し、owner として実行する."""
    store = _redis_store
    if store is None or not hasattr(store, "subscribe_commands"):
        return

    async for payload in store.subscribe_commands():
        await _handle_remote_command_request(payload)


async def run_migration_task(task_id: str, runtime: TaskRuntime) -> None:
    """Migration Engine をバックグラウンド実行する."""
    await _emit_event(task_id, {"event_type": "flow.start", "status": "started"})

    try:
        async for raw_event in runtime.engine._execute_stream(runtime.inputs):
            if not isinstance(raw_event, dict):
                continue

            await _emit_event(task_id, raw_event)

            event_name = str(raw_event.get("event") or "")
            node_name = str(raw_event.get("node") or "")
            if event_name == "node_complete" and node_name == "migration_pipeline":
                result_obj = raw_event.get("result")
                if isinstance(result_obj, dict):
                    runtime.result = result_obj

        runtime.status = "complete"
        await _emit_event(
            task_id,
            {
                "event_type": "flow.complete",
                "status": "complete",
                "result": runtime.result or {},
            },
        )
    except Exception as exc:
        runtime.status = "error"
        runtime.error = str(exc)
        logger.exception("Migration task failed")
        await _emit_event(
            task_id,
            {
                "event_type": "flow.error",
                "status": "error",
                "error_message": str(exc),
                "error_type": type(exc).__name__,
            },
        )
    finally:
        await _sync_runtime_state(runtime)


_cors_origins = _resolve_cors_origins()
_cors_allow_credentials = not (len(_cors_origins) == 1 and _cors_origins[0] == "*")

app = FastAPI(title="Code Migration Assistant API")

app.add_middleware(
    CORSMiddleware,
    allow_origins=_cors_origins,
    allow_credentials=_cors_allow_credentials,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Global runtime state
active_tasks: dict[str, TaskRuntime] = {}
task_websockets: dict[str, set[Any]] = {}

# Auth contract setup
_APP_CONFIG_PATH = Path(__file__).resolve().parents[1] / "app_config.json"
_PUBLIC_HTTP_PATHS = {"/api/health", "/docs", "/redoc", "/openapi.json"}
_AUTH_HEADER_NAME = "x-api-key"
_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_HTTP_PATHS,
        auth_header_name=_AUTH_HEADER_NAME,
        ws_query_key="api_key",
        api_key_env_selector_var="CODE_MIGRATION_API_KEY_ENV",
        default_api_key_env_var="CODE_MIGRATION_API_KEY",
    ),
)


async def _require_http_api_key(request: Request) -> None:
    """Enforce API key for protected HTTP routes."""
    await _auth_guard.require_http(request)


async def _require_ws_api_key(websocket: WebSocket) -> bool:
    """Enforce API key for websocket handshake."""
    ok, _ = await _auth_guard.require_ws(websocket)
    return ok


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    return await _auth_guard.http_middleware(request, call_next)


@app.get("/api/health")
async def health_check() -> dict[str, str]:
    """ヘルスチェック."""
    return {"status": "healthy", "service": "code_migration_assistant"}


app.include_router(migration_ui_router)
app.include_router(knowledge_router)


# ---------------------------------------------------------------------------
# 知識ベース DB / マネージャー初期化（起動時）
# ---------------------------------------------------------------------------

_knowledge_initialized = False


async def _init_knowledge_managers() -> None:
    """RAG 管理テーブルとマネージャーを初期化."""
    global _knowledge_initialized
    if _knowledge_initialized:
        return
    try:
        from apps.code_migration_assistant.backend.knowledge_db import (
            get_knowledge_session_factory,
            init_knowledge_db,
        )
        from apps.code_migration_assistant.backend.knowledge_router import init_managers

        from agentflow.knowledge.collection_manager import CollectionManager
        from agentflow.knowledge.document_manager import DocumentManager

        await init_knowledge_db()
        session_factory = get_knowledge_session_factory()

        col_mgr = CollectionManager(session_factory=session_factory)
        doc_mgr = DocumentManager(collection_manager=col_mgr, session_factory=session_factory)
        init_managers(col_mgr, doc_mgr)
        _knowledge_initialized = True
        logger.info("Knowledge managers initialized")
    except Exception as e:
        logger.warning("Knowledge manager init failed (non-critical): %s", e)


@app.on_event("startup")
async def _startup_knowledge() -> None:
    await _init_knowledge_managers()


async def _start_task(body: MigrationRequest, background_tasks: BackgroundTasks) -> dict[str, Any]:
    """共通タスク起動処理."""
    task_id = str(uuid.uuid4())
    engine = CodeMigrationEngine(migration_type=body.migration_type)
    await engine._initialize()

    artifacts_dir = Path("/tmp/migration_artifacts") / task_id
    runtime = TaskRuntime(
        task_id=task_id,
        engine=engine,
        inputs=_build_execution_inputs(task_id, body, artifacts_dir),
        flow_context=None,
        artifacts_dir=artifacts_dir,
        status="running",
    )

    active_tasks[task_id] = runtime
    await _sync_runtime_state(runtime)

    background_tasks.add_task(run_migration_task, task_id, runtime)

    return {
        "task_id": task_id,
        "status": "started",
        "ws_url": f"/api/ws/{task_id}",
    }


@app.post("/api/migration/start")
async def start_migration(
    request: Request,
    body: MigrationRequest,
    background_tasks: BackgroundTasks,
) -> dict[str, Any]:
    """旧 start エンドポイント（互換維持）."""
    del request
    response = await _start_task(body, background_tasks)
    response["deprecated"] = True
    response["replacement"] = "/api/migration/execute"
    return response


@app.post("/api/migration/execute")
async def execute_migration(
    request: Request,
    body: MigrationRequest,
    background_tasks: BackgroundTasks,
) -> dict[str, Any]:
    """推奨の移行実行エンドポイント."""
    del request
    return await _start_task(body, background_tasks)


@app.get("/api/migration/{task_id}/state")
async def get_migration_state(task_id: str) -> dict[str, Any]:
    """タスク状態を返す（ローカル優先、次に分散ストア）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return runtime.to_state(owner_instance=_local_instance_id())

    distributed_state = await _resolve_distributed_state(task_id)
    if distributed_state is not None:
        return distributed_state

    raise HTTPException(status_code=404, detail="Task not found")


@app.post("/api/migration/{task_id}/commands")
async def post_migration_command(task_id: str, command: TaskCommandRequest) -> dict[str, Any]:
    """ヒューマンコマンドを受け付ける（owner 判定つき）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return await _apply_local_command(task_id, runtime, command)

    distributed_state = await _resolve_distributed_state(task_id)
    if distributed_state is None:
        raise HTTPException(status_code=404, detail="Task not found")

    owner_instance = distributed_state.get("owner_instance")
    if isinstance(owner_instance, str) and owner_instance and owner_instance != _local_instance_id():
        return await _dispatch_remote_command(task_id, command)

    raise HTTPException(status_code=409, detail="task is owned by another process")


@app.websocket("/api/ws/{task_id}")
async def websocket_endpoint(websocket: WebSocket, task_id: str) -> None:
    """イベントストリーム WebSocket エンドポイント."""
    ok, _ = await _auth_guard.require_ws(websocket)
    if not ok:
        return

    await websocket.accept()
    subscribers = task_websockets.setdefault(task_id, set())
    subscribers.add(websocket)

    try:
        while True:
            await websocket.receive_text()
    except WebSocketDisconnect:
        logger.info("Client disconnected for task %s", task_id)
    finally:
        current = task_websockets.get(task_id)
        if current is not None:
            current.discard(websocket)
            if not current:
                task_websockets.pop(task_id, None)


@app.get("/api/approvals/{task_id}")
async def get_approvals(task_id: str) -> list[dict[str, Any]]:
    """保留中承認一覧（legacy endpoint）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return _collect_pending_approvals(runtime.engine)

    distributed_state = await _resolve_distributed_state(task_id)
    if distributed_state is not None:
        pending = distributed_state.get("pending_approvals", [])
        if isinstance(pending, list):
            return [item for item in pending if isinstance(item, dict)]

    raise HTTPException(status_code=404, detail="Task not found")


@app.post("/api/approvals/{task_id}/{request_id}")
async def submit_approval(task_id: str, request_id: str, approval: ApprovalRequest) -> dict[str, Any]:
    """旧承認 API を commands API へ転送する."""
    command_name = "approve" if approval.approved else "reject"
    response = await post_migration_command(
        task_id,
        TaskCommandRequest(
            command=command_name,
            request_id=request_id,
            actor="admin",
            comment=approval.comment,
        ),
    )
    response["deprecated"] = True
    response["replacement"] = f"/api/migration/{task_id}/commands"
    return response


@app.get("/api/artifacts/{task_id}/{stage}/{filename}")
async def get_artifact(task_id: str, stage: str, filename: str) -> FileResponse:
    """成果物を取得する（legacy endpoint）."""
    path = Path(f"/tmp/migration_artifacts/{task_id}/{stage}/{filename}")
    if not path.exists():
        raise HTTPException(status_code=404, detail="Artifact not found")
    return FileResponse(path)


# Serve UI
app.mount("/", StaticFiles(directory="apps/code_migration_assistant/frontend", html=True), name="ui")

if __name__ == "__main__":
    import uvicorn

    config_path = Path(__file__).resolve().parents[1] / "app_config.json"
    config_raw: dict[str, Any] = {}
    if config_path.is_file():
        try:
            config_raw = json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError:
            config_raw = {}

    api_port = config_raw.get("ports", {}).get("api", 8003)
    uvicorn.run(app, host="0.0.0.0", port=int(api_port))
