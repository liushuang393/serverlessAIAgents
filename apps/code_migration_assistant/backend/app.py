import json
import logging
import os
import time
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.engine import CodeMigrationEngine
from fastapi import BackgroundTasks, FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

from agentflow.protocols.agui_events import (
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeStartEvent,
)
from agentflow.security.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig


# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("migration_server")


def _resolve_cors_origins() -> list[str]:
    """Resolve CORS origins from env or use safe local defaults."""
    raw = os.getenv("CODE_MIGRATION_CORS_ORIGINS", "").strip()
    if raw:
        origins = [origin.strip() for origin in raw.split(",") if origin.strip()]
        if origins:
            return origins
    return ["http://localhost:3000", "http://localhost:5173", "http://localhost:5174"]


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

# Global State
active_tasks: dict[str, CodeMigrationEngine] = {}
task_websockets: dict[str, WebSocket] = {}
_APP_CONFIG_PATH = Path(__file__).resolve().parents[1] / "app_config.json"
_PUBLIC_HTTP_PATHS = {"/api/health", "/docs", "/redoc", "/openapi.json"}
_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_HTTP_PATHS,
        auth_header_name="x-api-key",
        ws_query_key="api_key",
        api_key_env_selector_var="CODE_MIGRATION_API_KEY_ENV",
        default_api_key_env_var="CODE_MIGRATION_API_KEY",
    ),
)


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json or return an empty dict."""
    return _auth_guard.load_app_config()


def _is_auth_required() -> bool:
    """Evaluate whether API key auth must be enforced."""
    return _auth_guard.is_auth_required()


def _verify_api_key(incoming_key: str | None) -> None:
    """Validate API key when auth is required."""
    _auth_guard.verify_api_key(incoming_key)


def _should_protect_http_path(path: str) -> bool:
    """Return whether HTTP path should be protected by API key."""
    return _auth_guard.should_protect_http_path(path)


async def _require_http_api_key(request: Request) -> None:
    """Enforce API key for protected HTTP routes."""
    await _auth_guard.require_http(request)


async def _require_ws_api_key(websocket: WebSocket) -> bool:
    """Enforce API key for websocket handshake."""
    ok, _ = await _auth_guard.require_ws(websocket)
    return ok


class MigrationRequest(BaseModel):
    source_code: str
    migration_type: str = "cobol-to-java"


class ApprovalRequest(BaseModel):
    approved: bool
    comment: str | None = None


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    return await _auth_guard.http_middleware(request, call_next)


@app.get("/api/health")
async def health_check() -> dict[str, str]:
    """ヘルスチェック.

    platform のヘルスチェック判定基準と統一した応答フォーマット。
    """
    return {"status": "healthy", "service": "code_migration_assistant"}


async def run_migration_task(task_id: str, engine: CodeMigrationEngine, inputs: dict):
    """Background task to run the engine and stream events via WebSocket."""
    logger.info(f"Starting migration task {task_id}")
    try:
        # Flow Start Event
        if task_id in task_websockets:
            flow_start = FlowStartEvent(timestamp=time.time(), flow_id=task_id, data={"inputs": inputs})
            await task_websockets[task_id].send_json(flow_start.to_dict())

        async for event in engine._execute_stream(inputs):
            # Send event to websocket if connected
            if task_id in task_websockets:
                ws = task_websockets[task_id]
                try:
                    # Check if event is already a dict compatible with AGUIEvent
                    # or if it's an engine internal event that needs conversion

                    agui_event = None
                    event_type = event.get("event") or event.get("event_type")

                    if event_type == "node_start":
                        agui_event = NodeStartEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            node_id=event.get("node", "unknown"),
                            node_name=event.get("node", "unknown"),
                        )
                    elif event_type == "node_complete":
                        agui_event = NodeCompleteEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            node_id=event.get("node", "unknown"),
                            node_name=event.get("node", "unknown"),
                            data=event.get("result", {}),
                        )
                    elif event_type == "log":
                        agui_event = LogEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            level=event.get("level", "INFO"),
                            message=event.get("message", ""),
                        )
                    elif event_type == "approval_required":
                        pass

                    if agui_event:
                        await ws.send_json(agui_event.to_dict())
                    else:
                        pass

                except Exception as e:
                    logger.exception(f"WebSocket send error: {e}")

        # Flow Complete Event
        if task_id in task_websockets:
            flow_complete = FlowCompleteEvent(
                timestamp=time.time(),
                flow_id=task_id,
                result={
                    "report_available": True,
                    "report_url": f"/api/artifacts/{task_id}/report/compliance_report.md",
                },
            )
            await task_websockets[task_id].send_json(flow_complete.to_dict())

    except Exception as e:
        logger.error(f"Migration task failed: {e}", exc_info=True)
        if task_id in task_websockets:
            flow_error = FlowErrorEvent(
                timestamp=time.time(),
                flow_id=task_id,
                error_message=str(e),
                error_type=type(e).__name__,
            )
            await task_websockets[task_id].send_json(flow_error.to_dict())

    finally:
        # Cleanup
        if task_id in active_tasks:
            # Don't remove immediately so user can see result?
            # For this demo, keep it.
            pass


@app.post("/api/migration/start")
async def start_migration(request: MigrationRequest, background_tasks: BackgroundTasks):
    task_id = str(uuid.uuid4())

    # Initialize Engine
    engine = CodeMigrationEngine(migration_type=request.migration_type)

    # Inject an event emitter that writes to WebSocket for ApprovalFlow?
    # ApprovalFlow supports event_emitter callback.
    async def ws_emitter(event_dict: dict):
        if task_id in task_websockets:
            await task_websockets[task_id].send_json(event_dict)

    engine._approval_flow._event_emitter = ws_emitter

    await engine._initialize()

    active_tasks[task_id] = engine

    inputs = {
        "source_code": request.source_code,
        "task_id": task_id,
        "artifacts_dir": f"/tmp/migration_artifacts/{task_id}",  # Temp dir for demo
    }

    # Start loop in background
    background_tasks.add_task(run_migration_task, task_id, engine, inputs)

    return {"task_id": task_id, "status": "started"}


@app.websocket("/api/ws/{task_id}")
async def websocket_endpoint(websocket: WebSocket, task_id: str):
    if not await _require_ws_api_key(websocket):
        return
    await websocket.accept()
    task_websockets[task_id] = websocket
    try:
        while True:
            # Keep alive / listen for client messages (optional)
            await websocket.receive_text()
            # Echo or handle commands
    except WebSocketDisconnect:
        logger.info(f"Client disconnected for task {task_id}")
        task_websockets.pop(task_id, None)


@app.get("/api/approvals/{task_id}")
async def get_approvals(task_id: str):
    if task_id not in active_tasks:
        raise HTTPException(status_code=404, detail="Task not found")

    engine = active_tasks[task_id]
    # Check pending approvals
    pending = engine._approval_flow.get_pending_requests()
    return [{"id": r.id, "action": r.action, "reason": r.reason, "context": r.context} for r in pending]


@app.post("/api/approvals/{task_id}/{request_id}")
async def submit_approval(task_id: str, request_id: str, approval: ApprovalRequest):
    if task_id not in active_tasks:
        raise HTTPException(status_code=404, detail="Task not found")

    engine = active_tasks[task_id]
    success = await engine._approval_flow.submit_response(
        request_id=request_id,
        approved=approval.approved,
        comment=approval.comment,
        approver="admin",
    )

    if not success:
        raise HTTPException(status_code=400, detail="Failed to submit approval (invalid ID or timeout)")

    return {"status": "submitted"}


@app.get("/api/artifacts/{task_id}/{stage}/{filename}")
async def get_artifact(task_id: str, stage: str, filename: str):
    """成果物を取得."""
    # Note: In production, validate task_id and use a secure storage
    path = Path(f"/tmp/migration_artifacts/{task_id}/{stage}/{filename}")
    if not path.exists():
        raise HTTPException(status_code=404, detail="Artifact not found")
    return FileResponse(path)


# Serve UI
app.mount("/", StaticFiles(directory="apps/code_migration_assistant/frontend", html=True), name="ui")

if __name__ == "__main__":
    import json

    import uvicorn

    config_path = Path(__file__).resolve().parents[1] / "app_config.json"
    config_raw: dict = {}
    if config_path.is_file():
        try:
            config_raw = json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError:
            config_raw = {}

    api_port = config_raw.get("ports", {}).get("api", 8003)
    uvicorn.run(app, host="0.0.0.0", port=int(api_port))
