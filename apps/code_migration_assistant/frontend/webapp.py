import json
import logging
import os
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.engine import CodeMigrationEngine
from fastapi import BackgroundTasks, FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

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


async def run_migration_task(task_id: str, engine: CodeMigrationEngine, inputs: dict):
    """Background task to run the engine and stream events via WebSocket."""
    logger.info(f"Starting migration task {task_id}")
    try:
        async for event in engine._execute_stream(inputs):
            # Send event to websocket if connected
            if task_id in task_websockets:
                ws = task_websockets[task_id]
                try:
                    # Convert event to JSON-serializable dict
                    # Handling Pydantic models if present
                    payload = json.dumps(event, default=str)
                    await ws.send_text(payload)
                except Exception as e:
                    logger.exception(f"WebSocket send error: {e}")

            # 承認要求イベントの場合は特別なログを出すなど
            if event.get("event") == "node_complete":
                pass

    except Exception as e:
        logger.error(f"Migration task failed: {e}", exc_info=True)
        if task_id in task_websockets:
            await task_websockets[task_id].send_json({"error": str(e), "status": "failed"})
    finally:
        # Cleanup
        if task_id in active_tasks:
            # Don't remove immediately so user can see result?
            # For this demo, keep it.
            pass


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    return await _auth_guard.http_middleware(request, call_next)


@app.post("/api/migration/start")
async def start_migration(request: MigrationRequest, background_tasks: BackgroundTasks):
    task_id = str(uuid.uuid4())

    # Initialize Engine
    engine = CodeMigrationEngine(migration_type=request.migration_type)
    await engine._initialize()  # Must init

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
    if task_id not in active_tasks:
        await websocket.close(code=4004, reason="Task not found")
        return

    task_websockets[task_id] = websocket
    try:
        while True:
            # Keep alive / listen for client messages (optional)
            await websocket.receive_text()
            # Echo or handle commands
    except WebSocketDisconnect:
        logger.info(f"Client disconnected for task {task_id}")
        del task_websockets[task_id]


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


# Serve UI
app.mount("/", StaticFiles(directory="apps/code_migration_assistant/ui", html=True), name="ui")

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
