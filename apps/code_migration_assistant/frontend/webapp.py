import json
import logging
import os
import uuid
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.engine import CodeMigrationEngine
from fastapi import BackgroundTasks, FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel


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
_AUTH_HEADER = "x-api-key"
_WS_AUTH_QUERY_KEY = "api_key"
_PUBLIC_HTTP_PATHS = {"/api/health", "/docs", "/redoc", "/openapi.json"}

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
    if not _APP_CONFIG_PATH.is_file():
        return {}
    try:
        return json.loads(_APP_CONFIG_PATH.read_text("utf-8"))
    except json.JSONDecodeError:
        return {}


def _get_auth_contract() -> dict[str, Any]:
    """Return contracts.auth from app config."""
    raw = _load_app_config()
    contracts = raw.get("contracts", {})
    if not isinstance(contracts, dict):
        return {}
    auth = contracts.get("auth", {})
    if not isinstance(auth, dict):
        return {}
    return auth


def _is_auth_required() -> bool:
    """Evaluate whether API key auth must be enforced."""
    auth = _get_auth_contract()
    enabled = bool(auth.get("enabled", False))
    allow_anonymous = bool(auth.get("allow_anonymous", True))
    return enabled and not allow_anonymous


def _api_key_env_name() -> str:
    """Return API key env var name."""
    return os.getenv("CODE_MIGRATION_API_KEY_ENV", "CODE_MIGRATION_API_KEY")


def _verify_api_key(incoming_key: str | None) -> None:
    """Validate API key when auth is required."""
    if not _is_auth_required():
        return

    env_name = _api_key_env_name()
    expected_key = os.getenv(env_name)
    if not expected_key:
        raise HTTPException(
            status_code=503,
            detail=f"Auth required but env '{env_name}' is not configured",
        )

    if incoming_key != expected_key:
        raise HTTPException(status_code=401, detail="Invalid API key")


def _should_protect_http_path(path: str) -> bool:
    """Return whether HTTP path should be protected by API key."""
    return path.startswith("/api/") and path not in _PUBLIC_HTTP_PATHS


def _require_http_api_key(request: Request) -> None:
    """Enforce API key for protected HTTP routes."""
    if not _should_protect_http_path(request.url.path):
        return
    _verify_api_key(request.headers.get(_AUTH_HEADER))


async def _require_ws_api_key(websocket: WebSocket) -> bool:
    """Enforce API key for websocket handshake."""
    if not _is_auth_required():
        return True
    incoming_key = websocket.headers.get(_AUTH_HEADER) or websocket.query_params.get(_WS_AUTH_QUERY_KEY)
    try:
        _verify_api_key(incoming_key)
    except HTTPException as exc:
        close_code = 4401 if exc.status_code == 401 else 1011
        await websocket.close(code=close_code, reason=str(exc.detail))
        return False
    return True


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
    try:
        _require_http_api_key(request)
    except HTTPException as exc:
        return JSONResponse({"detail": exc.detail}, status_code=exc.status_code)
    return await call_next(request)


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
