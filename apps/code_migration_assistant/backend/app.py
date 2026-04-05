"""Code Migration Assistant — FastAPI アプリケーション定義（ルーティングのみ）."""

from __future__ import annotations

import logging
import os
import uuid
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any

from fastapi import BackgroundTasks, FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles

from apps.code_migration_assistant.backend.command_handler import (
    apply_local_command,
    dispatch_remote_command,
    resolve_distributed_state,
)
from apps.code_migration_assistant.backend.event_bus import emit_event, sync_runtime_state
from apps.code_migration_assistant.backend.knowledge_router import router as knowledge_router
from apps.code_migration_assistant.backend.migration_router import router as migration_ui_router
from apps.code_migration_assistant.backend.models import (
    ApprovalDecisionRequest,
    MigrationRequest,
    TaskCommandRequest,
)
from apps.code_migration_assistant.backend.task_runtime import (
    CONTRACT_VERSION,
    TaskRuntime,
    active_tasks,
    build_execution_inputs,
    collect_pending_approvals,
    local_instance_id,
    task_websockets,
)
from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.runtime_env import load_code_migration_env
from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from infrastructure.observability.startup import log_startup_info
from shared.config.manifest import load_app_manifest

if TYPE_CHECKING:
    from collections.abc import AsyncIterator

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("migration_server")

_APP_ROOT = Path(__file__).resolve().parents[1]
load_code_migration_env(_APP_ROOT)


# ---------------------------------------------------------------------------
# 設定ユーティリティ
# ---------------------------------------------------------------------------

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


def _build_startup_runtime_overrides() -> dict[str, Any]:
    """Code Migration Assistant の runtime 上書き値を構築する."""
    knowledge_db_path = _APP_ROOT / "data" / "knowledge.db"
    return {
        "db": {
            "backend": "sqlite",
            "url": f"sqlite+aiosqlite:///{knowledge_db_path}",
        }
    }


# ---------------------------------------------------------------------------
# Auth contract
# ---------------------------------------------------------------------------

_APP_CONFIG_PATH = Path(__file__).resolve().parents[1] / "app_config.json"
_PUBLIC_HTTP_PATHS = {"/api/health", "/api/auth/dev-info", "/docs", "/redoc", "/openapi.json"}
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

_cors_origins = _resolve_cors_origins()
_cors_allow_credentials = not (len(_cors_origins) == 1 and _cors_origins[0] == "*")


# ---------------------------------------------------------------------------
# 知識ベース初期化
# ---------------------------------------------------------------------------

_knowledge_initialized = False


async def _init_knowledge_managers() -> None:
    """RAG 管理テーブルとマネージャーを初期化."""
    global _knowledge_initialized  # noqa: PLW0603
    if _knowledge_initialized:
        return
    try:
        from apps.code_migration_assistant.backend.knowledge_db import (
            get_knowledge_session_factory,
            init_knowledge_db,
        )
        from apps.code_migration_assistant.backend.knowledge_router import init_managers
        from shared.rag.collection_manager import CollectionManager
        from shared.rag.document_manager import DocumentManager

        await init_knowledge_db()
        session_factory = get_knowledge_session_factory()

        col_mgr = CollectionManager(session_factory=session_factory)
        doc_mgr = DocumentManager(collection_manager=col_mgr, session_factory=session_factory)
        init_managers(col_mgr, doc_mgr)
        _knowledge_initialized = True
        logger.info("Knowledge managers initialized")
    except Exception as e:
        logger.warning("Knowledge manager init failed (non-critical): %s", e)


# ---------------------------------------------------------------------------
# Lifespan
# ---------------------------------------------------------------------------

@asynccontextmanager
async def _lifespan(_app: FastAPI) -> AsyncIterator[None]:
    await _init_knowledge_managers()
    app_config = _auth_guard.load_app_config()
    app_config_path = _APP_ROOT / "app_config.json"
    log_startup_info(
        app_name=str(app_config.get("display_name") or "Code Migration Assistant API"),
        app_config_path=app_config_path,
        runtime_overrides=_build_startup_runtime_overrides(),
        extra_info={
            "version": str(app_config.get("version") or "1.0.0"),
        },
    )
    yield


# ---------------------------------------------------------------------------
# FastAPI app
# ---------------------------------------------------------------------------

app = FastAPI(title="Code Migration Assistant API", lifespan=_lifespan)

app.add_middleware(
    CORSMiddleware,
    allow_origins=_cors_origins,
    allow_credentials=_cors_allow_credentials,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.middleware("http")
async def cache_control_middleware(request: Request, call_next: Any) -> Any:
    """Cache-Control ヘッダーを設定するミドルウェア."""
    response = await call_next(request)

    # HTMLファイルに対してキャッシュを無効化
    path = request.url.path
    if path.endswith(".html") or path == "/" or path == "":
        response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
        response.headers["Pragma"] = "no-cache"
        response.headers["Expires"] = "0"

    return response


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    return await _auth_guard.http_middleware(request, call_next)


# ---------------------------------------------------------------------------
# Routers
# ---------------------------------------------------------------------------

app.include_router(migration_ui_router)
app.include_router(knowledge_router)


# ---------------------------------------------------------------------------
# Migration task lifecycle
# ---------------------------------------------------------------------------
# 注意: イベント形式の二重体系について
#   - /api/migrate/* (migration_router.py SSE) → stage_start / stage_complete / hitl_required 等
#     フロントエンド (app.js) はこの形式を消費する。
#   - /api/ws/* (本ファイル WebSocket) → AG-UI 形式 (flow.start / node.start 等)
#     CLI・外部統合クライアント向け。
# ---------------------------------------------------------------------------

async def run_migration_task(task_id: str, runtime: TaskRuntime) -> None:
    """Migration Engine をバックグラウンド実行する."""
    await emit_event(task_id, {"event_type": "flow.start", "status": "started"})

    try:
        async for raw_event in runtime.engine.execute_stream(runtime.inputs):
            await emit_event(task_id, raw_event)

            event_name = str(raw_event.get("event_type") or raw_event.get("event") or "")
            node_id = str(raw_event.get("node_name") or raw_event.get("node") or "")
            if event_name in {"node.complete", "node_complete"} and node_id == "migration_pipeline":
                result_obj = raw_event.get("data") or raw_event.get("result")
                if isinstance(result_obj, dict):
                    runtime.result = result_obj

        runtime.status = "complete"
        await emit_event(
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
        await emit_event(
            task_id,
            {
                "event_type": "flow.error",
                "status": "error",
                "error_message": str(exc),
                "error_type": type(exc).__name__,
            },
        )
    finally:
        await sync_runtime_state(runtime)


def _resolve_artifacts_dir(task_id: str) -> Path:
    """成果物ディレクトリを設定ベースで解決する."""
    base = os.getenv("CODE_MIGRATION_ARTIFACTS_DIR", "").strip()
    if not base:
        base = str(_APP_ROOT / "data" / "artifacts")
    return Path(base) / task_id


async def _start_task(body: MigrationRequest, background_tasks: BackgroundTasks) -> dict[str, Any]:
    """共通タスク起動処理."""
    task_id = str(uuid.uuid4())
    engine = CodeMigrationEngine(migration_type=body.migration_type)
    await engine.initialize()

    artifacts_dir = _resolve_artifacts_dir(task_id)
    runtime = TaskRuntime(
        task_id=task_id,
        engine=engine,
        inputs=build_execution_inputs(
            task_id=task_id,
            source_code=body.source_code,
            migration_type=body.migration_type,
            module=body.module,
            verification_mode=body.options.verification_mode,
            expected_outputs=body.expected_outputs,
            artifacts_dir=artifacts_dir,
        ),
        flow_context=None,
        artifacts_dir=artifacts_dir,
        status="running",
    )

    active_tasks[task_id] = runtime
    await sync_runtime_state(runtime)

    background_tasks.add_task(run_migration_task, task_id, runtime)

    return {
        "task_id": task_id,
        "status": "started",
        "ws_url": f"/api/ws/{task_id}",
    }


# ---------------------------------------------------------------------------
# Health
# ---------------------------------------------------------------------------

@app.get("/api/health")
async def health_check() -> dict[str, str]:
    """ヘルスチェック."""
    return {"status": "healthy", "service": "code_migration_assistant"}


@app.get("/api/auth/dev-info")
async def dev_auth_info() -> dict[str, Any]:
    """開発用キー情報を返す（自動生成モード時のみキーを公開）.

    本番では CODE_MIGRATION_AUTO_API_KEY=false にすることで
    このエンドポイントはキーを返さなくなる。
    """
    auto_flag = os.getenv("CODE_MIGRATION_AUTO_API_KEY", "true").strip().lower()
    is_auto_mode = auto_flag in {"true", "1", "yes"}
    auth_required = _auth_guard.is_auth_required()

    if is_auto_mode and auth_required:
        return {
            "auth_required": True,
            "auto_key_mode": True,
            "api_key": os.getenv("CODE_MIGRATION_API_KEY", ""),
        }
    return {
        "auth_required": auth_required,
        "auto_key_mode": is_auto_mode,
    }


# ---------------------------------------------------------------------------
# Legacy migration endpoints（deprecated — /api/migrate/* を推奨）
# ---------------------------------------------------------------------------

@app.post("/api/migration/start", deprecated=True)
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


@app.post("/api/migration/execute", deprecated=True)
async def execute_migration(
    request: Request,
    body: MigrationRequest,
    background_tasks: BackgroundTasks,
) -> dict[str, Any]:
    """推奨の移行実行エンドポイント（deprecated — /api/migrate/upload を使用）."""
    del request
    return await _start_task(body, background_tasks)


@app.get("/api/migration/{task_id}/state")
async def get_migration_state(task_id: str) -> dict[str, Any]:
    """タスク状態を返す（ローカル優先、次に分散ストア）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return runtime.to_state(owner_instance=local_instance_id())

    distributed_state = await resolve_distributed_state(task_id)
    if distributed_state is not None:
        return distributed_state

    raise HTTPException(status_code=404, detail="Task not found")


@app.post("/api/migration/{task_id}/commands")
async def post_migration_command(task_id: str, command: TaskCommandRequest) -> dict[str, Any]:
    """ヒューマンコマンドを受け付ける（owner 判定つき）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return await apply_local_command(task_id, runtime, command)

    distributed_state = await resolve_distributed_state(task_id)
    if distributed_state is None:
        raise HTTPException(status_code=404, detail="Task not found")

    owner_instance = distributed_state.get("owner_instance")
    if isinstance(owner_instance, str) and owner_instance and owner_instance != local_instance_id():
        return await dispatch_remote_command(task_id, command)

    raise HTTPException(status_code=409, detail="task is owned by another process")


@app.websocket("/api/ws/{task_id}")
async def websocket_endpoint(websocket: WebSocket, task_id: str) -> None:
    """イベントストリーム WebSocket エンドポイント."""
    ok, _ = await _auth_guard.require_ws(websocket)
    if not ok:
        return

    if websocket.client_state.name != "CONNECTED":
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


@app.get("/api/approvals/{task_id}", deprecated=True)
async def get_approvals(task_id: str) -> list[dict[str, Any]]:
    """保留中承認一覧（legacy endpoint）."""
    runtime = active_tasks.get(task_id)
    if runtime is not None:
        return collect_pending_approvals(runtime.engine)

    distributed_state = await resolve_distributed_state(task_id)
    if distributed_state is not None:
        pending = distributed_state.get("pending_approvals", [])
        if isinstance(pending, list):
            return [item for item in pending if isinstance(item, dict)]

    raise HTTPException(status_code=404, detail="Task not found")


@app.post("/api/approvals/{task_id}/{request_id}", deprecated=True)
async def submit_approval(task_id: str, request_id: str, approval: ApprovalDecisionRequest) -> dict[str, Any]:
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
    """成果物を取得する."""
    path = _resolve_artifacts_dir(task_id).parent / task_id / stage / filename
    if not path.exists():
        raise HTTPException(status_code=404, detail="Artifact not found")
    return FileResponse(path)


# ---------------------------------------------------------------------------
# Static frontend
# ---------------------------------------------------------------------------

app.mount("/", StaticFiles(directory="apps/code_migration_assistant/frontend", html=True), name="ui")


# ---------------------------------------------------------------------------
# Standalone entrypoint
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import uvicorn

    config_path = Path(__file__).resolve().parents[1] / "app_config.json"
    api_port = 8003
    if config_path.is_file():
        try:
            manifest = load_app_manifest(config_path)
            if manifest.ports.api is not None:
                api_port = manifest.ports.api
        except ValueError:
            api_port = 8003

    uvicorn.run(app, host="0.0.0.0", port=int(api_port))
