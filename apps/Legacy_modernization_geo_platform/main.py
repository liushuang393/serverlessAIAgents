"""FastAPI entry point for the Legacy Modernization GEO Platform."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
from typing import Any

try:
    from dotenv import load_dotenv
except ImportError:  # pragma: no cover - optional dependency
    load_dotenv = None

import uvicorn
from fastapi import FastAPI, HTTPException, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse, PlainTextResponse

from apps.Legacy_modernization_geo_platform.backend.orchestrator import GeoOrchestrator
from apps.Legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.Legacy_modernization_geo_platform.backend.repository import GeoRepository
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    ApprovalStatus,
    ApprovalRecord,
    ApprovalRequest,
    GeoExecuteRequest,
    TaskCommandRequest,
)
from apps.Legacy_modernization_geo_platform.backend.settings import APP_ROOT, GeoPlatformSettings


_APP_ENV_PATH = APP_ROOT / ".env"
if load_dotenv is not None and _APP_ENV_PATH.is_file():
    load_dotenv(_APP_ENV_PATH, override=True)


def create_app(settings: GeoPlatformSettings | None = None) -> FastAPI:
    """Create the FastAPI application with injected settings."""
    resolved_settings = settings or GeoPlatformSettings.from_env(app_root=APP_ROOT)
    repository = GeoRepository(resolved_settings.db_path)
    orchestrator = GeoOrchestrator(
        settings=resolved_settings,
        repository=repository,
        publisher=GeoPublisher(resolved_settings),
    )

    app = FastAPI(
        title="Legacy Modernization GEO Platform",
        version="0.1.0",
        description="Demand capture, content planning, approval, and publishing for legacy modernization GEO.",
    )
    app.state.settings = resolved_settings
    app.state.repository = repository
    app.state.orchestrator = orchestrator

    app.add_middleware(
        CORSMiddleware,
        allow_origins=resolved_settings.cors_origins or ["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    @app.get("/api/health")
    async def health() -> dict[str, str]:
        """Return a minimal health response."""
        return {"status": "ok", "app": "legacy_modernization_geo_platform"}

    @app.post("/api/geo/execute")
    async def execute_geo(request: Request, body: GeoExecuteRequest) -> dict[str, Any]:
        """Start a new GEO execution."""
        _require_api_key(request, resolved_settings)
        task_id = await orchestrator.start(body)
        return {
            "task_id": task_id,
            "status": "started",
            "ws_url": f"/api/ws/{task_id}",
        }

    @app.get("/api/geo/{task_id}/state")
    async def get_state(request: Request, task_id: str) -> JSONResponse:
        """Return the current task state."""
        _require_api_key(request, resolved_settings)
        state = orchestrator.get_state(task_id)
        if state is None:
            raise HTTPException(status_code=404, detail="Task not found")
        return JSONResponse(content=state.model_dump(mode="json"))

    @app.post("/api/geo/{task_id}/commands")
    async def post_command(
        request: Request,
        task_id: str,
        body: TaskCommandRequest,
    ) -> dict[str, Any]:
        """Apply an operator command to a task."""
        _require_api_key(request, resolved_settings)
        try:
            return await orchestrator.apply_command(task_id, body)
        except KeyError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

    @app.post("/api/geo/{task_id}/approval")
    async def post_approval(
        request: Request,
        task_id: str,
        body: ApprovalRequest,
        request_id: str | None = None,
    ) -> JSONResponse:
        """Submit an approval decision for the latest pending request."""
        _require_api_key(request, resolved_settings)
        selected_request_id = request_id or _latest_pending_approval_id(orchestrator.get_state(task_id))
        if selected_request_id is None:
            raise HTTPException(status_code=404, detail="No pending approval found")
        record = await orchestrator.submit_approval(task_id, selected_request_id, body)
        return JSONResponse(content=record.model_dump(mode="json"))

    @app.get("/api/geo/{task_id}/artifacts/{artifact_name}")
    async def get_artifact(request: Request, task_id: str, artifact_name: str) -> JSONResponse:
        """Return the latest artifact payload by name."""
        _require_api_key(request, resolved_settings)
        state = orchestrator.get_state(task_id)
        if state is None:
            raise HTTPException(status_code=404, detail="Task not found")
        record = next((item for item in reversed(state.artifacts) if item.artifact_name == artifact_name), None)
        if record is None:
            raise HTTPException(status_code=404, detail="Artifact not found")
        artifact_path = Path(record.path)
        if not artifact_path.is_file():
            raise HTTPException(status_code=404, detail="Artifact file missing")
        if artifact_path.suffix == ".json":
            return JSONResponse(content=json.loads(artifact_path.read_text(encoding="utf-8")))
        return JSONResponse(content={"content": artifact_path.read_text(encoding="utf-8")})

    @app.get("/api/geo/{task_id}/report")
    async def get_report(request: Request, task_id: str) -> JSONResponse:
        """Return the stored campaign report."""
        _require_api_key(request, resolved_settings)
        state = orchestrator.get_state(task_id)
        if state is None or state.report is None:
            raise HTTPException(status_code=404, detail="Report not found")
        return JSONResponse(content=state.report.model_dump(mode="json"))

    @app.websocket("/api/ws/{task_id}")
    async def websocket_events(websocket: WebSocket, task_id: str) -> None:
        """Stream execution events over WebSocket."""
        if not _require_ws_api_key(websocket, resolved_settings):
            await websocket.close(code=4401)
            return
        await orchestrator.connect_websocket(task_id, websocket)
        try:
            while True:
                await websocket.receive_text()
        except WebSocketDisconnect:
            await orchestrator.disconnect_websocket(task_id, websocket)

    @app.get("/geo/pages/{slug}")
    async def public_page(slug: str) -> FileResponse:
        """Serve a published static GEO page."""
        page = repository.get_published_page(slug)
        if page is None:
            raise HTTPException(status_code=404, detail="Published page not found")
        html_path = Path(page.html_path)
        if not html_path.is_file():
            raise HTTPException(status_code=404, detail="Published page file missing")
        return FileResponse(html_path, media_type="text/html; charset=utf-8")

    @app.get("/geo/sitemap.xml")
    async def sitemap() -> PlainTextResponse:
        """Serve the generated sitemap."""
        sitemap_path = resolved_settings.published_dir / "sitemap.xml"
        if not sitemap_path.is_file():
            raise HTTPException(status_code=404, detail="Sitemap not found")
        return PlainTextResponse(sitemap_path.read_text(encoding="utf-8"), media_type="application/xml")

    @app.get("/geo/ai-feed.json")
    async def ai_feed() -> JSONResponse:
        """Serve the generated AI feed."""
        ai_feed_path = resolved_settings.published_dir / "ai-feed.json"
        if not ai_feed_path.is_file():
            raise HTTPException(status_code=404, detail="AI feed not found")
        return JSONResponse(content=json.loads(ai_feed_path.read_text(encoding="utf-8")))

    @app.get("/", include_in_schema=False, response_model=None)
    @app.get("/{full_path:path}", include_in_schema=False, response_model=None)
    async def spa(full_path: str = ""):
        """Serve the built SPA if available."""
        dist_dir = resolved_settings.frontend_dist_dir
        if not dist_dir.is_dir():
            return JSONResponse(
                status_code=404,
                content={"detail": "Frontend not built. Run the frontend build first."},
            )
        asset_path = (dist_dir / full_path).resolve()
        if full_path and asset_path.is_file() and dist_dir in asset_path.parents:
            return FileResponse(asset_path)
        index_path = dist_dir / "index.html"
        if index_path.is_file():
            return FileResponse(index_path)
        return JSONResponse(status_code=404, content={"detail": "Frontend index missing"})

    return app


def _require_api_key(request: Request, settings: GeoPlatformSettings) -> None:
    """Validate API key if the application is configured to require one."""
    if not settings.api_key:
        return
    api_key = request.headers.get("x-api-key") or request.query_params.get("api_key")
    if api_key != settings.api_key:
        raise HTTPException(status_code=401, detail="Invalid API key")


def _require_ws_api_key(websocket: WebSocket, settings: GeoPlatformSettings) -> bool:
    """Validate API key for websocket requests."""
    if not settings.api_key:
        return True
    api_key = websocket.headers.get("x-api-key") or websocket.query_params.get("api_key")
    return api_key == settings.api_key


def _latest_pending_approval_id(state: Any) -> str | None:
    """Resolve the latest pending approval id from task state."""
    if state is None:
        return None
    approvals = getattr(state, "approvals", [])
    for item in reversed(approvals):
        if isinstance(item, ApprovalRecord) and item.status == ApprovalStatus.PENDING:
            return item.request_id
    return None


app = create_app()


def main() -> None:
    """Run the application via Uvicorn."""
    parser = argparse.ArgumentParser(description="Run the Legacy Modernization GEO Platform")
    parser.add_argument("--host", default=os.getenv("GEO_PLATFORM_HOST", "0.0.0.0"))
    parser.add_argument("--port", type=int, default=int(os.getenv("GEO_PLATFORM_PORT", "8010")))
    args = parser.parse_args()
    uvicorn.run("apps.Legacy_modernization_geo_platform.main:app", host=args.host, port=args.port, reload=False)


if __name__ == "__main__":
    main()
