"""FastAPI entry point for the Legacy Modernization GEO Platform."""

from __future__ import annotations

import argparse
import asyncio
import json
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Callable


load_dotenv: Callable[..., bool] | None
try:
    from dotenv import load_dotenv as _load_dotenv
except ImportError:  # pragma: no cover - optional dependency
    load_dotenv = None
else:
    load_dotenv = _load_dotenv

import uvicorn
from fastapi import FastAPI, HTTPException, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, JSONResponse, PlainTextResponse

from apps.legacy_modernization_geo_platform.backend.orchestrator import GeoOrchestrator
from apps.legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate
from apps.legacy_modernization_geo_platform.backend.repository import GeoRepository
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ApprovalRecord,
    ApprovalRequest,
    ApprovalStatus,
    GeoExecuteRequest,
    TaskCommandRequest,
    TaskEvent,
)
from apps.legacy_modernization_geo_platform.backend.settings import APP_ROOT, GeoPlatformSettings
from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from infrastructure.observability.startup import log_startup_info
from kernel.agents.app_agent_runtime import bootstrap_app_agents
from kernel.protocols.a2ui.components import A2UIComponent, CardComponent, TextComponent
from kernel.protocols.agui_events import (
    A2UIClearEvent,
    A2UIComponentEvent,
    A2UIUpdateEvent,
    AGUIEvent,
    ApprovalRequiredEvent,
    ApprovalSubmittedEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeStartEvent,
)
from shared.integrations.fastapi_integration import AgentContractRouter, create_sse_response


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable

    from infrastructure.security.auth_client import AuthClient


_APP_ENV_PATH = APP_ROOT / ".env"
_APP_CONFIG_PATH = APP_ROOT / "app_config.json"
_PUBLIC_HTTP_PATHS = {
    "/api/health",
}
_A2UI_SURFACES = ("workspace", "content", "approval", "report")

if load_dotenv is not None and _APP_ENV_PATH.is_file():
    load_dotenv(_APP_ENV_PATH, override=True)


def create_app(
    settings: GeoPlatformSettings | None = None,
    *,
    auth_client_factory: Callable[..., AuthClient] | None = None,
) -> FastAPI:
    """Create the FastAPI application with injected settings."""
    resolved_settings = settings or GeoPlatformSettings.from_env(app_root=APP_ROOT)
    repository = GeoRepository(resolved_settings.db_path)
    quality_gate = GeoQualityGate()
    publisher = GeoPublisher(resolved_settings)
    agent_runtime = bootstrap_app_agents(
        _APP_CONFIG_PATH,
        agent_init_overrides={
            "GeoQA": {"quality_gate": quality_gate},
            "Publishing": {"publisher": publisher},
        },
    )
    orchestrator = GeoOrchestrator(
        settings=resolved_settings,
        repository=repository,
        quality_gate=quality_gate,
        publisher=publisher,
        agent_runtime=agent_runtime,
    )
    auth_guard = ContractAuthGuard(
        ContractAuthGuardConfig(
            app_config_path=_APP_CONFIG_PATH,
            public_http_paths=_PUBLIC_HTTP_PATHS,
            auth_header_name="authorization",
            ws_query_key="access_token",
            browser_token_query_key="access_token",
            default_api_key_env_var="GEO_PLATFORM_API_KEY",
        ),
        auth_client_factory=auth_client_factory,
    )

    @asynccontextmanager
    async def lifespan(_app: FastAPI) -> AsyncIterator[None]:
        """起動時に統一 startup summary を出力する."""
        log_startup_info(
            app_name="Legacy Modernization GEO Platform",
            app_config_path=_APP_CONFIG_PATH,
            runtime_overrides={
                "db": {
                    "backend": "sqlite",
                    "url": f"sqlite:///{resolved_settings.db_path}",
                },
                "vectordb": {
                    "backend": "",
                    "path": "",
                    "collection": "",
                    "index": "",
                },
            },
        )
        yield

    app = FastAPI(
        title="Legacy Modernization GEO Platform",
        version="0.1.0",
        description="Demand capture, content planning, approval, and publishing for legacy modernization GEO.",
        lifespan=lifespan,
    )
    app.state.settings = resolved_settings
    app.state.repository = repository
    app.state.orchestrator = orchestrator
    app.state.agent_runtime = agent_runtime
    app.state.contract_auth_guard = auth_guard

    app.add_middleware(
        CORSMiddleware,
        allow_origins=resolved_settings.cors_origins or ["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    app.middleware("http")(auth_guard.http_middleware)

    agent_router = AgentContractRouter(agent_runtime)
    if agent_router is not None:
        app.include_router(agent_router, prefix="/api")

    @app.get("/api/health")
    async def health() -> dict[str, str]:
        """Return a minimal health response."""
        return {"status": "ok", "app": "legacy_modernization_geo_platform"}

    @app.get("/api/a2a/agents")
    async def list_a2a_agents() -> list[dict[str, Any]]:
        """A2AHub 登録済み Agent の AgentCard 一覧を返す."""
        from kernel.protocols.a2a_hub import get_hub

        hub = get_hub()
        return [card.to_a2a_format() for card in hub.list_agents()]

    @app.post("/api/geo/execute")
    async def execute_geo(body: GeoExecuteRequest) -> dict[str, Any]:
        """Start a new GEO execution."""
        task_id = await orchestrator.start(body)
        return {
            "task_id": task_id,
            "status": "started",
            "stream_url": f"/api/geo/{task_id}/stream",
            "ws_url": f"/api/ws/{task_id}",
        }

    @app.get("/api/geo/{task_id}/stream")
    async def stream_geo(task_id: str) -> Any:
        """Stream canonical AG-UI / A2UI events over SSE."""
        if orchestrator.get_state(task_id) is None:
            raise HTTPException(status_code=404, detail="Task not found")
        return create_sse_response(_geo_event_stream(orchestrator, task_id))

    @app.get("/api/geo/{task_id}/state")
    async def get_state(task_id: str) -> JSONResponse:
        """Return the current task state."""
        state = orchestrator.get_state(task_id)
        if state is None:
            raise HTTPException(status_code=404, detail="Task not found")
        return JSONResponse(content=state.model_dump(mode="json"))

    @app.post("/api/geo/{task_id}/commands")
    async def post_command(
        task_id: str,
        body: TaskCommandRequest,
    ) -> dict[str, Any]:
        """Apply an operator command to a task."""
        try:
            return await orchestrator.apply_command(task_id, body)
        except KeyError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

    @app.post("/api/geo/{task_id}/approval")
    async def post_approval(
        task_id: str,
        body: ApprovalRequest,
        request_id: str | None = None,
    ) -> JSONResponse:
        """Submit an approval decision for the latest pending request."""
        selected_request_id = request_id or _latest_pending_approval_id(orchestrator.get_state(task_id))
        if selected_request_id is None:
            raise HTTPException(status_code=404, detail="No pending approval found")
        record = await orchestrator.submit_approval(task_id, selected_request_id, body)
        return JSONResponse(content=record.model_dump(mode="json"))

    @app.get("/api/geo/{task_id}/artifacts/{artifact_name}")
    async def get_artifact(task_id: str, artifact_name: str) -> JSONResponse:
        """Return the latest artifact payload by name."""
        state = orchestrator.get_state(task_id)
        if state is None:
            raise HTTPException(status_code=404, detail="Task not found")
        record = next((item for item in reversed(state.artifacts) if item.artifact_name == artifact_name), None)
        if record is None:
            raise HTTPException(status_code=404, detail="Artifact not found")
        artifact_path = Path(record.path)
        if not await _path_is_file(artifact_path):
            raise HTTPException(status_code=404, detail="Artifact file missing")
        if artifact_path.suffix == ".json":
            return JSONResponse(content=json.loads(await _path_read_text(artifact_path)))
        return JSONResponse(content={"content": await _path_read_text(artifact_path)})

    @app.get("/api/geo/{task_id}/report")
    async def get_report(task_id: str) -> JSONResponse:
        """Return the stored campaign report."""
        state = orchestrator.get_state(task_id)
        if state is None or state.report is None:
            raise HTTPException(status_code=404, detail="Report not found")
        return JSONResponse(content=state.report.model_dump(mode="json"))

    @app.websocket("/api/ws/{task_id}")
    async def websocket_events(websocket: WebSocket, task_id: str) -> None:
        """Compatibility WebSocket stream. SSE is the preferred path."""
        ok, _principal = await auth_guard.require_ws(websocket)
        if not ok:
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
        if not await _path_is_file(html_path):
            raise HTTPException(status_code=404, detail="Published page file missing")
        return FileResponse(str(html_path), media_type="text/html; charset=utf-8")

    @app.get("/geo/sitemap.xml")
    async def sitemap() -> PlainTextResponse:
        """Serve the generated sitemap."""
        sitemap_path = resolved_settings.published_dir / "sitemap.xml"
        if not await _path_is_file(sitemap_path):
            raise HTTPException(status_code=404, detail="Sitemap not found")
        return PlainTextResponse(await _path_read_text(sitemap_path), media_type="application/xml")

    @app.get("/geo/ai-feed.json")
    async def ai_feed() -> JSONResponse:
        """Serve the generated AI feed."""
        ai_feed_path = resolved_settings.published_dir / "ai-feed.json"
        if not await _path_is_file(ai_feed_path):
            raise HTTPException(status_code=404, detail="AI feed not found")
        return JSONResponse(content=json.loads(await _path_read_text(ai_feed_path)))

    @app.get("/", include_in_schema=False, response_model=None)
    @app.get("/{full_path:path}", include_in_schema=False, response_model=None)
    async def spa(full_path: str = "") -> Any:
        """Serve the built SPA if available."""
        dist_dir = resolved_settings.frontend_dist_dir
        if not await _path_is_dir(dist_dir):
            return JSONResponse(
                status_code=404,
                content={"detail": "Frontend not built. Run the frontend build first."},
            )
        asset_path = await _path_resolve(dist_dir / full_path)
        if full_path and await _path_is_file(asset_path) and dist_dir in asset_path.parents:
            return FileResponse(str(asset_path))
        index_path = dist_dir / "index.html"
        if await _path_is_file(index_path):
            return FileResponse(str(index_path))
        return JSONResponse(status_code=404, content={"detail": "Frontend index missing"})

    return app


async def _geo_event_stream(orchestrator: GeoOrchestrator, task_id: str) -> AsyncIterator[AGUIEvent]:
    """Translate persisted GEO task events into canonical AG-UI / A2UI events."""
    async for task_event in orchestrator.stream_events(task_id):
        for event in _task_event_to_agui_events(task_event):
            yield event


def _task_event_to_agui_events(task_event: TaskEvent) -> list[AGUIEvent]:
    """Map persisted task events to canonical AG-UI / A2UI events."""
    base_data = _build_event_data(task_event)
    flow_id = task_event.task_id
    event_type = task_event.event_type
    node_id = task_event.agent or task_event.stage or task_event.task_id
    node_name = task_event.agent or task_event.stage or task_event.task_id
    message = task_event.message or event_type

    if event_type == "flow.start":
        events: list[AGUIEvent] = [
            FlowStartEvent(timestamp=task_event.timestamp.timestamp(), flow_id=flow_id, data=base_data),
        ]
        for surface_id in _A2UI_SURFACES:
            events.append(
                A2UIClearEvent(
                    timestamp=task_event.timestamp.timestamp(),
                    flow_id=flow_id,
                    surface_id=surface_id,
                    data=base_data,
                )
            )
        return events

    if event_type == "node.start":
        return [
            NodeStartEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                node_id=node_id,
                node_name=node_name,
                data=base_data,
            )
        ]

    if event_type == "node.complete":
        return [
            NodeCompleteEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                node_id=node_id,
                node_name=node_name,
                data=base_data,
            )
        ]

    if event_type == "flow.error":
        return [
            FlowErrorEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                error_message=message,
                error_type=str(task_event.payload.get("error_type", "GeoTaskError")),
                data=base_data,
            )
        ]

    if event_type == "flow.complete":
        return [
            FlowCompleteEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                result=base_data,
                include_result=True,
                data=base_data,
            )
        ]

    if event_type == "approval_required":
        actions = task_event.payload.get("actions")
        raw_actions = actions if isinstance(actions, list) else []
        options = [{"id": str(action), "label": str(action)} for action in raw_actions]
        return [
            ApprovalRequiredEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                request_id=str(task_event.payload.get("request_id", flow_id)),
                action="publish_review",
                reason=str(task_event.payload.get("reason", message)),
                risk_level=str(task_event.payload.get("risk_level", "normal")),
                context=base_data,
                options=options,
                data=base_data,
            )
        ]

    if event_type == "approval_submitted":
        status = str(task_event.payload.get("status", "")).lower()
        return [
            ApprovalSubmittedEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                request_id=str(task_event.payload.get("request_id", flow_id)),
                approved=status == ApprovalStatus.APPROVED.value,
                approver=str(task_event.payload.get("reviewer_name", "")) or None,
                comment=str(task_event.payload.get("comment", "")) or None,
                modifications=base_data,
                data=base_data,
            )
        ]

    if event_type in {"artifact.created", "artifact.updated"}:
        events = [
            LogEvent(
                timestamp=task_event.timestamp.timestamp(),
                flow_id=flow_id,
                level="INFO",
                message=message,
                source=node_name,
                data=base_data,
            )
        ]
        artifact_event = _artifact_task_event_to_a2ui(task_event, base_data)
        if artifact_event is not None:
            events.append(artifact_event)
        return events

    return [
        LogEvent(
            timestamp=task_event.timestamp.timestamp(),
            flow_id=flow_id,
            level="INFO",
            message=message,
            source=node_name,
            data=base_data,
        )
    ]


def _artifact_task_event_to_a2ui(task_event: TaskEvent, base_data: dict[str, Any]) -> AGUIEvent | None:
    """Translate artifact events into typed A2UI surface events."""
    artifact_name = str(task_event.payload.get("artifact_name", "")).strip()
    artifact_path = str(task_event.payload.get("artifact_path", "")).strip()
    if not artifact_name or not artifact_path:
        return None
    component = _build_artifact_component(
        task_id=task_event.task_id,
        artifact_name=artifact_name,
        artifact_path=Path(artifact_path),
    )
    if component is None:
        return None
    component_id = str(component.get("id", f"{task_event.task_id}:{artifact_name}"))
    surface_id = _artifact_surface_id(artifact_name)
    timestamp = task_event.timestamp.timestamp()

    if task_event.event_type == "artifact.updated":
        return A2UIUpdateEvent(
            timestamp=timestamp,
            flow_id=task_event.task_id,
            surface_id=surface_id,
            component_id=component_id,
            updates={"component": component},
            data=base_data,
        )

    return A2UIComponentEvent(
        timestamp=timestamp,
        flow_id=task_event.task_id,
        surface_id=surface_id,
        component=component,
        data=base_data,
    )


def _build_event_data(task_event: TaskEvent) -> dict[str, Any]:
    """Build supplemental event data payload."""
    data = dict(task_event.payload)
    data.setdefault("task_id", task_event.task_id)
    if task_event.stage is not None:
        data.setdefault("stage", task_event.stage)
    if task_event.agent is not None:
        data.setdefault("agent", task_event.agent)
    if task_event.message is not None:
        data.setdefault("message", task_event.message)
    return data


def _artifact_surface_id(artifact_name: str) -> str:
    """Map artifact names to UI surfaces."""
    if artifact_name in {"account_signal_artifact", "question_graph_artifact", "evidence_matrix"}:
        return "workspace"
    if artifact_name in {"content_blueprint_artifact", "content_draft_artifact"}:
        return "content"
    if artifact_name in {"geo_qa_report", "publish_manifest"}:
        return "approval"
    return "report"


def _build_artifact_component(
    *,
    task_id: str,
    artifact_name: str,
    artifact_path: Path,
) -> dict[str, Any] | None:
    """Build a card-style A2UI component from an artifact file."""
    if not artifact_path.is_file():
        return None
    payload = _load_artifact_payload(artifact_path)
    title = artifact_name.replace("_", " ").title()
    children: list[A2UIComponent] = [TextComponent(line) for line in _artifact_summary_lines(artifact_name, payload)]
    if not children:
        preview = (
            json.dumps(payload, ensure_ascii=False, indent=2) if isinstance(payload, (dict, list)) else str(payload)
        )
        children.append(TextComponent(preview[:1200]))
    card = CardComponent(title=title, children=children, artifact_name=artifact_name)
    card.id = f"{task_id}:{artifact_name}"
    return card.to_dict()


def _load_artifact_payload(artifact_path: Path) -> Any:
    """Load JSON/text artifact payload."""
    text = artifact_path.read_text(encoding="utf-8")
    if artifact_path.suffix != ".json":
        return text
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return text


def _artifact_summary_lines(artifact_name: str, payload: Any) -> list[str]:
    """Create concise summary lines for A2UI card rendering."""
    if not isinstance(payload, dict):
        return [str(payload)[:400]]

    if artifact_name == "account_signal_artifact":
        return [
            f"Company: {payload.get('company', '-')}",
            f"Urgency: {payload.get('urgency_hypothesis', '-')}",
            f"Fit score: {payload.get('modernization_fit_score', '-')}",
        ]
    if artifact_name == "question_graph_artifact":
        personas = payload.get("personas")
        persona_count = len(personas) if isinstance(personas, list) else 0
        return [f"Personas: {persona_count}"]
    if artifact_name == "evidence_matrix":
        entries = payload.get("entries")
        if isinstance(entries, list):
            return [
                f"Evidence rows: {len(entries)}",
                *[f"- {item.get('title', '-')}" for item in entries[:3] if isinstance(item, dict)],
            ]
    if artifact_name == "content_draft_artifact":
        pages = payload.get("pages")
        if isinstance(pages, list):
            return [
                f"Pages: {len(pages)}",
                *[f"- {item.get('title', '-')}" for item in pages[:3] if isinstance(item, dict)],
            ]
    if artifact_name == "geo_qa_report":
        return [
            f"Risk: {payload.get('risk_level', '-')}",
            f"Publish ready: {payload.get('publish_ready', False)}",
            *[f"- {item}" for item in payload.get("issues", [])[:3] if isinstance(item, str)],
        ]
    if artifact_name == "publish_manifest":
        pages = payload.get("pages")
        if isinstance(pages, list):
            return [
                f"Published pages: {len(pages)}",
                *[f"- {item.get('page_url', '-')}" for item in pages[:3] if isinstance(item, dict)],
            ]
    if artifact_name == "campaign_report":
        highlights = payload.get("highlights")
        if isinstance(highlights, list):
            return [
                f"Highlights: {len(highlights)}",
                *[f"- {item}" for item in highlights[:3] if isinstance(item, str)],
            ]

    preview = json.dumps(payload, ensure_ascii=False, indent=2)
    return [preview[:1200]]


async def _path_is_dir(path: Path) -> bool:
    """Check directory existence without blocking the event loop."""
    return await asyncio.to_thread(path.is_dir)


async def _path_is_file(path: Path) -> bool:
    """Check file existence without blocking the event loop."""
    return await asyncio.to_thread(path.is_file)


async def _path_read_text(path: Path) -> str:
    """Read a UTF-8 text file without blocking the event loop."""
    return await asyncio.to_thread(path.read_text, encoding="utf-8")


async def _path_resolve(path: Path) -> Path:
    """Resolve a filesystem path without blocking the event loop."""
    return await asyncio.to_thread(path.resolve)


def _latest_pending_approval_id(state: Any) -> str | None:
    """Resolve the latest pending approval id from task state."""
    if state is None:
        return None
    approvals = getattr(state, "approvals", [])
    for item in reversed(approvals):
        if isinstance(item, ApprovalRecord) and item.status == ApprovalStatus.PENDING:
            return item.request_id
    return None


def get_app() -> FastAPI:
    """遅延ファクトリ: import 時ではなく初回アクセス時に app を生成する."""
    global _app_instance
    if _app_instance is None:
        _app_instance = create_app()
    return _app_instance


_app_instance: FastAPI | None = None
app: FastAPI = None  # type: ignore[assignment]  # uvicorn が参照するエントリポイント


def __getattr__(name: str) -> Any:
    """モジュールレベル属性の遅延解決（uvicorn 互換）."""
    if name == "app":
        return get_app()
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


def main() -> None:
    """Uvicorn でアプリケーションを起動する."""
    default_settings = GeoPlatformSettings.from_env(app_root=APP_ROOT)
    parser = argparse.ArgumentParser(description="Run the Legacy Modernization GEO Platform")
    parser.add_argument("--host", default=default_settings.host)
    parser.add_argument("--port", type=int, default=default_settings.port)
    args = parser.parse_args()
    uvicorn.run("apps.legacy_modernization_geo_platform.main:app", host=args.host, port=args.port, reload=False)


if __name__ == "__main__":
    main()
