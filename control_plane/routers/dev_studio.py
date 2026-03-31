"""dev_studio proxy と materialize API."""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import TYPE_CHECKING, Any, Literal

import httpx
from fastapi import APIRouter, HTTPException, Request, Response
from pydantic import BaseModel, Field

from control_plane.schemas.app_config_schemas import AppConfig
from control_plane.services.port_allocator import PortAllocatorService


if TYPE_CHECKING:
    from control_plane.services.app_discovery import AppDiscoveryService


router = APIRouter(prefix="/api/studios/framework/dev-studio", tags=["dev-studio"])

_discovery: AppDiscoveryService | None = None
_port_allocator: PortAllocatorService | None = None

_UPSTREAM_TIMEOUT = httpx.Timeout(60.0, connect=5.0)
_DEFAULT_DEV_STUDIO_URL = "http://127.0.0.1:8011"
_VALID_OUTPUT_TYPES = {"frontend", "backend", "fullstack"}


class MaterializeRequest(BaseModel):
    """生成ファイルを app として物化する."""

    name: str = Field(..., min_length=1, max_length=50, description="app 名")
    display_name: str | None = Field(default=None, description="表示名")
    description: str = Field(default="", description="説明")
    icon: str = Field(default="🧱", description="アイコン")
    template_id: str = Field(default="", description="テンプレート ID")
    goal: str = Field(default="", description="目標")
    spec_kind: Literal["agent", "system"] = Field(default="agent", description="仕様種別")
    spec: dict[str, Any] = Field(default_factory=dict, description="builder spec")
    generated_files: dict[str, str] = Field(default_factory=dict, description="生成ファイル")
    output_type: Literal["frontend", "backend", "fullstack"] = Field(
        default="fullstack",
        description="生成タイプ",
    )
    framework: str = Field(default="fastapi", description="フレームワーク名")


class MaterializeResponse(BaseModel):
    """物化レスポンス."""

    success: bool
    app_name: str
    app_dir: str
    config_path: str
    files_written: list[str]
    ports: dict[str, int | None]


def init_dev_studio_services(discovery: AppDiscoveryService) -> None:
    """サービス初期化."""
    global _discovery, _port_allocator
    _discovery = discovery
    _port_allocator = PortAllocatorService(discovery)


def _get_discovery() -> AppDiscoveryService:
    if _discovery is None:
        msg = "AppDiscoveryService が未初期化です"
        raise RuntimeError(msg)
    return _discovery


def _get_port_allocator() -> PortAllocatorService:
    if _port_allocator is None:
        msg = "PortAllocatorService が未初期化です"
        raise RuntimeError(msg)
    return _port_allocator


def _resolve_upstream_base() -> str:
    config = _get_discovery().get_app("dev_studio")
    if config is None:
        return _DEFAULT_DEV_STUDIO_URL

    backend = config.runtime.urls.backend
    if isinstance(backend, str) and backend.strip():
        return backend.rstrip("/")
    if config.ports.api is not None:
        return f"http://127.0.0.1:{config.ports.api}"
    return _DEFAULT_DEV_STUDIO_URL


def _normalize_app_name(name: str) -> str:
    normalized = re.sub(r"[^a-z0-9_]+", "_", name.strip().lower())
    normalized = re.sub(r"_+", "_", normalized).strip("_")
    if not normalized:
        normalized = "builder_app"
    if normalized[0].isdigit():
        normalized = f"app_{normalized}"
    return normalized


def _safe_relative_path(raw_path: str) -> Path:
    path = Path(raw_path)
    if path.is_absolute() or ".." in path.parts:
        msg = f"Invalid generated file path: {raw_path}"
        raise ValueError(msg)
    return path


def _build_agents_metadata(spec_kind: str, spec: dict[str, Any]) -> list[dict[str, Any]]:
    if spec_kind == "system":
        agents = spec.get("agents", [])
        if isinstance(agents, list):
            result = []
            for agent in agents:
                if not isinstance(agent, dict):
                    continue
                result.append(
                    {
                        "name": str(agent.get("name", "GeneratedAgent")).strip() or "GeneratedAgent",
                        "module": None,
                        "capabilities": list(agent.get("capabilities", [])),
                        "business_base": "platform",
                        "agent_type": str(agent.get("engine_type", "specialist")).strip() or None,
                        "pattern": str(agent.get("engine_type", "specialist")).strip() or None,
                    }
                )
            return result

    return [
        {
            "name": str(spec.get("name", "GeneratedAgent")).strip() or "GeneratedAgent",
            "module": None,
            "capabilities": list(spec.get("capabilities", [])),
            "business_base": "platform",
            "agent_type": str(spec.get("engine_type", "specialist")).strip() or None,
            "pattern": str(spec.get("engine_type", "specialist")).strip() or None,
        }
    ]


def _infer_engine_pattern(spec_kind: str, spec: dict[str, Any]) -> str:
    if spec_kind == "system":
        return "coordinator"
    engine_type = str(spec.get("engine_type", "simple")).strip().lower()
    if engine_type in {"simple", "pipeline"}:
        return engine_type
    return "flow"


def _build_app_config(
    *,
    request: MaterializeRequest,
    app_name: str,
    ports: dict[str, int | None],
    api_module: str | None,
    frontend_dir: str | None,
) -> dict[str, Any]:
    backend_url = f"http://localhost:{ports['api']}" if ports["api"] is not None else None
    frontend_url = f"http://localhost:{ports['frontend']}" if ports["frontend"] is not None else None
    runtime_commands = {
        "backend_dev": (
            f"python -m uvicorn {api_module} --host 0.0.0.0 --port {ports['api']} --reload"
            if api_module and ports["api"] is not None
            else None
        ),
        "frontend_dev": (
            (
                f"cd apps/{app_name}/{frontend_dir} && npm run dev -- --host 0.0.0.0 --port {ports['frontend']}"
                if frontend_dir
                else f"cd apps/{app_name} && npm run dev -- --host 0.0.0.0 --port {ports['frontend']}"
            )
            if ports["frontend"] is not None
            else None
        ),
        "publish": "docker compose up -d --build",
        "start": "docker compose up -d",
        "stop": "docker compose down",
    }

    manifest = {
        "name": app_name,
        "display_name": request.display_name or request.name,
        "description": request.description or request.goal,
        "business_base": "platform",
        "version": "0.1.0",
        "icon": request.icon,
        "ports": ports,
        "entry_points": {
            "api_module": api_module,
            "health": "/health" if api_module else None,
        },
        "agents": _build_agents_metadata(request.spec_kind, request.spec),
        "services": {
            "builder": {
                "template_id": request.template_id,
                "spec_kind": request.spec_kind,
                "output_type": request.output_type,
                "framework": request.framework,
                "generated_file_count": len(request.generated_files),
            }
        },
        "dependencies": {
            "database": None,
            "redis": False,
            "external": sorted({request.framework, "builder"} - {""}),
        },
        "runtime": {
            "urls": {
                "backend": backend_url,
                "frontend": frontend_url,
                "health": f"{backend_url}/health" if backend_url else None,
                "database": None,
            },
            "hosts": {
                "backend": "0.0.0.0" if backend_url else None,
                "frontend": "0.0.0.0" if frontend_url else None,
            },
            "database": {},
            "commands": runtime_commands,
        },
        "contracts": {
            "auth": {
                "enabled": False,
                "providers": [],
                "allow_anonymous": True,
            },
            "skills": {
                "auto_install": False,
                "hot_reload": True,
                "allowed_sources": [],
                "default_skills": [],
            },
            "release": {
                "strategy": "manual",
                "targets": ["local", "docker"],
                "environments": ["dev"],
                "require_approval": False,
            },
        },
        "product_line": "framework",
        "surface_profile": "developer",
        "audit_profile": "developer",
        "evolution": {
            "enabled": False,
            "validator_queue": {"backend": "none", "redis_url": None},
        },
        "plugin_bindings": [],
        "security_mode": None,
        "blueprint": {
            "engine_pattern": _infer_engine_pattern(request.spec_kind, request.spec),
            "flow_pattern": request.template_id or None,
            "system_prompt": request.goal or request.description,
            "app_template": "workflow_orchestrator" if request.spec_kind == "system" else "ops_automation_runner",
            "agents": [
                {
                    "name": agent["name"],
                    "role": "specialist",
                    "agent_type": agent.get("agent_type"),
                    "prompt": request.goal or request.description,
                    "capabilities": agent.get("capabilities", []),
                }
                for agent in _build_agents_metadata(request.spec_kind, request.spec)
            ],
        },
        "visibility": {"mode": "private", "tenants": []},
        "tags": [
            "builder-materialized",
            request.spec_kind,
            request.output_type,
            request.framework,
        ],
    }
    validated = AppConfig.model_validate(manifest)
    return validated.model_dump(mode="json", exclude_none=False)


def _determine_layout(
    generated_files: dict[str, str],
    output_type: str,
) -> tuple[bool, bool, str | None, str | None]:
    has_backend = (
        output_type in {"backend", "fullstack"} or "app.py" in generated_files or "backend/app.py" in generated_files
    )
    has_frontend = (
        output_type in {"frontend", "fullstack"}
        or "package.json" in generated_files
        or "frontend/package.json" in generated_files
    )

    api_module: str | None = None
    frontend_dir: str | None = None
    if "backend/app.py" in generated_files:
        api_module = "backend.app:app"
        frontend_dir = "frontend" if has_frontend else None
    elif "app.py" in generated_files:
        api_module = "app:app"
        frontend_dir = None
    else:
        frontend_dir = "frontend" if "frontend/package.json" in generated_files else None

    return has_backend, has_frontend, api_module, frontend_dir


def _write_generated_files(
    *,
    app_dir: Path,
    generated_files: dict[str, str],
    has_backend: bool,
    has_frontend: bool,
) -> list[str]:
    written: list[str] = []
    app_dir.mkdir(parents=True, exist_ok=False)
    (app_dir / "__init__.py").write_text("", encoding="utf-8")
    written.append(str((app_dir / "__init__.py").relative_to(Path.cwd())))

    if has_backend and "backend/app.py" in generated_files:
        backend_init = app_dir / "backend" / "__init__.py"
        backend_init.parent.mkdir(parents=True, exist_ok=True)
        backend_init.write_text("", encoding="utf-8")
        written.append(str(backend_init.relative_to(Path.cwd())))

    if has_frontend and "frontend/package.json" in generated_files:
        frontend_init = app_dir / "frontend" / ".gitkeep"
        frontend_init.parent.mkdir(parents=True, exist_ok=True)
        if not frontend_init.exists():
            frontend_init.write_text("", encoding="utf-8")
            written.append(str(frontend_init.relative_to(Path.cwd())))

    for raw_path, content in generated_files.items():
        target_rel = _safe_relative_path(raw_path)
        target = app_dir / target_rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content, encoding="utf-8")
        written.append(str(target.relative_to(Path.cwd())))

    return written


@router.post("/materialize", response_model=MaterializeResponse)
async def materialize_app(request: MaterializeRequest) -> MaterializeResponse:
    """生成ファイルを apps 配下に物化する."""
    if request.output_type not in _VALID_OUTPUT_TYPES:
        raise HTTPException(
            status_code=422,
            detail={"message": f"Unsupported output_type: {request.output_type}", "error_code": "INVALID_OUTPUT_TYPE"},
        )
    if not request.generated_files:
        raise HTTPException(
            status_code=422,
            detail={"message": "generated_files は必須です", "error_code": "GENERATED_FILES_REQUIRED"},
        )

    discovery = _get_discovery()
    app_name = _normalize_app_name(request.name)
    if discovery.get_app(app_name) is not None:
        raise HTTPException(
            status_code=409,
            detail={"message": f"App already exists: {app_name}", "error_code": "APP_ALREADY_EXISTS"},
        )

    app_dir = Path.cwd() / "apps" / app_name
    if app_dir.exists():
        raise HTTPException(
            status_code=409,
            detail={"message": f"App directory already exists: {app_dir}", "error_code": "APP_DIRECTORY_EXISTS"},
        )

    has_backend, has_frontend, api_module_suffix, frontend_dir = _determine_layout(
        request.generated_files,
        request.output_type,
    )
    ports = _get_port_allocator().allocate_for_new_app(
        frontend_enabled=has_frontend,
        database="none",
        redis_enabled=False,
    )

    written_files = _write_generated_files(
        app_dir=app_dir,
        generated_files=request.generated_files,
        has_backend=has_backend,
        has_frontend=has_frontend,
    )

    api_module = f"apps.{app_name}.{api_module_suffix}" if api_module_suffix else None
    config = _build_app_config(
        request=request,
        app_name=app_name,
        ports=ports,
        api_module=api_module,
        frontend_dir=frontend_dir,
    )
    config_path = app_dir / "app_config.json"
    config_path.write_text(json.dumps(config, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    written_files.append(str(config_path.relative_to(Path.cwd())))

    await discovery.scan()

    return MaterializeResponse(
        success=True,
        app_name=app_name,
        app_dir=str(app_dir),
        config_path=str(config_path),
        files_written=sorted(set(written_files)),
        ports=ports,
    )


@router.api_route("/{service_path:path}", methods=["GET", "POST", "PUT", "PATCH", "DELETE"])
async def proxy_to_dev_studio(service_path: str, request: Request) -> Response:
    """dev_studio へ透過 proxy する."""
    upstream = f"{_resolve_upstream_base()}/{service_path.lstrip('/')}"
    headers = {
        key: value
        for key, value in request.headers.items()
        if key.lower() not in {"host", "content-length", "connection"}
    }
    body = await request.body()

    try:
        async with httpx.AsyncClient(timeout=_UPSTREAM_TIMEOUT, trust_env=False) as client:
            upstream_response = await client.request(
                method=request.method,
                url=upstream,
                params=dict(request.query_params),
                content=body or None,
                headers=headers,
            )
    except httpx.ConnectError as exc:
        raise HTTPException(
            status_code=503,
            detail={
                "message": "dev_studio backend is unavailable",
                "error_code": "DEV_STUDIO_UNAVAILABLE",
                "upstream": upstream,
                "detail": str(exc),
            },
        ) from exc
    except httpx.TimeoutException as exc:
        raise HTTPException(
            status_code=504,
            detail={
                "message": "dev_studio backend timed out",
                "error_code": "DEV_STUDIO_TIMEOUT",
                "upstream": upstream,
                "detail": str(exc),
            },
        ) from exc

    content_type = upstream_response.headers.get("content-type", "")
    if upstream_response.status_code >= 400:
        detail: dict[str, Any]
        if "application/json" in content_type:
            payload = upstream_response.json()
            detail = payload if isinstance(payload, dict) else {"message": str(payload)}
        else:
            detail = {"message": upstream_response.text or "dev_studio request failed"}
        detail.setdefault("error_code", "DEV_STUDIO_ERROR")
        detail["upstream_status"] = upstream_response.status_code
        detail["upstream"] = upstream
        raise HTTPException(status_code=upstream_response.status_code, detail=detail)

    response_headers = {}
    if content_type:
        response_headers["content-type"] = content_type
    return Response(
        content=upstream_response.content,
        status_code=upstream_response.status_code,
        headers=response_headers,
    )


__all__ = ["init_dev_studio_services", "router"]
