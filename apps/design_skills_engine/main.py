"""Design Skills Engine application entrypoint.

Expose DesignSkillsEngine via FastAPI endpoints with optional API key auth.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
from typing import TYPE_CHECKING, Any

import uvicorn
from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from agentflow.skills.builtin.design_skills.engine import DesignSkillsEngine


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


_APP_ROOT = Path(__file__).resolve().parent
_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"
_AUTH_HEADER = "x-api-key"
_PUBLIC_PATHS = {"/api/health", "/docs", "/redoc", "/openapi.json"}


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
    return os.getenv("DESIGN_SKILLS_API_KEY_ENV", "DESIGN_SKILLS_API_KEY")


def _require_api_key(request: Request) -> None:
    """Validate API key when auth is required."""
    if request.url.path in _PUBLIC_PATHS:
        return
    if not _is_auth_required():
        return

    env_name = _api_key_env_name()
    expected_key = os.getenv(env_name)
    if not expected_key:
        raise HTTPException(
            status_code=503,
            detail=f"Auth required but env '{env_name}' is not configured",
        )

    incoming_key = request.headers.get(_AUTH_HEADER)
    if incoming_key != expected_key:
        raise HTTPException(status_code=401, detail="Invalid API key")


def _to_jsonable(payload: Any) -> dict[str, Any]:
    """Normalize arbitrary event payload to dict."""
    if isinstance(payload, dict):
        return payload
    if hasattr(payload, "model_dump"):
        return payload.model_dump()  # type: ignore[no-any-return]
    if hasattr(payload, "to_dict"):
        return payload.to_dict()  # type: ignore[no-any-return]
    return {"value": str(payload)}


class DesignRequest(BaseModel):
    """Request model for design generation."""

    brief: str = Field(..., min_length=1, description="Natural language design brief")
    num_images: int = Field(default=4, ge=1, le=16, description="Number of images to generate")
    target_platform: str | None = Field(default=None, description="Target platform")
    style_preferences: list[str] | None = Field(default=None, description="Style keywords")
    brand_colors: list[str] | None = Field(default=None, description="Brand colors")
    aspect_ratio: str | None = Field(default=None, description="Aspect ratio")
    output_directory: str | None = Field(default=None, description="Output path")

    def to_payload(self) -> dict[str, Any]:
        """Convert request to pipeline payload."""
        return self.model_dump(exclude_none=True)


app = FastAPI(
    title="Design Skills Engine",
    description="Pipeline app for multi-image generation via ComfyUI",
    version="1.0.0",
)

_engine: DesignSkillsEngine | None = None


def _get_engine() -> DesignSkillsEngine:
    """Lazily initialize DesignSkillsEngine."""
    global _engine  # noqa: PLW0603
    if _engine is None:
        _engine = DesignSkillsEngine()
    return _engine


@app.get("/api/health")
async def health() -> dict[str, Any]:
    """Health endpoint."""
    return {
        "status": "healthy",
        "service": "design_skills_engine",
        "auth_required": _is_auth_required(),
    }


@app.post("/api/design")
async def generate_design(request: Request, body: DesignRequest) -> dict[str, Any]:
    """Run design pipeline and return final result."""
    _require_api_key(request)
    engine = _get_engine()
    result = await engine.run(body.to_payload())
    return _to_jsonable(result)


@app.post("/api/design/stream")
async def stream_design(request: Request, body: DesignRequest) -> StreamingResponse:
    """Run design pipeline as SSE stream."""
    _require_api_key(request)
    engine = _get_engine()
    payload = body.to_payload()

    async def event_generator() -> AsyncIterator[str]:
        start_event = {"type": "flow.start", "app": "design_skills_engine"}
        yield f"data: {json.dumps(start_event, ensure_ascii=False)}\n\n"
        try:
            async for event in engine.run_stream(payload):
                normalized = _to_jsonable(event)
                yield f"data: {json.dumps(normalized, ensure_ascii=False, default=str)}\n\n"
            complete_event = {"type": "flow.complete", "status": "ok"}
            yield f"data: {json.dumps(complete_event, ensure_ascii=False)}\n\n"
        except Exception as exc:
            error_event = {
                "type": "flow.error",
                "status": "error",
                "message": str(exc),
            }
            yield f"data: {json.dumps(error_event, ensure_ascii=False)}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


def main() -> None:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(description="Design Skills Engine API")
    parser.add_argument("--host", default=None, help="Host (default from env or app_config)")
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="Port (default from env or app_config)",
    )
    parser.add_argument("--reload", action="store_true", help="Enable auto reload")
    args = parser.parse_args()

    app_config = _load_app_config()
    default_port = app_config.get("ports", {}).get("api", 8006)
    host = args.host or os.getenv("DSE_HOST", "0.0.0.0")
    port = args.port or int(os.getenv("DSE_PORT", str(default_port)))

    if args.reload:
        uvicorn.run(
            "apps.design_skills_engine.main:app",
            host=host,
            port=port,
            reload=True,
            reload_dirs=["apps/design_skills_engine", "agentflow/skills/builtin/design_skills"],
        )
    else:
        uvicorn.run(app, host=host, port=port)


if __name__ == "__main__":
    main()
