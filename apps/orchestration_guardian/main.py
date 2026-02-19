"""Orchestration Guardian API.

A lightweight service for orchestration/protocol readiness checks.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
from typing import Any

import uvicorn
from fastapi import FastAPI, Response
from pydantic import BaseModel, Field

from agentflow import __version__ as agentflow_version


_APP_ROOT = Path(__file__).resolve().parent
_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"
_READY_SCORE_THRESHOLD = 75.0


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json."""
    if not _APP_CONFIG_PATH.is_file():
        return {}
    try:
        return json.loads(_APP_CONFIG_PATH.read_text("utf-8"))
    except json.JSONDecodeError:
        return {}


class VerifyRequest(BaseModel):
    """Payload for orchestration readiness check."""

    app_name: str = Field(..., min_length=1, description="Target app name")
    has_streaming: bool = Field(default=False, description="Whether SSE/WebSocket is implemented")
    has_a2a: bool = Field(default=False, description="Whether A2A contract is implemented")
    has_rag_contract: bool = Field(default=False, description="Whether RAG contract is aligned")
    has_auth_baseline: bool = Field(default=False, description="Whether auth baseline is enforced")


app = FastAPI(
    title="Orchestration Guardian",
    description="Orchestration and protocol readiness checker",
    version="1.0.0",
)


@app.get("/")
async def root() -> dict[str, str]:
    """Root endpoint to avoid 404."""
    return {
        "message": "Orchestration Guardian API",
        "docs_url": "/docs",
        "health_url": "/api/health",
    }


@app.get("/favicon.ico", include_in_schema=False)
async def favicon() -> Response:
    """Favicon endpoint to avoid 404."""
    return Response(content=b"", media_type="image/x-icon")


@app.get("/api/health")
async def health() -> dict[str, Any]:
    """Health endpoint."""
    return {
        "status": "healthy",
        "service": "orchestration_guardian",
        "agentflow_version": agentflow_version,
    }


@app.get("/api/checklist")
async def checklist() -> dict[str, Any]:
    """Return baseline checklist for app orchestration."""
    return {
        "items": [
            "engine pattern and implementation match",
            "frontend-backend protocol contract is explicit",
            "streaming progress surface exists",
            "agent-to-agent contract (A2A card/routing) exists for multi-agent apps",
            "MCP/RAG/Skills declarations match runtime implementation",
            "security baseline is enabled for external dependencies",
        ]
    }


@app.post("/api/verify")
async def verify(payload: VerifyRequest) -> dict[str, Any]:
    """Evaluate basic orchestration readiness score."""
    checks = {
        "streaming": payload.has_streaming,
        "a2a": payload.has_a2a,
        "rag_contract": payload.has_rag_contract,
        "auth_baseline": payload.has_auth_baseline,
    }
    passed = sum(1 for ok in checks.values() if ok)
    total = len(checks)
    score = round((passed / total) * 100, 2)
    return {
        "app_name": payload.app_name,
        "score": score,
        "passed": passed,
        "total": total,
        "checks": checks,
        "status": "ready" if score >= _READY_SCORE_THRESHOLD else "needs_improvement",
    }


def main() -> None:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(description="Orchestration Guardian API")
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
    default_port = app_config.get("ports", {}).get("api", 8007)
    host = args.host or os.getenv("ORCHESTRATION_GUARDIAN_HOST", "0.0.0.0")
    port = args.port or int(os.getenv("ORCHESTRATION_GUARDIAN_PORT", str(default_port)))

    if args.reload:
        uvicorn.run(
            "apps.orchestration_guardian.main:app",
            host=host,
            port=port,
            reload=True,
            reload_dirs=["apps/orchestration_guardian"],
        )
    else:
        uvicorn.run(app, host=host, port=port)


if __name__ == "__main__":
    main()
