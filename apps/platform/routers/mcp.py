# -*- coding: utf-8 -*-
"""MCP Router — MCP 設定管理 API."""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter, HTTPException

from apps.platform.schemas.provisioning_schemas import (
    MCPLazyLoadingPatchRequest,
    MCPServerUpsertRequest,
)
from apps.platform.services.mcp_registry import MCPRegistryService


router = APIRouter(prefix="/api/studios/framework/mcp", tags=["mcp"])

_registry: MCPRegistryService | None = None


def init_mcp_services(registry: MCPRegistryService) -> None:
    """サービスインスタンスを設定."""
    global _registry  # noqa: PLW0603
    _registry = registry


def _get_registry() -> MCPRegistryService:
    """MCPRegistryService を取得（未初期化時はエラー）."""
    if _registry is None:
        msg = "MCPRegistryService が未初期化です"
        raise RuntimeError(msg)
    return _registry


@router.get("/config")
async def get_mcp_config() -> dict[str, Any]:
    """MCP 設定全体を取得."""
    registry = _get_registry()
    config = registry.get_config()
    return {
        "config_path": str(registry.config_path),
        "config": config,
    }


@router.get("/servers")
async def list_mcp_servers() -> dict[str, Any]:
    """MCP サーバー一覧を取得."""
    registry = _get_registry()
    servers = registry.list_servers()
    return {
        "servers": servers,
        "total": len(servers),
    }


@router.post("/servers")
async def upsert_mcp_server(server: MCPServerUpsertRequest) -> dict[str, Any]:
    """MCP サーバーを追加または更新."""
    updated = _get_registry().upsert_server(server)
    return {
        "success": True,
        "server": updated,
    }


@router.delete("/servers/{name}")
async def delete_mcp_server(name: str) -> dict[str, Any]:
    """MCP サーバーを削除."""
    deleted = _get_registry().delete_server(name)
    if not deleted:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"MCP server not found: {name}",
                "error_code": "MCP_SERVER_NOT_FOUND",
            },
        )

    return {
        "success": True,
        "deleted": name,
    }


@router.patch("/lazy-loading")
async def patch_lazy_loading(patch: MCPLazyLoadingPatchRequest) -> dict[str, Any]:
    """lazy_loading 設定を部分更新."""
    updated = _get_registry().patch_lazy_loading(patch)
    return {
        "success": True,
        "lazy_loading": updated,
    }
