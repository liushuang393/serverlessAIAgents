"""マーケットプレイスAPI ルート.

エージェントマーケットプレイスの検索とインストール機能。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, HTTPException

from agentflow.studio.models import MarketplaceInstallRequest, MarketplaceSearchRequest

if TYPE_CHECKING:
    from agentflow.marketplace.client import MarketplaceClient


def create_marketplace_router(marketplace: MarketplaceClient) -> APIRouter:
    """マーケットプレイスAPIルーターを作成.

    Args:
        marketplace: マーケットプレイスクライアント

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/marketplace", tags=["marketplace"])

    @router.post("/search")
    async def search_marketplace(
        body: MarketplaceSearchRequest,
    ) -> list[dict[str, Any]]:
        """マーケットプレイスを検索。"""
        results = marketplace.search(
            query=body.query,
            category=body.category,
            protocols=body.protocols,
        )
        return [
            {
                "id": agent.id,
                "name": agent.name,
                "version": agent.version,
                "description": agent.description,
                "author": agent.author,
                "category": agent.category,
                "protocols": agent.protocols,
            }
            for agent in results
        ]

    @router.post("/install")
    async def install_agent(body: MarketplaceInstallRequest) -> dict[str, Any]:
        """マーケットプレイスからエージェントをインストール。"""
        try:
            marketplace.install(request.agent_id, force=request.force)
            return {
                "status": "success",
                "message": f"Agent {body.agent_id} installed successfully",
            }
        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))

    @router.get("/categories")
    async def list_categories() -> list[str]:
        """利用可能なカテゴリ一覧."""
        categories = {agent.category for agent in marketplace.search(limit=100)}
        return sorted(categories)

    @router.get("/featured")
    async def list_featured() -> list[dict[str, Any]]:
        """おすすめエージェント一覧."""
        results = marketplace.search(limit=5)
        return [
            {
                "id": agent.id,
                "name": agent.name,
                "description": agent.description,
                "category": agent.category,
            }
            for agent in results
        ]

    return router
