"""サービスノードAPI ルート.

Studio UIのノードパレット用サービスノード定義。
"""

from __future__ import annotations

from typing import Any, cast

from fastapi import APIRouter, HTTPException


def create_service_nodes_router() -> APIRouter:
    """サービスノードAPIルーターを作成.

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/nodes/service", tags=["service-nodes"])

    @router.get("")
    async def list_service_nodes() -> list[dict[str, Any]]:
        """利用可能なサービスノード一覧.

        Studio UIのノードパレット用。
        RAG/Text2SQL/Chart/Suggestion/FAQノードを含む。
        """
        from agentflow.flow.service_nodes import get_all_service_node_definitions

        return get_all_service_node_definitions()

    @router.get("/{node_type}")
    async def get_service_node_definition(node_type: str) -> dict[str, Any]:
        """特定のサービスノード定義を取得."""
        from agentflow.flow.service_nodes import (
            ChartNode,
            FAQNode,
            RAGNode,
            SuggestionNode,
            Text2SQLNode,
        )

        node_map: dict[str, type[Any]] = {
            "rag": RAGNode,
            "text2sql": Text2SQLNode,
            "chart": ChartNode,
            "suggestion": SuggestionNode,
            "faq": FAQNode,
        }

        node_cls = node_map.get(node_type)
        if not node_cls:
            raise HTTPException(
                status_code=404,
                detail=f"Service node '{node_type}' not found",
            )

        definition = node_cls.get_studio_definition()
        return cast("dict[str, Any]", definition)

    @router.get("/{node_type}/config")
    async def get_service_node_config(node_type: str) -> dict[str, Any]:
        """サービスノードの設定スキーマを取得."""
        from agentflow.flow.service_nodes import (
            ChartNode,
            FAQNode,
            RAGNode,
            SuggestionNode,
            Text2SQLNode,
        )

        node_map: dict[str, type[Any]] = {
            "rag": RAGNode,
            "text2sql": Text2SQLNode,
            "chart": ChartNode,
            "suggestion": SuggestionNode,
            "faq": FAQNode,
        }

        node_cls = node_map.get(node_type)
        if not node_cls:
            raise HTTPException(
                status_code=404,
                detail=f"Service node '{node_type}' not found",
            )

        definition = node_cls.get_studio_definition()
        config = definition.get("config", {})
        return cast("dict[str, Any]", config if isinstance(config, dict) else {})

    return router
