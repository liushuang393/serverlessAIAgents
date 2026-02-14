# -*- coding: utf-8 -*-
"""Agents Router — Agent 管理 API エンドポイント.

GET  /api/agents              — 全 App 横断の Agent 一覧
GET  /api/agents/stats        — Agent 統計
GET  /api/agents/capabilities — 全能力タグ一覧
GET  /api/agents/by-app       — App 別グルーピング
GET  /api/agents/search       — 能力ベース検索
"""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter, Query

from apps.platform.services.agent_aggregator import AgentAggregatorService


router = APIRouter(prefix="/api/agents", tags=["agents"])

# モジュールレベルのシングルトン（main.py で初期化）
_aggregator: AgentAggregatorService | None = None


def init_agent_services(aggregator: AgentAggregatorService) -> None:
    """サービスインスタンスを設定.

    Args:
        aggregator: Agent 集約サービス
    """
    global _aggregator  # noqa: PLW0603
    _aggregator = aggregator


def _get_aggregator() -> AgentAggregatorService:
    """AgentAggregatorService を取得（未初期化時はエラー）."""
    if _aggregator is None:
        msg = "AgentAggregatorService が未初期化です"
        raise RuntimeError(msg)
    return _aggregator


# ------------------------------------------------------------------
# エンドポイント
# ------------------------------------------------------------------


@router.get("")
async def list_agents() -> dict[str, Any]:
    """全 App 横断の Agent 一覧."""
    aggregator = _get_aggregator()
    agents = aggregator.list_all()
    return {
        "agents": [a.to_dict() for a in agents],
        "total": len(agents),
    }


@router.get("/stats")
async def get_agent_stats() -> dict[str, Any]:
    """Agent 統計情報."""
    return _get_aggregator().stats()


@router.get("/capabilities")
async def list_capabilities() -> dict[str, Any]:
    """全能力タグとその出現回数."""
    aggregator = _get_aggregator()
    caps = aggregator.all_capabilities()
    return {"capabilities": caps, "total": len(caps)}


@router.get("/by-app")
async def agents_by_app() -> dict[str, Any]:
    """App 別にグルーピングした Agent 一覧."""
    aggregator = _get_aggregator()
    groups = aggregator.group_by_app()
    return {
        "groups": groups,
        "total_apps": len(groups),
    }


@router.get("/search")
async def search_agents(
    capability: str = Query(..., min_length=1, description="検索する能力タグ"),
) -> dict[str, Any]:
    """能力タグで Agent を検索.

    Args:
        capability: 検索する能力タグ
    """
    aggregator = _get_aggregator()
    agents = aggregator.search_by_capability(capability)
    return {
        "agents": [a.to_dict() for a in agents],
        "total": len(agents),
        "query": capability,
    }

