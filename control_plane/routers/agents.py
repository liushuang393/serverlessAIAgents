"""Agents Router — Agent 管理 API エンドポイント.

GET  /api/studios/framework/agents              — 全 App 横断の Agent 一覧
GET  /api/studios/framework/agents/stats        — Agent 統計
GET  /api/studios/framework/agents/by-app       — App 別グルーピング
GET  /api/studios/framework/agents/by-business-base — 業務基盤別グルーピング
GET  /api/studios/framework/agents/by-type      — Agent type 別グルーピング
GET  /api/studios/framework/agents/types        — Agent type 定義
GET  /api/studios/framework/agents/by-pattern   — Agent pattern 別グルーピング
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from fastapi import APIRouter


if TYPE_CHECKING:
    from control_plane.services.agent_aggregator import AgentAggregatorService


router = APIRouter(prefix="/api/studios/framework/agents", tags=["agents"])

# モジュールレベルのシングルトン（main.py で初期化）
_aggregator: AgentAggregatorService | None = None


def init_agent_services(aggregator: AgentAggregatorService) -> None:
    """サービスインスタンスを設定.

    Args:
        aggregator: Agent 集約サービス
    """
    global _aggregator
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


@router.get("/by-app")
async def agents_by_app() -> dict[str, Any]:
    """App 別にグルーピングした Agent 一覧."""
    aggregator = _get_aggregator()
    groups = aggregator.group_by_app()
    return {
        "groups": groups,
        "total_apps": len(groups),
    }


@router.get("/by-business-base")
async def agents_by_business_base() -> dict[str, Any]:
    """業務基盤別にグルーピングした Agent 一覧."""
    aggregator = _get_aggregator()
    groups = aggregator.grouped_business_bases()
    return {
        "groups": groups,
        "total_groups": len(groups),
    }


@router.get("/by-pattern")
async def agents_by_pattern() -> dict[str, Any]:
    """Agent pattern 別にグルーピングした Agent 一覧."""
    aggregator = _get_aggregator()
    groups = aggregator.grouped_patterns()
    return {
        "groups": groups,
        "total_groups": len(groups),
    }


@router.get("/by-type")
async def agents_by_type() -> dict[str, Any]:
    """Agent type 別にグルーピングした Agent 一覧."""
    aggregator = _get_aggregator()
    groups = aggregator.grouped_types()
    return {
        "groups": groups,
        "total_groups": len(groups),
    }


@router.get("/types")
async def agent_type_definitions() -> dict[str, Any]:
    """Agent type 定義と行動説明を返す."""
    aggregator = _get_aggregator()
    items = aggregator.type_definitions()
    return {
        "types": items,
        "total": len(items),
    }



