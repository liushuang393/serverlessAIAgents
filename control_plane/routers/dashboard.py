"""Dashboard Router - ダッシュボード API エンドポイント.

GET /api/studios/framework/dashboard/{tenant_id} - ダッシュボードサマリー
GET /api/studios/framework/dashboard/{tenant_id}/stats - テナント統計
GET /api/studios/framework/dashboard/{tenant_id}/top-components - 人気コンポーネント
GET /api/studios/framework/dashboard/{tenant_id}/activities - 最近のアクティビティ
GET /api/studios/framework/dashboard/{tenant_id}/trends - 使用傾向
"""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter, Depends, Query

from control_plane.engine import PlatformEngine


router = APIRouter(prefix="/api/studios/framework/dashboard", tags=["dashboard"])

# 依存性注入用のエンジンインスタンス
_engine: PlatformEngine | None = None


def get_engine() -> PlatformEngine:
    """Platform エンジンを取得."""
    global _engine
    if _engine is None:
        _engine = PlatformEngine()
    return _engine


def _as_dict(value: Any) -> dict[str, Any]:
    """ダッシュボード応答を辞書へ正規化する."""
    return value if isinstance(value, dict) else {}


def _as_list_of_dicts(value: Any) -> list[dict[str, Any]]:
    """ダッシュボード応答を辞書リストへ正規化する."""
    if not isinstance(value, list):
        return []
    return [item for item in value if isinstance(item, dict)]


@router.get("/{tenant_id}")
async def get_dashboard_summary(
    tenant_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """ダッシュボードサマリーを取得.

    Args:
        tenant_id: テナントID
        engine: Platform エンジン

    Returns:
        サマリー情報
    """
    summary = await engine.get_dashboard_summary(tenant_id)
    return _as_dict(summary)


@router.get("/{tenant_id}/stats")
async def get_tenant_stats(
    tenant_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """テナント統計を取得.

    Args:
        tenant_id: テナントID
        engine: Platform エンジン

    Returns:
        統計情報
    """
    stats = await engine.get_tenant_stats(tenant_id)
    return _as_dict(stats)


@router.get("/{tenant_id}/top-components")
async def get_top_components(
    tenant_id: str,
    limit: int = Query(default=10, ge=1, le=50, description="取得数"),
    engine: PlatformEngine = Depends(get_engine),
) -> list[dict[str, Any]]:
    """人気コンポーネントを取得.

    Args:
        tenant_id: テナントID
        limit: 取得数
        engine: Platform エンジン

    Returns:
        人気コンポーネントリスト
    """
    components = await engine.get_top_components(tenant_id, limit)
    return _as_list_of_dicts(components)


@router.get("/{tenant_id}/activities")
async def get_recent_activities(
    tenant_id: str,
    limit: int = Query(default=20, ge=1, le=100, description="取得数"),
    engine: PlatformEngine = Depends(get_engine),
) -> list[dict[str, Any]]:
    """最近のアクティビティを取得.

    Args:
        tenant_id: テナントID
        limit: 取得数
        engine: Platform エンジン

    Returns:
        アクティビティリスト
    """
    activities = await engine._dashboard.get_recent_activities(tenant_id, limit)
    return [
        {
            "type": a.activity_type,
            "component_id": a.component_id,
            "description": a.description,
            "timestamp": a.timestamp.isoformat(),
            "user_id": a.user_id,
        }
        for a in activities
    ]


@router.get("/{tenant_id}/trends")
async def get_usage_trends(
    tenant_id: str,
    days: int = Query(default=30, ge=1, le=90, description="取得日数"),
    engine: PlatformEngine = Depends(get_engine),
) -> list[dict[str, Any]]:
    """使用傾向を取得.

    Args:
        tenant_id: テナントID
        days: 取得日数
        engine: Platform エンジン

    Returns:
        使用傾向リスト
    """
    trends = await engine._dashboard.get_usage_trends(tenant_id, days)
    return [
        {
            "date": t.date.isoformat(),
            "api_calls": t.api_calls,
            "agent_executions": t.agent_executions,
            "errors": t.errors,
        }
        for t in trends
    ]
