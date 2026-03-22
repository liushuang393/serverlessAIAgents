"""Control-plane dashboard 集約サービス.

カード型の軽量ダッシュボード API と、
テナント統計ダッシュボードの正規実装をまとめる。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime, timedelta
from typing import Any

from control_plane.publish.runtime import ComponentLibrary, ComponentType, get_component_library
from kernel.runtime.multi_tenant.manager import TenantManager, get_tenant_manager


@dataclass(frozen=True, slots=True)
class DashboardCard:
    """ダッシュボード表示用カード."""

    card_id: str
    title: str
    metrics: dict[str, Any] = field(default_factory=dict)


class DashboardService:
    """カード一覧を管理する."""

    def __init__(self) -> None:
        self._cards: dict[str, DashboardCard] = {}

    def upsert(self, card: DashboardCard) -> None:
        """カードを追加または更新する。"""
        self._cards[card.card_id] = card

    def list_cards(self) -> list[DashboardCard]:
        """カード一覧を返す。"""
        return sorted(self._cards.values(), key=lambda item: item.card_id)


@dataclass(slots=True)
class TenantStats:
    """テナント統計情報."""

    tenant_id: str
    component_count: int = 0
    agent_count: int = 0
    flow_count: int = 0
    tool_count: int = 0
    skill_count: int = 0
    total_usage: int = 0
    active_deployments: int = 0
    api_calls_today: int = 0
    api_calls_month: int = 0
    storage_used_mb: float = 0.0
    last_activity: datetime | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書へ変換する。"""
        return {
            "tenant_id": self.tenant_id,
            "component_count": self.component_count,
            "agent_count": self.agent_count,
            "flow_count": self.flow_count,
            "tool_count": self.tool_count,
            "skill_count": self.skill_count,
            "total_usage": self.total_usage,
            "active_deployments": self.active_deployments,
            "api_calls_today": self.api_calls_today,
            "api_calls_month": self.api_calls_month,
            "storage_used_mb": self.storage_used_mb,
            "last_activity": self.last_activity.isoformat() if self.last_activity else None,
        }


@dataclass(slots=True)
class UsageTrend:
    """使用傾向データ."""

    date: datetime
    api_calls: int = 0
    agent_executions: int = 0
    errors: int = 0


@dataclass(slots=True)
class TopComponent:
    """人気コンポーネント."""

    component_id: str
    name: str
    type: ComponentType
    usage_count: int


@dataclass(slots=True)
class RecentActivity:
    """最近のアクティビティ."""

    activity_type: str
    component_id: str | None
    description: str
    timestamp: datetime
    user_id: str | None = None


class TenantDashboard:
    """多租戸ダッシュボードサービス."""

    def __init__(
        self,
        tenant_manager: TenantManager | None = None,
        component_library: ComponentLibrary | None = None,
    ) -> None:
        """初期化する。"""
        self._tenant_manager = tenant_manager or get_tenant_manager()
        self._library = component_library or get_component_library()
        self._logger = logging.getLogger(__name__)
        self._activities: dict[str, list[RecentActivity]] = {}

    async def get_stats(self, tenant_id: str) -> TenantStats:
        """テナント統計を返す。"""
        del self._tenant_manager
        all_components = self._library.list_all()
        tenant_components = [c for c in all_components.values() if c.tenant_id == tenant_id]

        agent_count = sum(1 for c in tenant_components if c.type == ComponentType.AGENT)
        flow_count = sum(1 for c in tenant_components if c.type == ComponentType.FLOW)
        tool_count = sum(1 for c in tenant_components if c.type == ComponentType.TOOL)
        skill_count = sum(1 for c in tenant_components if c.type == ComponentType.SKILL)
        total_usage = sum(c.usage_count for c in tenant_components)
        last_activity = max((c.updated_at for c in tenant_components), default=None)

        return TenantStats(
            tenant_id=tenant_id,
            component_count=len(tenant_components),
            agent_count=agent_count,
            flow_count=flow_count,
            tool_count=tool_count,
            skill_count=skill_count,
            total_usage=total_usage,
            last_activity=last_activity,
        )

    async def get_usage_trends(self, tenant_id: str, days: int = 30) -> list[UsageTrend]:
        """使用傾向を返す。"""
        normalized_days = max(1, min(days, 365))
        now = datetime.now(UTC)
        start_date = (now - timedelta(days=normalized_days - 1)).date()
        daily: dict[str, UsageTrend] = {}

        for offset in range(normalized_days):
            date = start_date + timedelta(days=offset)
            key = date.isoformat()
            daily[key] = UsageTrend(
                date=datetime.combine(date, datetime.min.time(), tzinfo=UTC),
            )

        for activity in self._activities.get(tenant_id, []):
            activity_key = activity.timestamp.astimezone(UTC).date().isoformat()
            trend = daily.get(activity_key)
            if trend is None:
                continue

            activity_type = activity.activity_type.lower()
            if activity_type in {"create", "publish", "start", "restart", "run", "invoke"}:
                trend.api_calls += 1
            if activity_type in {"start", "restart", "run", "invoke"}:
                trend.agent_executions += 1
            if "error" in activity_type or "fail" in activity_type:
                trend.errors += 1

        return [daily[key] for key in sorted(daily.keys())]

    async def get_top_components(self, tenant_id: str, limit: int = 10) -> list[TopComponent]:
        """人気コンポーネントを返す。"""
        all_components = self._library.list_all()
        tenant_components = [c for c in all_components.values() if c.tenant_id == tenant_id]
        sorted_components = sorted(
            tenant_components,
            key=lambda component: component.usage_count,
            reverse=True,
        )[:limit]

        return [
            TopComponent(
                component_id=component.id,
                name=component.name,
                type=component.type,
                usage_count=component.usage_count,
            )
            for component in sorted_components
        ]

    async def get_recent_activities(self, tenant_id: str, limit: int = 20) -> list[RecentActivity]:
        """最近のアクティビティを返す。"""
        activities = self._activities.get(tenant_id, [])
        sorted_activities = sorted(
            activities,
            key=lambda activity: activity.timestamp,
            reverse=True,
        )
        return sorted_activities[:limit]

    def record_activity(
        self,
        tenant_id: str,
        activity_type: str,
        description: str,
        component_id: str | None = None,
        user_id: str | None = None,
    ) -> None:
        """アクティビティを記録する。"""
        bucket = self._activities.setdefault(tenant_id, [])
        bucket.append(
            RecentActivity(
                activity_type=activity_type,
                component_id=component_id,
                description=description,
                timestamp=datetime.now(UTC),
                user_id=user_id,
            )
        )
        if len(bucket) > 1000:
            self._activities[tenant_id] = bucket[-1000:]

    async def get_dashboard_summary(self, tenant_id: str) -> dict[str, Any]:
        """ダッシュボード要約を返す。"""
        stats = await self.get_stats(tenant_id)
        top_components = await self.get_top_components(tenant_id, limit=5)
        recent_activities = await self.get_recent_activities(tenant_id, limit=10)
        return {
            "stats": stats.to_dict(),
            "top_components": [
                {
                    "id": component.component_id,
                    "name": component.name,
                    "type": component.type.value,
                    "usage_count": component.usage_count,
                }
                for component in top_components
            ],
            "recent_activities": [
                {
                    "type": activity.activity_type,
                    "component_id": activity.component_id,
                    "description": activity.description,
                    "timestamp": activity.timestamp.isoformat(),
                }
                for activity in recent_activities
            ],
        }


_dashboard_singleton: TenantDashboard | None = None


def get_tenant_dashboard() -> TenantDashboard:
    """TenantDashboard シングルトンを返す。"""
    global _dashboard_singleton
    if _dashboard_singleton is None:
        _dashboard_singleton = TenantDashboard()
    return _dashboard_singleton


__all__ = [
    "DashboardCard",
    "DashboardService",
    "RecentActivity",
    "TenantDashboard",
    "TenantStats",
    "TopComponent",
    "UsageTrend",
    "get_tenant_dashboard",
]
