"""TenantDashboard - 多租户ダッシュボードサービス.

テナント別の使用状況、分析、管理機能を提供。

使用例:
    >>> dashboard = TenantDashboard()
    >>> stats = await dashboard.get_stats("tenant-123")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from typing import Any

from apps.platform.services.component_library import (
    ComponentLibrary,
    ComponentType,
    get_component_library,
)

from agentflow.multi_tenant.manager import TenantManager, get_tenant_manager


@dataclass
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
        """辞書に変換."""
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


@dataclass
class UsageTrend:
    """使用傾向データ."""

    date: datetime
    api_calls: int = 0
    agent_executions: int = 0
    errors: int = 0


@dataclass
class TopComponent:
    """人気コンポーネント."""

    component_id: str
    name: str
    type: ComponentType
    usage_count: int


@dataclass
class RecentActivity:
    """最近のアクティビティ."""

    activity_type: str  # deploy, publish, create, update, delete
    component_id: str | None
    description: str
    timestamp: datetime
    user_id: str | None = None


class TenantDashboard:
    """多租户ダッシュボードサービス.

    テナント別の統計、分析、アクティビティを提供。
    """

    def __init__(
        self,
        tenant_manager: TenantManager | None = None,
        component_library: ComponentLibrary | None = None,
    ) -> None:
        """初期化.

        Args:
            tenant_manager: テナントマネージャー
            component_library: コンポーネントライブラリ
        """
        self._tenant_manager = tenant_manager or get_tenant_manager()
        self._library = component_library or get_component_library()
        self._logger = logging.getLogger(__name__)

        # アクティビティログ（インメモリ、本番ではDBに保存）
        self._activities: dict[str, list[RecentActivity]] = {}

    async def get_stats(self, tenant_id: str) -> TenantStats:
        """テナント統計を取得.

        Args:
            tenant_id: テナントID

        Returns:
            テナント統計情報
        """
        # テナントのコンポーネントを取得
        all_components = self._library.list_all()
        tenant_components = [c for c in all_components.values() if c.tenant_id == tenant_id]

        # タイプ別カウント
        agent_count = sum(1 for c in tenant_components if c.type == ComponentType.AGENT)
        flow_count = sum(1 for c in tenant_components if c.type == ComponentType.FLOW)
        tool_count = sum(1 for c in tenant_components if c.type == ComponentType.TOOL)
        skill_count = sum(1 for c in tenant_components if c.type == ComponentType.SKILL)

        # 総使用回数
        total_usage = sum(c.usage_count for c in tenant_components)

        # 最終アクティビティ
        last_activity = None
        if tenant_components:
            last_activity = max(c.updated_at for c in tenant_components)

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

    async def get_usage_trends(
        self,
        tenant_id: str,
        days: int = 30,
    ) -> list[UsageTrend]:
        """使用傾向を取得.

        Args:
            tenant_id: テナントID
            days: 取得日数

        Returns:
            使用傾向リスト
        """
        # TODO: 実際のメトリクス収集を実装
        # 現在はダミーデータを返す
        trends = []
        now = datetime.now(UTC)

        for i in range(days):
            date = now - timedelta(days=days - 1 - i)
            trends.append(
                UsageTrend(
                    date=date,
                    api_calls=0,  # 実際のデータに置き換え
                    agent_executions=0,
                    errors=0,
                )
            )

        return trends

    async def get_top_components(
        self,
        tenant_id: str,
        limit: int = 10,
    ) -> list[TopComponent]:
        """人気コンポーネントを取得.

        Args:
            tenant_id: テナントID
            limit: 取得数

        Returns:
            人気コンポーネントリスト
        """
        all_components = self._library.list_all()
        tenant_components = [c for c in all_components.values() if c.tenant_id == tenant_id]

        # 使用回数でソート
        sorted_components = sorted(
            tenant_components,
            key=lambda c: c.usage_count,
            reverse=True,
        )[:limit]

        return [
            TopComponent(
                component_id=c.id,
                name=c.name,
                type=c.type,
                usage_count=c.usage_count,
            )
            for c in sorted_components
        ]

    async def get_recent_activities(
        self,
        tenant_id: str,
        limit: int = 20,
    ) -> list[RecentActivity]:
        """最近のアクティビティを取得.

        Args:
            tenant_id: テナントID
            limit: 取得数

        Returns:
            アクティビティリスト
        """
        activities = self._activities.get(tenant_id, [])
        # 新しい順にソート
        sorted_activities = sorted(
            activities,
            key=lambda a: a.timestamp,
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
        """アクティビティを記録.

        Args:
            tenant_id: テナントID
            activity_type: アクティビティタイプ
            description: 説明
            component_id: コンポーネントID
            user_id: ユーザーID
        """
        if tenant_id not in self._activities:
            self._activities[tenant_id] = []

        activity = RecentActivity(
            activity_type=activity_type,
            component_id=component_id,
            description=description,
            timestamp=datetime.now(UTC),
            user_id=user_id,
        )

        self._activities[tenant_id].append(activity)

        # 古いアクティビティを削除（最大1000件保持）
        if len(self._activities[tenant_id]) > 1000:
            self._activities[tenant_id] = self._activities[tenant_id][-1000:]

    async def get_dashboard_summary(self, tenant_id: str) -> dict[str, Any]:
        """ダッシュボードサマリーを取得.

        Args:
            tenant_id: テナントID

        Returns:
            サマリー情報
        """
        stats = await self.get_stats(tenant_id)
        top_components = await self.get_top_components(tenant_id, limit=5)
        recent_activities = await self.get_recent_activities(tenant_id, limit=10)

        return {
            "stats": stats.to_dict(),
            "top_components": [
                {
                    "id": c.component_id,
                    "name": c.name,
                    "type": c.type.value,
                    "usage_count": c.usage_count,
                }
                for c in top_components
            ],
            "recent_activities": [
                {
                    "type": a.activity_type,
                    "component_id": a.component_id,
                    "description": a.description,
                    "timestamp": a.timestamp.isoformat(),
                }
                for a in recent_activities
            ],
        }


__all__ = ["RecentActivity", "TenantDashboard", "TenantStats", "TopComponent", "UsageTrend"]
