"""AnalyticsAgent - 分析・レポートAgent.

使用状況の分析とレポート生成を行う。

使用例:
    >>> agent = AnalyticsAgent()
    >>> result = await agent.run({"tenant_id": "my-tenant", "analysis_type": "usage"})
"""

from __future__ import annotations

from typing import Any

from apps.platform.services.tenant_dashboard import TenantDashboard

from agentflow.core.agent_block import AgentBlock


class AnalyticsAgent(AgentBlock):
    """分析・レポートAgent.

    テナントの使用状況を分析し、インサイトを提供する。
    """

    name = "analytics-agent"

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
        """
        super().__init__()
        self._llm_client = llm_client
        self._dashboard = TenantDashboard()

    def _get_system_prompt(self) -> str:
        """システムプロンプトを取得."""
        return """あなたはAgentFlow分析アシスタントです。

テナントの使用状況を分析し、有用なインサイトを提供します。

分析タイプ:
- usage: 使用状況の概要
- trends: 使用傾向の分析
- components: コンポーネント使用分析
- recommendations: 改善提案

回答は日本語で、データに基づいた具体的なインサイトを提供してください。"""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行.

        Args:
            input_data: 入力データ

        Returns:
            分析結果
        """
        tenant_id = input_data.get("tenant_id", "default")
        analysis_type = input_data.get("analysis_type", "usage")

        if analysis_type == "usage":
            return await self._analyze_usage(tenant_id)
        if analysis_type == "trends":
            return await self._analyze_trends(tenant_id)
        if analysis_type == "components":
            return await self._analyze_components(tenant_id)
        if analysis_type == "recommendations":
            return await self._generate_recommendations(tenant_id)
        return {"success": False, "error": f"Unknown analysis type: {analysis_type}"}

    async def _analyze_usage(self, tenant_id: str) -> dict[str, Any]:
        """使用状況を分析.

        Args:
            tenant_id: テナントID

        Returns:
            分析結果
        """
        stats = await self._dashboard.get_stats(tenant_id)

        insights = []

        # コンポーネント構成の分析
        total = stats.component_count
        if total > 0:
            agent_ratio = stats.agent_count / total * 100
            if agent_ratio > 70:
                insights.append("Agent中心の構成です。Flowの活用で効率化できる可能性があります。")
            elif agent_ratio < 30:
                insights.append("Tool/Skill中心の構成です。複雑なタスクにはAgentの活用を検討してください。")

        # 使用頻度の分析
        if stats.total_usage == 0:
            insights.append("まだコンポーネントが使用されていません。テストを実行してみてください。")
        elif stats.total_usage > 1000:
            insights.append("活発に使用されています。パフォーマンス最適化を検討してください。")

        return {
            "success": True,
            "tenant_id": tenant_id,
            "stats": stats.to_dict(),
            "insights": insights,
        }

    async def _analyze_trends(self, tenant_id: str) -> dict[str, Any]:
        """使用傾向を分析.

        Args:
            tenant_id: テナントID

        Returns:
            分析結果
        """
        trends = await self._dashboard.get_usage_trends(tenant_id, days=30)

        # 傾向分析
        if len(trends) >= 7:
            recent_week = trends[-7:]
            previous_week = trends[-14:-7] if len(trends) >= 14 else []

            recent_total = sum(t.api_calls for t in recent_week)
            previous_total = sum(t.api_calls for t in previous_week) if previous_week else 0

            change = ((recent_total - previous_total) / previous_total * 100) if previous_total > 0 else 0

            trend_direction = "増加" if change > 0 else "減少" if change < 0 else "横ばい"
        else:
            change = 0
            trend_direction = "データ不足"

        return {
            "success": True,
            "tenant_id": tenant_id,
            "period_days": 30,
            "trend_direction": trend_direction,
            "change_percentage": round(change, 1),
            "data_points": [
                {
                    "date": t.date.isoformat(),
                    "api_calls": t.api_calls,
                    "agent_executions": t.agent_executions,
                    "errors": t.errors,
                }
                for t in trends
            ],
        }

    async def _analyze_components(self, tenant_id: str) -> dict[str, Any]:
        """コンポーネント使用を分析.

        Args:
            tenant_id: テナントID

        Returns:
            分析結果
        """
        top_components = await self._dashboard.get_top_components(tenant_id, limit=10)

        # 使用分布の分析
        if top_components:
            total_usage = sum(c.usage_count for c in top_components)
            top_usage = top_components[0].usage_count if top_components else 0
            concentration = (top_usage / total_usage * 100) if total_usage > 0 else 0
        else:
            concentration = 0

        insights = []
        if concentration > 50:
            insights.append("使用が特定のコンポーネントに集中しています。依存リスクを検討してください。")
        elif concentration < 20 and len(top_components) > 5:
            insights.append("使用が分散しています。共通化の機会があるかもしれません。")

        return {
            "success": True,
            "tenant_id": tenant_id,
            "top_components": [
                {
                    "id": c.component_id,
                    "name": c.name,
                    "type": c.type.value,
                    "usage_count": c.usage_count,
                }
                for c in top_components
            ],
            "concentration_percentage": round(concentration, 1),
            "insights": insights,
        }

    async def _generate_recommendations(self, tenant_id: str) -> dict[str, Any]:
        """改善提案を生成.

        Args:
            tenant_id: テナントID

        Returns:
            提案結果
        """
        stats = await self._dashboard.get_stats(tenant_id)
        await self._dashboard.get_top_components(tenant_id, limit=5)

        recommendations = []

        # コンポーネント数に基づく提案
        if stats.component_count < 5:
            recommendations.append(
                {
                    "category": "growth",
                    "title": "コンポーネントの追加",
                    "description": "Galleryから便利なコンポーネントを探してみてください。",
                    "priority": "medium",
                }
            )

        # Flowの活用提案
        if stats.flow_count == 0 and stats.agent_count >= 3:
            recommendations.append(
                {
                    "category": "optimization",
                    "title": "Flowの活用",
                    "description": "複数のAgentをFlowで連携させることで、複雑なタスクを効率化できます。",
                    "priority": "high",
                }
            )

        # Skillの再利用提案
        if stats.skill_count < stats.agent_count:
            recommendations.append(
                {
                    "category": "reusability",
                    "title": "Skillの抽出",
                    "description": "Agent内の共通機能をSkillとして抽出し、再利用性を高めましょう。",
                    "priority": "medium",
                }
            )

        return {
            "success": True,
            "tenant_id": tenant_id,
            "recommendations": recommendations,
            "recommendation_count": len(recommendations),
        }


__all__ = ["AnalyticsAgent"]
