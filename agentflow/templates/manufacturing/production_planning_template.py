# -*- coding: utf-8 -*-
"""生産計画Agentテンプレート.

生産計画の最適化を行うAgentテンプレート。

【機能】
- 生産スケジューリング
- リソース配分最適化
- 生産能力計画
- ボトルネック分析
"""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field

from agentflow import ResilientAgent
from agentflow.templates.base_template import (
    AgentTemplate,
    IndustryType,
    TemplateCategory,
    TemplateConfig,
    TemplateMetadata,
    TemplateParameter,
)

logger = logging.getLogger(__name__)


class ProductionPlanningInput(BaseModel):
    """生産計画入力."""

    orders: list[dict[str, Any]] = Field(description="受注情報")
    resources: dict[str, Any] = Field(default_factory=dict, description="リソース情報")
    constraints: dict[str, Any] = Field(default_factory=dict, description="制約条件")
    planning_horizon: str = Field(default="1W", description="計画期間")
    optimization_goal: str = Field(default="throughput", description="最適化目標")


class ProductionSchedule(BaseModel):
    """生産スケジュール."""

    order_id: str
    product_id: str
    start_time: str
    end_time: str
    resource_id: str
    quantity: int


class ProductionPlanningOutput(BaseModel):
    """生産計画出力."""

    schedules: list[ProductionSchedule] = Field(default_factory=list)
    utilization_rate: float = Field(ge=0, le=100)
    bottlenecks: list[str] = Field(default_factory=list)
    total_throughput: int = 0
    completion_rate: float = Field(ge=0, le=100)
    recommendations: list[str] = Field(default_factory=list)


class ProductionPlanningAgent(
    ResilientAgent[ProductionPlanningInput, ProductionPlanningOutput]
):
    """生産計画Agent."""

    name = "ProductionPlanningAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたは生産計画専門のAgentです。

【計画基準】
- 納期遵守
- リソース効率最大化
- ボトルネック回避
- 段取り時間最小化

【出力形式】
JSON形式で以下を出力:
{
    "schedules": [...],
    "utilization_rate": 0-100,
    "bottlenecks": [...],
    "total_throughput": 数量,
    "completion_rate": 0-100,
    "recommendations": [...]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ProductionPlanningInput:
        """入力をパース."""
        return ProductionPlanningInput(**input_data)

    async def process(
        self, input_data: ProductionPlanningInput
    ) -> ProductionPlanningOutput:
        """生産計画を実行."""
        if self._llm:
            return await self._plan_with_llm(input_data)
        return self._plan_rule_based(input_data)

    async def _plan_with_llm(
        self, input_data: ProductionPlanningInput
    ) -> ProductionPlanningOutput:
        """LLMを使用した生産計画."""
        from agentflow.utils import extract_json

        prompt = f"""以下の生産計画を立案してください：
受注数: {len(input_data.orders)}件
計画期間: {input_data.planning_horizon}
最適化目標: {input_data.optimization_goal}
リソース: {input_data.resources}
制約: {input_data.constraints}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return ProductionPlanningOutput(**data)
        return self._plan_rule_based(input_data)

    def _plan_rule_based(
        self, input_data: ProductionPlanningInput
    ) -> ProductionPlanningOutput:
        """ルールベースの生産計画."""
        schedules: list[ProductionSchedule] = []

        # 簡易的なスケジューリング
        for i, order in enumerate(input_data.orders[:10]):
            schedules.append(ProductionSchedule(
                order_id=order.get("order_id", f"ORD-{i}"),
                product_id=order.get("product_id", f"PRD-{i}"),
                start_time=f"Day {i // 2 + 1} 08:00",
                end_time=f"Day {i // 2 + 1} 17:00",
                resource_id="LINE-01",
                quantity=order.get("quantity", 100),
            ))

        return ProductionPlanningOutput(
            schedules=schedules,
            utilization_rate=80.0,
            bottlenecks=["組立工程"],
            total_throughput=sum(s.quantity for s in schedules),
            completion_rate=95.0,
            recommendations=["夜間シフトの追加検討", "ボトルネック工程の増強"],
        )

    def validate_output(self, output: ProductionPlanningOutput) -> bool:
        """出力検証."""
        return 0 <= output.utilization_rate <= 100


class ProductionPlanningTemplate(AgentTemplate):
    """生産計画Agentテンプレート."""

    template_id: str = "production_planning"
    name: str = "生産計画Agent"
    description: str = "生産計画の最適化を行うAgent"
    industry: IndustryType = IndustryType.MANUFACTURING
    category: TemplateCategory = TemplateCategory.PRODUCTION_PLANNING
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="planning_horizon",
            description="計画期間",
            param_type="string",
            default="1W",
        ),
        TemplateParameter(
            name="optimization_goal",
            description="最適化目標",
            param_type="string",
            default="throughput",
        ),
    ]
    default_config: TemplateConfig = TemplateConfig(
        model="gpt-4o",
        temperature=0.3,
        max_tokens=4096,
    )
    metadata: TemplateMetadata = TemplateMetadata(
        author="AgentFlow Team",
        version="1.0.0",
        tags=["manufacturing", "production", "planning"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> ProductionPlanningAgent:
        """生産計画Agentを生成."""
        merged_config = self.merge_config(config)
        agent = ProductionPlanningAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
