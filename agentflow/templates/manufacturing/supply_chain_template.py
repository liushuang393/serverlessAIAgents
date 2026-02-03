# -*- coding: utf-8 -*-
"""サプライチェーン最適化Agentテンプレート.

サプライチェーンの最適化を行うAgentテンプレート。

【機能】
- 在庫最適化
- 需要予測
- サプライヤー評価
- リードタイム最適化
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


class SupplyChainInput(BaseModel):
    """サプライチェーン入力."""

    analysis_type: str = Field(description="分析タイプ（INVENTORY/DEMAND/SUPPLIER/LEADTIME）")
    product_ids: list[str] = Field(default_factory=list, description="対象製品ID")
    historical_data: dict[str, Any] = Field(default_factory=dict, description="過去データ")
    constraints: dict[str, Any] = Field(default_factory=dict, description="制約条件")
    time_horizon: str = Field(default="1M", description="計画期間")


class InventoryRecommendation(BaseModel):
    """在庫推奨."""

    product_id: str
    current_level: int
    recommended_level: int
    reorder_point: int
    safety_stock: int


class SupplyChainOutput(BaseModel):
    """サプライチェーン出力."""

    analysis_type: str
    optimization_score: float = Field(ge=0, le=100)
    inventory_recommendations: list[InventoryRecommendation] = Field(default_factory=list)
    cost_savings: float = 0.0
    risk_factors: list[str] = Field(default_factory=list)
    recommendations: list[str] = Field(default_factory=list)


class SupplyChainAgent(ResilientAgent[SupplyChainInput, SupplyChainOutput]):
    """サプライチェーン最適化Agent."""

    name = "SupplyChainAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたはサプライチェーン最適化専門のAgentです。

【最適化項目】
- 在庫: 安全在庫、発注点、経済的発注量
- 需要: 時系列予測、季節性、トレンド
- サプライヤー: 信頼性、コスト、リードタイム
- リードタイム: ボトルネック、改善策

【出力形式】
JSON形式で以下を出力:
{
    "analysis_type": "分析タイプ",
    "optimization_score": 0-100,
    "inventory_recommendations": [...],
    "cost_savings": コスト削減額,
    "risk_factors": [...],
    "recommendations": [...]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> SupplyChainInput:
        """入力をパース."""
        return SupplyChainInput(**input_data)

    async def process(self, input_data: SupplyChainInput) -> SupplyChainOutput:
        """サプライチェーン分析を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: SupplyChainInput
    ) -> SupplyChainOutput:
        """LLMを使用したサプライチェーン分析."""
        from agentflow.utils import extract_json

        prompt = f"""以下のサプライチェーン分析を実施してください：
分析タイプ: {input_data.analysis_type}
対象製品: {input_data.product_ids}
計画期間: {input_data.time_horizon}
過去データ: {input_data.historical_data}
制約条件: {input_data.constraints}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return SupplyChainOutput(**data)
        return self._analyze_rule_based(input_data)

    def _analyze_rule_based(self, input_data: SupplyChainInput) -> SupplyChainOutput:
        """ルールベースのサプライチェーン分析."""
        recommendations: list[InventoryRecommendation] = []

        # 簡易的な在庫推奨
        for product_id in input_data.product_ids[:5]:
            recommendations.append(InventoryRecommendation(
                product_id=product_id,
                current_level=100,
                recommended_level=120,
                reorder_point=80,
                safety_stock=40,
            ))

        return SupplyChainOutput(
            analysis_type=input_data.analysis_type,
            optimization_score=75.0,
            inventory_recommendations=recommendations,
            cost_savings=0.0,
            risk_factors=["サプライヤー集中リスク"],
            recommendations=["在庫レベルの定期的な見直し", "サプライヤー分散化"],
        )

    def validate_output(self, output: SupplyChainOutput) -> bool:
        """出力検証."""
        return 0 <= output.optimization_score <= 100


class SupplyChainTemplate(AgentTemplate):
    """サプライチェーン最適化Agentテンプレート."""

    template_id: str = "supply_chain"
    name: str = "サプライチェーン最適化Agent"
    description: str = "サプライチェーンの最適化を行うAgent"
    industry: IndustryType = IndustryType.MANUFACTURING
    category: TemplateCategory = TemplateCategory.SUPPLY_CHAIN
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="analysis_types",
            description="分析タイプ",
            param_type="list",
            default=["INVENTORY", "DEMAND"],
        ),
        TemplateParameter(
            name="time_horizon",
            description="計画期間",
            param_type="string",
            default="1M",
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
        tags=["manufacturing", "supply_chain", "optimization"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> SupplyChainAgent:
        """サプライチェーン最適化Agentを生成."""
        merged_config = self.merge_config(config)
        agent = SupplyChainAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
