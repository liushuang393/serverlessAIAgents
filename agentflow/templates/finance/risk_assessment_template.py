# -*- coding: utf-8 -*-
"""リスク評価Agentテンプレート.

金融取引・投資判断のリスク評価を行うAgentテンプレート。

【機能】
- 市場リスク評価
- 信用リスク評価
- オペレーショナルリスク評価
- 流動性リスク評価
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
    TemplateValidationRule,
    ValidationRuleType,
)

logger = logging.getLogger(__name__)


class RiskAssessmentInput(BaseModel):
    """リスク評価入力."""

    asset_type: str = Field(description="資産タイプ（株式/債券/為替/商品）")
    amount: float = Field(description="評価額")
    currency: str = Field(default="JPY", description="通貨")
    time_horizon: str = Field(default="1M", description="評価期間（1D/1W/1M/1Y）")
    risk_factors: list[str] = Field(default_factory=list, description="考慮するリスク要因")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class RiskScore(BaseModel):
    """リスクスコア."""

    category: str
    score: float = Field(ge=0, le=100)
    level: str = Field(description="LOW/MEDIUM/HIGH/CRITICAL")
    factors: list[str] = Field(default_factory=list)
    mitigation: str = ""


class RiskAssessmentOutput(BaseModel):
    """リスク評価出力."""

    overall_risk_score: float = Field(ge=0, le=100)
    overall_risk_level: str
    risk_breakdown: list[RiskScore] = Field(default_factory=list)
    recommendation: str = ""
    confidence: float = Field(ge=0, le=1)


class RiskAssessmentAgent(ResilientAgent[RiskAssessmentInput, RiskAssessmentOutput]):
    """リスク評価Agent."""

    name = "RiskAssessmentAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたはリスク評価専門のAgentです。

【評価基準】
- 市場リスク: 価格変動、金利変動、為替変動
- 信用リスク: 取引先の信用力、デフォルト確率
- 流動性リスク: 換金性、市場深度
- オペレーショナルリスク: 運用上のリスク、システムリスク

【出力形式】
JSON形式で以下を出力:
{
    "overall_risk_score": 0-100の数値,
    "overall_risk_level": "LOW/MEDIUM/HIGH/CRITICAL",
    "risk_breakdown": [
        {"category": "市場リスク", "score": 60, "level": "MEDIUM", "factors": [...], "mitigation": "..."}
    ],
    "recommendation": "推奨事項",
    "confidence": 0.0-1.0
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> RiskAssessmentInput:
        """入力をパース."""
        return RiskAssessmentInput(**input_data)

    async def process(self, input_data: RiskAssessmentInput) -> RiskAssessmentOutput:
        """リスク評価を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: RiskAssessmentInput
    ) -> RiskAssessmentOutput:
        """LLMを使用したリスク評価."""
        from agentflow.utils import extract_json

        prompt = f"""以下の資産のリスクを評価してください：
資産タイプ: {input_data.asset_type}
評価額: {input_data.amount:,.0f} {input_data.currency}
評価期間: {input_data.time_horizon}
リスク要因: {', '.join(input_data.risk_factors) if input_data.risk_factors else '自動判定'}
追加情報: {input_data.context}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return RiskAssessmentOutput(**data)
        return self._analyze_rule_based(input_data)

    def _analyze_rule_based(
        self, input_data: RiskAssessmentInput
    ) -> RiskAssessmentOutput:
        """ルールベースのリスク評価."""
        # 簡易的なルールベース評価
        base_score = 50.0
        if input_data.amount > 100_000_000:
            base_score += 20
        if input_data.asset_type in ["為替", "商品"]:
            base_score += 15

        level = "LOW" if base_score < 40 else "MEDIUM" if base_score < 70 else "HIGH"

        return RiskAssessmentOutput(
            overall_risk_score=min(base_score, 100),
            overall_risk_level=level,
            risk_breakdown=[
                RiskScore(category="市場リスク", score=base_score, level=level, factors=["価格変動"]),
            ],
            recommendation="定期的なモニタリングを推奨",
            confidence=0.7,
        )

    def validate_output(self, output: RiskAssessmentOutput) -> bool:
        """出力検証."""
        return 0 <= output.overall_risk_score <= 100


class RiskAssessmentTemplate(AgentTemplate):
    """リスク評価Agentテンプレート."""

    template_id: str = "risk_assessment"
    name: str = "リスク評価Agent"
    description: str = "金融取引・投資判断のリスク評価を行うAgent"
    industry: IndustryType = IndustryType.FINANCE
    category: TemplateCategory = TemplateCategory.RISK_ASSESSMENT
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="risk_threshold",
            description="リスク閾値（この値を超えると警告）",
            param_type="float",
            default=70.0,
            required=False,
            validation_rules=[
                TemplateValidationRule(
                    rule_type=ValidationRuleType.RANGE,
                    field="risk_threshold",
                    params={"min": 0, "max": 100},
                )
            ],
        ),
        TemplateParameter(
            name="include_mitigation",
            description="緩和策を含めるか",
            param_type="bool",
            default=True,
        ),
    ]
    default_config: TemplateConfig = TemplateConfig(
        model="gpt-4o",
        temperature=0.3,
        max_tokens=2048,
    )
    metadata: TemplateMetadata = TemplateMetadata(
        author="AgentFlow Team",
        version="1.0.0",
        tags=["finance", "risk", "assessment"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> RiskAssessmentAgent:
        """リスク評価Agentを生成.

        Args:
            config: カスタム設定
            llm_client: LLMクライアント

        Returns:
            RiskAssessmentAgent
        """
        merged_config = self.merge_config(config)
        agent = RiskAssessmentAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        agent.timeout_seconds = merged_config.timeout_seconds
        agent.max_retries = merged_config.max_retries
        return agent
