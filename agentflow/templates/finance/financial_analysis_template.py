# -*- coding: utf-8 -*-
"""財務分析Agentテンプレート.

企業財務分析を行うAgentテンプレート。

【機能】
- 財務諸表分析
- 収益性分析
- 安全性分析
- 成長性分析
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


class FinancialAnalysisInput(BaseModel):
    """財務分析入力."""

    company_name: str = Field(description="企業名")
    fiscal_year: str = Field(description="会計年度")
    financial_data: dict[str, Any] = Field(description="財務データ")
    industry: str = Field(default="", description="業界")
    analysis_type: list[str] = Field(
        default_factory=lambda: ["profitability", "safety", "growth"],
        description="分析タイプ",
    )


class FinancialRatio(BaseModel):
    """財務指標."""

    name: str
    value: float
    unit: str = ""
    benchmark: float | None = None
    assessment: str = ""  # GOOD/AVERAGE/POOR


class FinancialAnalysisOutput(BaseModel):
    """財務分析出力."""

    company_name: str
    fiscal_year: str
    overall_assessment: str
    ratios: list[FinancialRatio] = Field(default_factory=list)
    strengths: list[str] = Field(default_factory=list)
    weaknesses: list[str] = Field(default_factory=list)
    recommendation: str = ""
    confidence: float = Field(ge=0, le=1)


class FinancialAnalysisAgent(
    ResilientAgent[FinancialAnalysisInput, FinancialAnalysisOutput]
):
    """財務分析Agent."""

    name = "FinancialAnalysisAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたは財務分析専門のAgentです。

【分析項目】
- 収益性: ROE, ROA, 営業利益率, 純利益率
- 安全性: 自己資本比率, 流動比率, 固定比率
- 成長性: 売上成長率, 利益成長率, 総資産成長率
- 効率性: 総資産回転率, 棚卸資産回転率

【出力形式】
JSON形式で以下を出力:
{
    "company_name": "企業名",
    "fiscal_year": "年度",
    "overall_assessment": "総合評価",
    "ratios": [...],
    "strengths": [...],
    "weaknesses": [...],
    "recommendation": "推奨事項",
    "confidence": 0.0-1.0
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> FinancialAnalysisInput:
        """入力をパース."""
        return FinancialAnalysisInput(**input_data)

    async def process(
        self, input_data: FinancialAnalysisInput
    ) -> FinancialAnalysisOutput:
        """財務分析を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: FinancialAnalysisInput
    ) -> FinancialAnalysisOutput:
        """LLMを使用した財務分析."""
        from agentflow.utils import extract_json

        prompt = f"""以下の企業の財務分析を実施してください：
企業名: {input_data.company_name}
会計年度: {input_data.fiscal_year}
業界: {input_data.industry}
分析タイプ: {', '.join(input_data.analysis_type)}
財務データ: {input_data.financial_data}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return FinancialAnalysisOutput(**data)
        return self._analyze_rule_based(input_data)

    def _analyze_rule_based(
        self, input_data: FinancialAnalysisInput
    ) -> FinancialAnalysisOutput:
        """ルールベースの財務分析."""
        data = input_data.financial_data
        ratios: list[FinancialRatio] = []

        # ROE計算
        if "net_income" in data and "equity" in data and data["equity"] > 0:
            roe = data["net_income"] / data["equity"] * 100
            ratios.append(FinancialRatio(
                name="ROE",
                value=round(roe, 2),
                unit="%",
                benchmark=10.0,
                assessment="GOOD" if roe > 10 else "AVERAGE" if roe > 5 else "POOR",
            ))

        return FinancialAnalysisOutput(
            company_name=input_data.company_name,
            fiscal_year=input_data.fiscal_year,
            overall_assessment="詳細分析が必要",
            ratios=ratios,
            strengths=[],
            weaknesses=[],
            recommendation="詳細な財務データを提供してください",
            confidence=0.5,
        )

    def validate_output(self, output: FinancialAnalysisOutput) -> bool:
        """出力検証."""
        return bool(output.company_name and output.fiscal_year)


class FinancialAnalysisTemplate(AgentTemplate):
    """財務分析Agentテンプレート."""

    template_id: str = "financial_analysis"
    name: str = "財務分析Agent"
    description: str = "企業財務分析を行うAgent"
    industry: IndustryType = IndustryType.FINANCE
    category: TemplateCategory = TemplateCategory.FINANCIAL_ANALYSIS
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="analysis_types",
            description="分析タイプ",
            param_type="list",
            default=["profitability", "safety", "growth"],
        ),
        TemplateParameter(
            name="include_benchmark",
            description="ベンチマーク比較を含めるか",
            param_type="bool",
            default=True,
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
        tags=["finance", "financial", "analysis"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> FinancialAnalysisAgent:
        """財務分析Agentを生成."""
        merged_config = self.merge_config(config)
        agent = FinancialAnalysisAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
