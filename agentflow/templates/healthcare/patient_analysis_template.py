# -*- coding: utf-8 -*-
"""患者データ分析Agentテンプレート.

患者データの分析を行うAgentテンプレート。

【機能】
- 健康トレンド分析
- リスク因子特定
- 予防的介入提案
- 人口統計分析
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


class PatientAnalysisInput(BaseModel):
    """患者データ分析入力."""

    patient_id: str = Field(description="患者ID")
    analysis_type: str = Field(description="分析タイプ（TREND/RISK/PREVENTIVE/POPULATION）")
    time_range: str = Field(default="1Y", description="分析期間")
    data_points: list[dict[str, Any]] = Field(default_factory=list, description="データポイント")
    demographics: dict[str, Any] = Field(default_factory=dict, description="人口統計情報")


class HealthTrend(BaseModel):
    """健康トレンド."""

    metric: str
    trend: str = Field(description="IMPROVING/STABLE/DECLINING")
    change_rate: float = 0.0
    significance: str = ""


class RiskFactor(BaseModel):
    """リスク因子."""

    factor: str
    risk_level: str = Field(description="LOW/MEDIUM/HIGH")
    modifiable: bool = True
    intervention: str = ""


class PatientAnalysisOutput(BaseModel):
    """患者データ分析出力."""

    patient_id: str
    analysis_type: str
    health_trends: list[HealthTrend] = Field(default_factory=list)
    risk_factors: list[RiskFactor] = Field(default_factory=list)
    preventive_recommendations: list[str] = Field(default_factory=list)
    key_insights: list[str] = Field(default_factory=list)
    follow_up_actions: list[str] = Field(default_factory=list)


class PatientAnalysisAgent(
    ResilientAgent[PatientAnalysisInput, PatientAnalysisOutput]
):
    """患者データ分析Agent."""

    name = "PatientAnalysisAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたは患者データ分析専門のAgentです。

【分析項目】
- トレンド分析: バイタル、検査値の経時変化
- リスク分析: 疾患リスク、合併症リスク
- 予防分析: 予防的介入の効果予測

【出力形式】
JSON形式で以下を出力:
{
    "patient_id": "患者ID",
    "analysis_type": "分析タイプ",
    "health_trends": [...],
    "risk_factors": [...],
    "preventive_recommendations": [...],
    "key_insights": [...],
    "follow_up_actions": [...]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> PatientAnalysisInput:
        """入力をパース."""
        return PatientAnalysisInput(**input_data)

    async def process(
        self, input_data: PatientAnalysisInput
    ) -> PatientAnalysisOutput:
        """患者データ分析を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: PatientAnalysisInput
    ) -> PatientAnalysisOutput:
        """LLMを使用した患者データ分析."""
        from agentflow.utils import extract_json

        prompt = f"""以下の患者データを分析してください：
患者ID: {input_data.patient_id}
分析タイプ: {input_data.analysis_type}
分析期間: {input_data.time_range}
データポイント数: {len(input_data.data_points)}
人口統計: {input_data.demographics}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return PatientAnalysisOutput(**data)
        return self._analyze_rule_based(input_data)

    def _analyze_rule_based(
        self, input_data: PatientAnalysisInput
    ) -> PatientAnalysisOutput:
        """ルールベースの患者データ分析."""
        return PatientAnalysisOutput(
            patient_id=input_data.patient_id,
            analysis_type=input_data.analysis_type,
            health_trends=[],
            risk_factors=[],
            preventive_recommendations=["定期検診の継続を推奨"],
            key_insights=["詳細なデータが必要です"],
            follow_up_actions=["次回診察時に詳細分析を実施"],
        )

    def validate_output(self, output: PatientAnalysisOutput) -> bool:
        """出力検証."""
        return bool(output.patient_id and output.analysis_type)


class PatientAnalysisTemplate(AgentTemplate):
    """患者データ分析Agentテンプレート."""

    template_id: str = "patient_analysis"
    name: str = "患者データ分析Agent"
    description: str = "患者データの分析を行うAgent"
    industry: IndustryType = IndustryType.HEALTHCARE
    category: TemplateCategory = TemplateCategory.PATIENT_ANALYSIS
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="analysis_types",
            description="分析タイプ",
            param_type="list",
            default=["TREND", "RISK"],
        ),
        TemplateParameter(
            name="time_range",
            description="分析期間",
            param_type="string",
            default="1Y",
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
        tags=["healthcare", "patient", "analysis"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> PatientAnalysisAgent:
        """患者データ分析Agentを生成."""
        merged_config = self.merge_config(config)
        agent = PatientAnalysisAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
