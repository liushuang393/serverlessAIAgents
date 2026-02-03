# -*- coding: utf-8 -*-
"""診断支援Agentテンプレート.

医療診断の支援を行うAgentテンプレート。

【機能】
- 症状分析
- 鑑別診断支援
- 検査推奨
- エビデンス提示

【注意】
このAgentは診断支援ツールであり、最終的な診断は医師が行う必要があります。
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


class DiagnosticSupportInput(BaseModel):
    """診断支援入力."""

    symptoms: list[str] = Field(description="症状リスト")
    patient_age: int = Field(ge=0, le=150, description="患者年齢")
    patient_gender: str = Field(description="患者性別（M/F/Other）")
    medical_history: list[str] = Field(default_factory=list, description="既往歴")
    current_medications: list[str] = Field(default_factory=list, description="服用中薬")
    vital_signs: dict[str, float] = Field(default_factory=dict, description="バイタル")
    lab_results: dict[str, Any] = Field(default_factory=dict, description="検査結果")


class DifferentialDiagnosis(BaseModel):
    """鑑別診断."""

    diagnosis: str
    probability: float = Field(ge=0, le=1)
    supporting_factors: list[str] = Field(default_factory=list)
    contradicting_factors: list[str] = Field(default_factory=list)
    recommended_tests: list[str] = Field(default_factory=list)


class DiagnosticSupportOutput(BaseModel):
    """診断支援出力."""

    differential_diagnoses: list[DifferentialDiagnosis] = Field(default_factory=list)
    urgency_level: str = Field(description="LOW/MEDIUM/HIGH/CRITICAL")
    recommended_tests: list[str] = Field(default_factory=list)
    red_flags: list[str] = Field(default_factory=list)
    notes: str = ""
    disclaimer: str = "この結果は参考情報であり、最終的な診断は医師が行ってください。"


class DiagnosticSupportAgent(
    ResilientAgent[DiagnosticSupportInput, DiagnosticSupportOutput]
):
    """診断支援Agent."""

    name = "DiagnosticSupportAgent"
    temperature = 0.2

    SYSTEM_PROMPT = """あなたは診断支援専門のAgentです。

【重要】
- これは診断支援ツールであり、最終判断は医師が行います
- 緊急性の高い状態を見逃さないことが最優先

【出力形式】
JSON形式で以下を出力:
{
    "differential_diagnoses": [...],
    "urgency_level": "LOW/MEDIUM/HIGH/CRITICAL",
    "recommended_tests": [...],
    "red_flags": [...],
    "notes": "...",
    "disclaimer": "..."
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> DiagnosticSupportInput:
        """入力をパース."""
        return DiagnosticSupportInput(**input_data)

    async def process(
        self, input_data: DiagnosticSupportInput
    ) -> DiagnosticSupportOutput:
        """診断支援を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: DiagnosticSupportInput
    ) -> DiagnosticSupportOutput:
        """LLMを使用した診断支援."""
        from agentflow.utils import extract_json

        prompt = f"""以下の患者情報に基づいて鑑別診断を支援してください：
年齢: {input_data.patient_age}歳
性別: {input_data.patient_gender}
症状: {', '.join(input_data.symptoms)}
既往歴: {', '.join(input_data.medical_history) if input_data.medical_history else 'なし'}
服用中薬: {', '.join(input_data.current_medications) if input_data.current_medications else 'なし'}
バイタル: {input_data.vital_signs}
検査結果: {input_data.lab_results}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return DiagnosticSupportOutput(**data)
        return self._analyze_rule_based(input_data)

    def _analyze_rule_based(
        self, input_data: DiagnosticSupportInput
    ) -> DiagnosticSupportOutput:
        """ルールベースの診断支援."""
        red_flags: list[str] = []
        urgency = "LOW"

        # バイタル異常チェック
        vitals = input_data.vital_signs
        if vitals.get("temperature", 37) > 39:
            red_flags.append("高熱")
            urgency = "HIGH"
        if vitals.get("blood_pressure_systolic", 120) > 180:
            red_flags.append("重度高血圧")
            urgency = "CRITICAL"

        return DiagnosticSupportOutput(
            differential_diagnoses=[],
            urgency_level=urgency,
            recommended_tests=["基本血液検査", "尿検査"],
            red_flags=red_flags,
            notes="詳細な病歴聴取と身体診察を推奨します",
        )

    def validate_output(self, output: DiagnosticSupportOutput) -> bool:
        """出力検証."""
        return output.urgency_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]


class DiagnosticSupportTemplate(AgentTemplate):
    """診断支援Agentテンプレート."""

    template_id: str = "diagnostic_support"
    name: str = "診断支援Agent"
    description: str = "医療診断の支援を行うAgent（最終診断は医師が行う）"
    industry: IndustryType = IndustryType.HEALTHCARE
    category: TemplateCategory = TemplateCategory.DIAGNOSTIC_SUPPORT
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="include_red_flags",
            description="レッドフラグチェックを含めるか",
            param_type="bool",
            default=True,
        ),
        TemplateParameter(
            name="max_diagnoses",
            description="最大鑑別診断数",
            param_type="int",
            default=5,
        ),
    ]
    default_config: TemplateConfig = TemplateConfig(
        model="gpt-4o",
        temperature=0.2,
        max_tokens=4096,
    )
    metadata: TemplateMetadata = TemplateMetadata(
        author="AgentFlow Team",
        version="1.0.0",
        tags=["healthcare", "diagnostic", "support"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> DiagnosticSupportAgent:
        """診断支援Agentを生成."""
        merged_config = self.merge_config(config)
        agent = DiagnosticSupportAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
