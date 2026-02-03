# -*- coding: utf-8 -*-
"""治療計画Agentテンプレート.

治療計画の立案を支援するAgentテンプレート。

【機能】
- 治療オプション提示
- 薬物相互作用チェック
- 治療リスク評価
- フォローアップ計画
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


class TreatmentPlanningInput(BaseModel):
    """治療計画入力."""

    diagnosis: str = Field(description="診断名")
    patient_age: int = Field(ge=0, le=150, description="患者年齢")
    patient_gender: str = Field(description="患者性別")
    comorbidities: list[str] = Field(default_factory=list, description="併存疾患")
    allergies: list[str] = Field(default_factory=list, description="アレルギー")
    current_medications: list[str] = Field(default_factory=list, description="服用中薬")
    patient_preferences: dict[str, Any] = Field(default_factory=dict, description="患者希望")


class TreatmentOption(BaseModel):
    """治療オプション."""

    option_id: str
    name: str
    description: str
    efficacy: str = Field(description="LOW/MEDIUM/HIGH")
    risks: list[str] = Field(default_factory=list)
    contraindications: list[str] = Field(default_factory=list)
    estimated_duration: str = ""


class TreatmentPlanningOutput(BaseModel):
    """治療計画出力."""

    diagnosis: str
    treatment_options: list[TreatmentOption] = Field(default_factory=list)
    recommended_option: str = ""
    drug_interactions: list[str] = Field(default_factory=list)
    precautions: list[str] = Field(default_factory=list)
    follow_up_schedule: str = ""
    disclaimer: str = "この計画は参考情報であり、最終的な治療決定は医師が行ってください。"


class TreatmentPlanningAgent(
    ResilientAgent[TreatmentPlanningInput, TreatmentPlanningOutput]
):
    """治療計画Agent."""

    name = "TreatmentPlanningAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたは治療計画支援専門のAgentです。

【重要】
- これは治療計画支援ツールであり、最終決定は医師が行います
- 薬物相互作用とアレルギーは必ず確認

【出力形式】
JSON形式で以下を出力:
{
    "diagnosis": "診断名",
    "treatment_options": [...],
    "recommended_option": "推奨オプションID",
    "drug_interactions": [...],
    "precautions": [...],
    "follow_up_schedule": "...",
    "disclaimer": "..."
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> TreatmentPlanningInput:
        """入力をパース."""
        return TreatmentPlanningInput(**input_data)

    async def process(
        self, input_data: TreatmentPlanningInput
    ) -> TreatmentPlanningOutput:
        """治療計画を実行."""
        if self._llm:
            return await self._plan_with_llm(input_data)
        return self._plan_rule_based(input_data)

    async def _plan_with_llm(
        self, input_data: TreatmentPlanningInput
    ) -> TreatmentPlanningOutput:
        """LLMを使用した治療計画."""
        from agentflow.utils import extract_json

        prompt = f"""以下の患者の治療計画を立案してください：
診断: {input_data.diagnosis}
年齢: {input_data.patient_age}歳
性別: {input_data.patient_gender}
併存疾患: {', '.join(input_data.comorbidities) if input_data.comorbidities else 'なし'}
アレルギー: {', '.join(input_data.allergies) if input_data.allergies else 'なし'}
服用中薬: {', '.join(input_data.current_medications) if input_data.current_medications else 'なし'}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return TreatmentPlanningOutput(**data)
        return self._plan_rule_based(input_data)

    def _plan_rule_based(
        self, input_data: TreatmentPlanningInput
    ) -> TreatmentPlanningOutput:
        """ルールベースの治療計画."""
        return TreatmentPlanningOutput(
            diagnosis=input_data.diagnosis,
            treatment_options=[
                TreatmentOption(
                    option_id="A",
                    name="標準治療",
                    description="ガイドライン推奨の標準治療",
                    efficacy="HIGH",
                    risks=["一般的な副作用"],
                    estimated_duration="4-6週間",
                ),
            ],
            recommended_option="A",
            drug_interactions=[],
            precautions=["定期的なフォローアップが必要"],
            follow_up_schedule="2週間後に再診",
        )

    def validate_output(self, output: TreatmentPlanningOutput) -> bool:
        """出力検証."""
        return bool(output.diagnosis)


class TreatmentPlanningTemplate(AgentTemplate):
    """治療計画Agentテンプレート."""

    template_id: str = "treatment_planning"
    name: str = "治療計画Agent"
    description: str = "治療計画の立案を支援するAgent（最終決定は医師が行う）"
    industry: IndustryType = IndustryType.HEALTHCARE
    category: TemplateCategory = TemplateCategory.TREATMENT_PLANNING
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="check_interactions",
            description="薬物相互作用チェックを行うか",
            param_type="bool",
            default=True,
        ),
        TemplateParameter(
            name="include_alternatives",
            description="代替治療を含めるか",
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
        tags=["healthcare", "treatment", "planning"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> TreatmentPlanningAgent:
        """治療計画Agentを生成."""
        merged_config = self.merge_config(config)
        agent = TreatmentPlanningAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
