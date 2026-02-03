# -*- coding: utf-8 -*-
"""品質管理Agentテンプレート.

製造工程の品質管理を行うAgentテンプレート。

【機能】
- 不良品検出・分類
- 品質指標モニタリング
- 根本原因分析
- 是正措置提案
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


class QualityControlInput(BaseModel):
    """品質管理入力."""

    product_id: str = Field(description="製品ID")
    inspection_type: str = Field(description="検査タイプ（VISUAL/DIMENSIONAL/FUNCTIONAL）")
    measurements: dict[str, float] = Field(default_factory=dict, description="測定値")
    specifications: dict[str, dict[str, float]] = Field(
        default_factory=dict, description="仕様（min/max）"
    )
    production_batch: str = Field(default="", description="生産バッチ")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class QualityDefect(BaseModel):
    """品質欠陥."""

    defect_id: str
    defect_type: str
    severity: str = Field(description="MINOR/MAJOR/CRITICAL")
    location: str = ""
    description: str
    root_cause: str = ""


class QualityControlOutput(BaseModel):
    """品質管理出力."""

    product_id: str
    inspection_result: str = Field(description="PASS/FAIL/CONDITIONAL")
    quality_score: float = Field(ge=0, le=100)
    defects: list[QualityDefect] = Field(default_factory=list)
    out_of_spec: list[str] = Field(default_factory=list)
    corrective_actions: list[str] = Field(default_factory=list)
    recommendation: str = ""


class QualityControlAgent(ResilientAgent[QualityControlInput, QualityControlOutput]):
    """品質管理Agent."""

    name = "QualityControlAgent"
    temperature = 0.2

    SYSTEM_PROMPT = """あなたは品質管理専門のAgentです。

【検査基準】
- 外観検査: キズ、汚れ、変形
- 寸法検査: 公差内か確認
- 機能検査: 動作確認

【出力形式】
JSON形式で以下を出力:
{
    "product_id": "製品ID",
    "inspection_result": "PASS/FAIL/CONDITIONAL",
    "quality_score": 0-100,
    "defects": [...],
    "out_of_spec": [...],
    "corrective_actions": [...],
    "recommendation": "..."
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> QualityControlInput:
        """入力をパース."""
        return QualityControlInput(**input_data)

    async def process(self, input_data: QualityControlInput) -> QualityControlOutput:
        """品質検査を実行."""
        if self._llm:
            return await self._inspect_with_llm(input_data)
        return self._inspect_rule_based(input_data)

    async def _inspect_with_llm(
        self, input_data: QualityControlInput
    ) -> QualityControlOutput:
        """LLMを使用した品質検査."""
        from agentflow.utils import extract_json

        prompt = f"""以下の製品の品質検査を実施してください：
製品ID: {input_data.product_id}
検査タイプ: {input_data.inspection_type}
測定値: {input_data.measurements}
仕様: {input_data.specifications}
バッチ: {input_data.production_batch}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return QualityControlOutput(**data)
        return self._inspect_rule_based(input_data)

    def _inspect_rule_based(
        self, input_data: QualityControlInput
    ) -> QualityControlOutput:
        """ルールベースの品質検査."""
        out_of_spec: list[str] = []

        # 仕様との比較
        for param, value in input_data.measurements.items():
            if param in input_data.specifications:
                spec = input_data.specifications[param]
                if value < spec.get("min", float("-inf")):
                    out_of_spec.append(f"{param}: {value} < {spec['min']}")
                elif value > spec.get("max", float("inf")):
                    out_of_spec.append(f"{param}: {value} > {spec['max']}")

        result = "PASS" if not out_of_spec else "FAIL"
        score = 100 - len(out_of_spec) * 20

        return QualityControlOutput(
            product_id=input_data.product_id,
            inspection_result=result,
            quality_score=max(score, 0),
            defects=[],
            out_of_spec=out_of_spec,
            corrective_actions=["工程パラメータの見直し"] if out_of_spec else [],
            recommendation="継続的な品質モニタリングを推奨",
        )

    def validate_output(self, output: QualityControlOutput) -> bool:
        """出力検証."""
        return output.inspection_result in ["PASS", "FAIL", "CONDITIONAL"]


class QualityControlTemplate(AgentTemplate):
    """品質管理Agentテンプレート."""

    template_id: str = "quality_control"
    name: str = "品質管理Agent"
    description: str = "製造工程の品質管理を行うAgent"
    industry: IndustryType = IndustryType.MANUFACTURING
    category: TemplateCategory = TemplateCategory.QUALITY_CONTROL
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="inspection_types",
            description="検査タイプ",
            param_type="list",
            default=["VISUAL", "DIMENSIONAL"],
        ),
        TemplateParameter(
            name="defect_threshold",
            description="欠陥閾値",
            param_type="int",
            default=3,
        ),
    ]
    default_config: TemplateConfig = TemplateConfig(
        model="gpt-4o",
        temperature=0.2,
        max_tokens=2048,
    )
    metadata: TemplateMetadata = TemplateMetadata(
        author="AgentFlow Team",
        version="1.0.0",
        tags=["manufacturing", "quality", "inspection"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> QualityControlAgent:
        """品質管理Agentを生成."""
        merged_config = self.merge_config(config)
        agent = QualityControlAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
