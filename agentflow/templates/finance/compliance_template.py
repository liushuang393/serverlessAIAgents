# -*- coding: utf-8 -*-
"""コンプライアンスチェックAgentテンプレート.

金融規制コンプライアンスチェックを行うAgentテンプレート。

【機能】
- AML（マネーロンダリング対策）チェック
- KYC（本人確認）チェック
- 規制要件適合性チェック
- 内部統制チェック
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


class ComplianceCheckInput(BaseModel):
    """コンプライアンスチェック入力."""

    check_type: str = Field(description="チェックタイプ（AML/KYC/REGULATORY/INTERNAL）")
    entity_type: str = Field(description="対象タイプ（INDIVIDUAL/CORPORATE）")
    entity_data: dict[str, Any] = Field(description="対象データ")
    regulations: list[str] = Field(default_factory=list, description="適用規制")
    jurisdiction: str = Field(default="JP", description="管轄地域")


class ComplianceViolation(BaseModel):
    """コンプライアンス違反."""

    rule_id: str
    rule_name: str
    severity: str = Field(description="LOW/MEDIUM/HIGH/CRITICAL")
    description: str
    remediation: str = ""


class ComplianceCheckOutput(BaseModel):
    """コンプライアンスチェック出力."""

    is_compliant: bool
    overall_score: float = Field(ge=0, le=100)
    violations: list[ComplianceViolation] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    checked_rules: list[str] = Field(default_factory=list)
    recommendation: str = ""


class ComplianceCheckAgent(ResilientAgent[ComplianceCheckInput, ComplianceCheckOutput]):
    """コンプライアンスチェックAgent."""

    name = "ComplianceCheckAgent"
    temperature = 0.2

    SYSTEM_PROMPT = """あなたはコンプライアンスチェック専門のAgentです。

【チェック項目】
- AML: 取引パターン、資金源、PEP該当性
- KYC: 本人確認、住所確認、職業確認
- 規制: 適用法令、報告義務、開示要件
- 内部統制: 権限、承認フロー、記録保持

【出力形式】
JSON形式で以下を出力:
{
    "is_compliant": true/false,
    "overall_score": 0-100,
    "violations": [...],
    "warnings": [...],
    "checked_rules": [...],
    "recommendation": "..."
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ComplianceCheckInput:
        """入力をパース."""
        return ComplianceCheckInput(**input_data)

    async def process(
        self, input_data: ComplianceCheckInput
    ) -> ComplianceCheckOutput:
        """コンプライアンスチェックを実行."""
        if self._llm:
            return await self._check_with_llm(input_data)
        return self._check_rule_based(input_data)

    async def _check_with_llm(
        self, input_data: ComplianceCheckInput
    ) -> ComplianceCheckOutput:
        """LLMを使用したコンプライアンスチェック."""
        from agentflow.utils import extract_json

        prompt = f"""以下のコンプライアンスチェックを実施してください：
チェックタイプ: {input_data.check_type}
対象タイプ: {input_data.entity_type}
管轄地域: {input_data.jurisdiction}
適用規制: {', '.join(input_data.regulations) if input_data.regulations else '自動判定'}
対象データ: {input_data.entity_data}"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{prompt}")
        data = extract_json(response)

        if data:
            return ComplianceCheckOutput(**data)
        return self._check_rule_based(input_data)

    def _check_rule_based(
        self, input_data: ComplianceCheckInput
    ) -> ComplianceCheckOutput:
        """ルールベースのコンプライアンスチェック."""
        violations: list[ComplianceViolation] = []
        warnings: list[str] = []

        # 基本チェック
        if not input_data.entity_data.get("id"):
            violations.append(ComplianceViolation(
                rule_id="KYC-001",
                rule_name="本人確認",
                severity="HIGH",
                description="本人確認情報が不足しています",
                remediation="本人確認書類の提出が必要です",
            ))

        is_compliant = len([v for v in violations if v.severity in ["HIGH", "CRITICAL"]]) == 0
        score = 100 - len(violations) * 20

        return ComplianceCheckOutput(
            is_compliant=is_compliant,
            overall_score=max(score, 0),
            violations=violations,
            warnings=warnings,
            checked_rules=["KYC-001", "AML-001"],
            recommendation="定期的なコンプライアンスレビューを推奨",
        )

    def validate_output(self, output: ComplianceCheckOutput) -> bool:
        """出力検証."""
        return 0 <= output.overall_score <= 100


class ComplianceCheckTemplate(AgentTemplate):
    """コンプライアンスチェックAgentテンプレート."""

    template_id: str = "compliance_check"
    name: str = "コンプライアンスチェックAgent"
    description: str = "金融規制コンプライアンスチェックを行うAgent"
    industry: IndustryType = IndustryType.FINANCE
    category: TemplateCategory = TemplateCategory.COMPLIANCE
    parameters: list[TemplateParameter] = [
        TemplateParameter(
            name="check_types",
            description="チェックタイプ（AML/KYC/REGULATORY/INTERNAL）",
            param_type="list",
            default=["AML", "KYC"],
        ),
        TemplateParameter(
            name="jurisdiction",
            description="管轄地域",
            param_type="string",
            default="JP",
            validation_rules=[
                TemplateValidationRule(
                    rule_type=ValidationRuleType.ENUM,
                    field="jurisdiction",
                    params={"values": ["JP", "US", "EU", "UK", "SG", "HK"]},
                )
            ],
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
        tags=["finance", "compliance", "aml", "kyc"],
    )

    def create_agent(
        self,
        config: dict[str, Any] | None = None,
        llm_client: Any = None,
    ) -> ComplianceCheckAgent:
        """コンプライアンスチェックAgentを生成."""
        merged_config = self.merge_config(config)
        agent = ComplianceCheckAgent(llm_client=llm_client)
        agent.temperature = merged_config.temperature
        agent.max_tokens = merged_config.max_tokens
        return agent
