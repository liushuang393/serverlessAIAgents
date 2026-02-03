# -*- coding: utf-8 -*-
"""金融業界Agentテンプレート.

金融業界向けの専門Agentテンプレートを提供。

テンプレート一覧:
- RiskAssessmentTemplate: リスク評価Agent
- ComplianceCheckTemplate: コンプライアンスチェックAgent
- FinancialAnalysisTemplate: 財務分析Agent
"""

from agentflow.templates.finance.compliance_template import ComplianceCheckTemplate
from agentflow.templates.finance.financial_analysis_template import (
    FinancialAnalysisTemplate,
)
from agentflow.templates.finance.risk_assessment_template import RiskAssessmentTemplate

__all__ = [
    "RiskAssessmentTemplate",
    "ComplianceCheckTemplate",
    "FinancialAnalysisTemplate",
]


def register_all() -> None:
    """全金融テンプレートを登録."""
    from agentflow.templates import register_template

    register_template(RiskAssessmentTemplate())
    register_template(ComplianceCheckTemplate())
    register_template(FinancialAnalysisTemplate())

