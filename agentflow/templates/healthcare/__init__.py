# -*- coding: utf-8 -*-
"""医療業界Agentテンプレート.

医療業界向けの専門Agentテンプレートを提供。

テンプレート一覧:
- DiagnosticSupportTemplate: 診断支援Agent
- TreatmentPlanningTemplate: 治療計画Agent
- PatientAnalysisTemplate: 患者データ分析Agent
"""

from agentflow.templates.healthcare.diagnostic_support_template import (
    DiagnosticSupportTemplate,
)
from agentflow.templates.healthcare.patient_analysis_template import (
    PatientAnalysisTemplate,
)
from agentflow.templates.healthcare.treatment_planning_template import (
    TreatmentPlanningTemplate,
)

__all__ = [
    "DiagnosticSupportTemplate",
    "TreatmentPlanningTemplate",
    "PatientAnalysisTemplate",
]


def register_all() -> None:
    """全医療テンプレートを登録."""
    from agentflow.templates import register_template

    register_template(DiagnosticSupportTemplate())
    register_template(TreatmentPlanningTemplate())
    register_template(PatientAnalysisTemplate())

