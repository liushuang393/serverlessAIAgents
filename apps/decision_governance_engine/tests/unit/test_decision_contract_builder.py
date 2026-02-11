"""DecisionGovContractBuilder のユニットテスト."""

from apps.decision_governance_engine.schemas.contract_schemas import DecisionRole
from apps.decision_governance_engine.services.decision_contract_builder import (
    DecisionGovContractBuilder,
)


def test_infer_decision_role_prefers_scoring_verdict() -> None:
    """scoring.verdict がある場合は最優先で採用する."""
    report = {
        "review": {"overall_verdict": "PASS"},
        "scoring": {"verdict": "NO_GO"},
        "fa": {
            "recommended_paths": [
                {"success_probability": 0.95},
            ]
        },
    }

    role = DecisionGovContractBuilder.infer_decision_role(report)
    assert role == DecisionRole.NO_GO


def test_build_from_report_includes_scoring_payload() -> None:
    """契約レスポンスに scoring フィールドを含む."""
    report = {
        "report_id": "PROP-202602-ABC123",
        "question": "新規事業へ投資すべきかを判断したい",
        "review": {"overall_verdict": "PASS"},
        "scoring": {"verdict": "PILOT", "weighted_score": 3.2},
        "executive_summary": {"one_line_decision": "段階投資で検証を先行"},
    }

    contract = DecisionGovContractBuilder.build_from_report(report)
    assert contract.scoring["verdict"] == "PILOT"
    assert contract.decision_role == DecisionRole.PILOT
