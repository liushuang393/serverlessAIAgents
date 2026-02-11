"""DecisionScoringService のユニットテスト."""

from apps.decision_governance_engine.services.decision_scoring_service import (
    score_decision_results,
)


def _build_sample_results() -> dict:
    return {
        "dao": {
            "death_traps": [],
            "causal_gears": [{"gear_id": 1}, {"gear_id": 2}],
            "existing_alternatives": [{"name": "A"}],
        },
        "fa": {
            "recommended_paths": [
                {
                    "name": "段階投資",
                    "success_probability": 0.72,
                    "pros": ["学習効果が高い", "初期投資を抑制"],
                    "cons": ["速度がやや遅い"],
                    "strategy_type": "CONSERVATIVE",
                    "reversibility": "HIGH",
                }
            ]
        },
        "shu": {
            "phases": [
                {"duration": "2ヶ月"},
                {"duration": "1ヶ月"},
            ],
            "dependencies": ["法務", "営業"],
            "first_action": "30分のキックオフを設定する",
        },
        "qi": {
            "implementations": [{"component": "API"}],
            "integration_points": ["CRM 連携"],
            "technical_debt_warnings": [],
            "domain_technologies": [{"name": "WebRTC"}],
            "regulatory_considerations": [{"region": "JP"}],
            "geographic_considerations": [{"region": "APAC"}],
        },
        "review": {
            "confidence_score": 0.81,
            "findings": [],
        },
    }


def test_score_decision_results_returns_scoring_payload() -> None:
    """通常入力で scoring ペイロードが生成される."""
    payload = score_decision_results(_build_sample_results())

    assert payload is not None
    assert "weighted_score" in payload
    assert "verdict" in payload
    assert "dimension_scores" in payload
    assert "raw_dimension_scores" in payload
    assert len(payload["raw_dimension_scores"]) == 8


def test_score_decision_results_no_go_when_critical_risk_high() -> None:
    """重大指摘が多い場合は NO_GO に寄る."""
    sample = _build_sample_results()
    sample["dao"]["death_traps"] = [
        {"action": "致命的な規制違反"},
        {"action": "撤退不能な固定投資"},
    ]
    sample["review"]["findings"] = [
        {"severity": "CRITICAL"},
        {"severity": "WARNING"},
    ]

    payload = score_decision_results(sample)
    assert payload is not None
    assert payload["verdict"] in {"NO_GO", "DELAY"}
