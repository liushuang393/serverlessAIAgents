"""ReportGenerator の scoring 連携テスト."""

from apps.decision_governance_engine.services.report_generator import ReportGenerator


def test_report_generator_embeds_scoring_result() -> None:
    """generate() が DecisionReport.scoring を埋める."""
    generator = ReportGenerator()
    results = {
        "dao": {
            "problem_type": "TRADE_OFF",
            "death_traps": [],
            "causal_gears": [{"gear_id": 1}],
            "existing_alternatives": [{"name": "内製"}],
        },
        "fa": {
            "recommended_paths": [
                {
                    "path_id": "A",
                    "name": "段階投資",
                    "description": "PoC を優先",
                    "pros": ["柔軟性が高い"],
                    "cons": ["即効性は限定的"],
                    "success_probability": 0.68,
                    "strategy_type": "CONSERVATIVE",
                    "reversibility": "HIGH",
                }
            ]
        },
        "shu": {
            "phases": [{"duration": "2ヶ月"}, {"duration": "1ヶ月"}],
            "first_action": "関係者合意を取得する",
            "dependencies": ["法務"],
        },
        "qi": {
            "implementations": [{"component": "API"}],
            "integration_points": ["既存DB"],
            "technical_debt_warnings": [],
            "domain_technologies": [{"technology_name": "FastAPI"}],
            "regulatory_considerations": [],
            "geographic_considerations": [],
        },
        "review": {
            "overall_verdict": "PASS",
            "findings": [],
            "confidence_score": 0.78,
            "final_warnings": [],
        },
    }

    report = generator.generate(results=results, original_question="新規事業への投資判断をしたい")
    assert report.scoring is not None
    assert report.scoring.get("verdict") in {"GO", "PILOT", "DELAY", "NO_GO"}
