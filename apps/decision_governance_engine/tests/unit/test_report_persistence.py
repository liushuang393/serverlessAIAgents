from __future__ import annotations

from apps.decision_governance_engine.routers.decision import _extract_results_for_history


def test_extract_results_for_history_prefers_v3_keys() -> None:
    result = {
        "dao": {"essence": "E"},
        "fa": {"recommended_paths": []},
        "shu": {"first_action": "A"},
        "qi": {"implementations": []},
        "review": {"overall_verdict": "PASS"},
        "dao_analysis": {"essence": "OLD"},
    }

    extracted = _extract_results_for_history(result)
    assert extracted["dao"] == {"essence": "E"}
    assert extracted["fa"] == {"recommended_paths": []}
    assert extracted["shu"] == {"first_action": "A"}
    assert extracted["qi"] == {"implementations": []}
    assert extracted["review"]["overall_verdict"] == "PASS"
    assert extracted["review"]["findings"] == []


def test_extract_results_for_history_supports_legacy_analysis_keys() -> None:
    result = {
        "dao_analysis": {"essence": "E"},
        "fa_analysis": {"recommended_paths": []},
        "shu_analysis": {"first_action": "A"},
        "qi_analysis": {"implementations": []},
        "review": {"overall_verdict": "PASS"},
    }

    extracted = _extract_results_for_history(result)
    assert extracted["dao"] == {"essence": "E"}
    assert extracted["fa"] == {"recommended_paths": []}
    assert extracted["shu"] == {"first_action": "A"}
    assert extracted["qi"] == {"implementations": []}
    assert extracted["review"]["overall_verdict"] == "PASS"
    assert extracted["review"]["findings"] == []


def test_extract_results_for_history_includes_gate_sections_when_present() -> None:
    result = {
        "cognitive_gate": {"complexity": "high"},
        "gatekeeper": {"is_acceptable": True},
        "dao": {"essence": "E"},
        "fa": {"recommended_paths": []},
        "shu": {"first_action": "A"},
        "qi": {"implementations": []},
        "review": {"overall_verdict": "PASS"},
    }

    extracted = _extract_results_for_history(result)
    assert extracted["cognitive_gate"] == {"complexity": "high"}
    assert extracted["gatekeeper"] == {"is_acceptable": True}
