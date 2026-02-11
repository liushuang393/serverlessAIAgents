"""意思決定スコアリング統合サービス.

目的:
    既存の Dao/Fa/Shu/Qi/Review 出力を 8 次元評価へ写像し、
    ScoringEngine の入力に変換する。
"""

from __future__ import annotations

import logging
import re
from collections.abc import Mapping
from typing import Any

from pydantic import BaseModel

from apps.decision_governance_engine.config import get_config
from apps.decision_governance_engine.services.scoring_engine import ScoringEngine


logger = logging.getLogger(__name__)


def _to_mapping(data: Any) -> Mapping[str, Any]:
    """Pydantic/辞書を Mapping に正規化."""
    if isinstance(data, BaseModel):
        return data.model_dump()
    if isinstance(data, Mapping):
        return data
    return {}


def _clamp_score(value: float, min_score: float = 1.0, max_score: float = 5.0) -> float:
    """スコアを 1.0-5.0 に丸める."""
    return max(min_score, min(max_score, value))


def _safe_float(value: Any, default: float) -> float:
    """安全な float 変換."""
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _parse_duration_to_months(duration: Any) -> int:
    """期間文字列を概算月数へ変換."""
    if not isinstance(duration, str) or not duration.strip():
        return 1

    if week_match := re.search(r"(\d+)\s*(週|week)", duration, re.IGNORECASE):
        return max(1, int(week_match.group(1)) // 4)
    if month_match := re.search(r"(\d+)\s*(ヶ月|月|month)", duration, re.IGNORECASE):
        return max(1, int(month_match.group(1)))
    return 1


class DecisionScoringService:
    """意思決定結果を定量評価へ変換するサービス."""

    def __init__(self) -> None:
        self._config = get_config()
        self._engine = ScoringEngine(self._config)

    def score(self, results: Mapping[str, Any]) -> dict[str, Any]:
        """実行結果からスコアリングを実施."""
        dao = _to_mapping(results.get("dao", {}))
        fa = _to_mapping(results.get("fa", {}))
        shu = _to_mapping(results.get("shu", {}))
        qi = _to_mapping(results.get("qi", {}))
        review = _to_mapping(results.get("review", {}))

        recommended_paths = fa.get("recommended_paths", [])
        first_path = (
            recommended_paths[0]
            if isinstance(recommended_paths, list) and recommended_paths
            and isinstance(recommended_paths[0], Mapping)
            else {}
        )

        success_probability = _safe_float(first_path.get("success_probability"), 0.5)
        pros = first_path.get("pros", []) if isinstance(first_path.get("pros"), list) else []
        cons = first_path.get("cons", []) if isinstance(first_path.get("cons"), list) else []
        strategy_type = str(first_path.get("strategy_type", "")).upper()

        phases = shu.get("phases", []) if isinstance(shu.get("phases"), list) else []
        total_months = sum(
            _parse_duration_to_months(
                phase.get("duration", "") if isinstance(phase, Mapping) else ""
            )
            for phase in phases
        )
        dependencies = shu.get("dependencies", []) if isinstance(shu.get("dependencies"), list) else []
        first_action = str(shu.get("first_action", "")).strip()

        implementations = qi.get("implementations", []) if isinstance(qi.get("implementations"), list) else []
        integration_points = (
            qi.get("integration_points", [])
            if isinstance(qi.get("integration_points"), list) else []
        )
        debt_warnings = (
            qi.get("technical_debt_warnings", [])
            if isinstance(qi.get("technical_debt_warnings"), list) else []
        )
        domain_tech = (
            qi.get("domain_technologies", [])
            if isinstance(qi.get("domain_technologies"), list) else []
        )
        regulatory = (
            qi.get("regulatory_considerations", [])
            if isinstance(qi.get("regulatory_considerations"), list) else []
        )
        geographic = (
            qi.get("geographic_considerations", [])
            if isinstance(qi.get("geographic_considerations"), list) else []
        )

        findings = review.get("findings", []) if isinstance(review.get("findings"), list) else []
        critical_count = sum(
            1
            for finding in findings
            if isinstance(finding, Mapping)
            and str(finding.get("severity", "")).upper() == "CRITICAL"
        )
        warning_count = sum(
            1
            for finding in findings
            if isinstance(finding, Mapping)
            and str(finding.get("severity", "")).upper() == "WARNING"
        )
        death_traps = dao.get("death_traps", []) if isinstance(dao.get("death_traps"), list) else []
        causal_gears = dao.get("causal_gears", []) if isinstance(dao.get("causal_gears"), list) else []
        existing_alternatives = (
            dao.get("existing_alternatives", [])
            if isinstance(dao.get("existing_alternatives"), list) else []
        )

        reversibility_map = {"HIGH": 4.8, "MEDIUM": 3.4, "LOW": 2.0}
        reversibility_raw = reversibility_map.get(
            str(first_path.get("reversibility", "")).upper(),
            3.2,
        )

        user_impact = _clamp_score(1.0 + success_probability * 4.0)
        growth_potential = _clamp_score(
            1.2 + success_probability * 3.6 + (0.3 if strategy_type == "AGGRESSIVE" else 0.0)
        )
        if total_months <= 3:
            timing_window = 4.6
        elif total_months <= 6:
            timing_window = 4.0
        elif total_months <= 12:
            timing_window = 3.1
        elif total_months <= 18:
            timing_window = 2.4
        else:
            timing_window = 1.8
        if first_action:
            timing_window += 0.2
        timing_window = _clamp_score(timing_window)

        channel_reach = _clamp_score(3.0 + (len(pros) - len(cons)) * 0.3)
        cost = _clamp_score(
            1.8
            + len(implementations) * 0.25
            + len(debt_warnings) * 0.6
            + max(0, total_months - 6) * 0.08
        )
        risk = _clamp_score(
            1.6
            + critical_count * 1.8
            + warning_count * 0.5
            + len(death_traps) * 0.35
        )
        reversibility = _clamp_score(reversibility_raw - critical_count * 0.3)
        dependencies_complexity = _clamp_score(
            1.6 + len(dependencies) * 0.35 + len(integration_points) * 0.3
        )

        dimension_scores = {
            "user_impact": round(user_impact, 2),
            "growth_potential": round(growth_potential, 2),
            "timing_window": round(timing_window, 2),
            "channel_reach": round(channel_reach, 2),
            "cost": round(cost, 2),
            "risk": round(risk, 2),
            "reversibility": round(reversibility, 2),
            "dependencies": round(dependencies_complexity, 2),
        }

        confidence = _safe_float(review.get("confidence_score"), 0.6)
        confidence = max(0.0, min(1.0, confidence))

        evidence_signals = (
            len(domain_tech)
            + len(regulatory)
            + len(geographic)
            + len(causal_gears)
            + len(existing_alternatives)
        )
        evidence_coverage = max(0.2, min(1.0, evidence_signals / 10.0))

        scoring_result = self._engine.calculate_score(
            dimension_scores=dimension_scores,
            confidence=confidence,
            evidence_coverage=evidence_coverage,
        )
        payload = scoring_result.model_dump()
        payload["raw_dimension_scores"] = dimension_scores
        payload["active_preset"] = self._config.active_preset
        return payload


def score_decision_results(results: Mapping[str, Any]) -> dict[str, Any] | None:
    """意思決定結果をスコアリングし、失敗時は None を返す."""
    try:
        service = DecisionScoringService()
        return service.score(results)
    except Exception as exc:  # noqa: BLE001
        logger.warning(f"意思決定スコアリングに失敗: {exc}")
        return None


__all__ = ["DecisionScoringService", "score_decision_results"]
