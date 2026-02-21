"""Success-first scoring engine for Evolution V2."""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(slots=True)
class SuccessFirstScoringEngine:
    """Implements the fixed score formula from Evolution V2 plan."""

    def compute_final_score(
        self,
        *,
        success_rate_30d: float,
        success_rate_7d: float,
        reuse: float,
        freshness: float,
        condition_match: float,
        cost_efficiency: float,
        suspicion_penalty: float,
    ) -> float:
        """Compute final score in [0, 1] range."""
        raw = (
            0.50 * self._clip(success_rate_30d)
            + 0.15 * self._clip(success_rate_7d)
            + 0.10 * self._clip(reuse)
            + 0.10 * self._clip(freshness)
            + 0.10 * self._clip(condition_match)
            + 0.05 * self._clip(cost_efficiency)
            - max(0.0, suspicion_penalty)
        )
        return self._clip(raw)

    def should_mark_suspect(
        self,
        *,
        failure_streak: int,
        success_7d: float,
        success_30d: float,
        environment_mismatch: bool,
        last_verified_age_days: int,
        max_age_days: int = 30,
    ) -> bool:
        """Apply suspect trigger conditions."""
        if failure_streak >= 2:
            return True

        if success_30d > 0 and success_7d < success_30d * 0.8:
            return True

        if environment_mismatch:
            return True

        return last_verified_age_days > max_age_days

    def freshness_decay(self, last_verified_age_days: int, max_age_days: int = 30) -> float:
        """Convert age days to freshness decay factor."""
        if last_verified_age_days <= 0:
            return 1.0
        if last_verified_age_days >= max_age_days:
            return 0.1
        ratio = 1.0 - (last_verified_age_days / max_age_days)
        return self._clip(max(0.1, ratio))

    @staticmethod
    def _clip(value: float) -> float:
        return max(0.0, min(1.0, value))
