"""Harness evaluation サービス."""

from __future__ import annotations

from typing import Any

from contracts.policy import EvalResult
from shared.registry import ComponentToggle


class EvaluationService:
    """簡易評価サービス."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        self._toggle = toggle or ComponentToggle()

    def evaluate(
        self,
        *,
        evaluator: str,
        passed: bool,
        score: float | None = None,
        reason: str = "",
        metadata: dict[str, Any] | None = None,
    ) -> EvalResult:
        """評価結果を返す."""
        if not self._toggle.enabled:
            return EvalResult(
                evaluator=evaluator,
                passed=True,
                score=score,
                reason="evaluation disabled",
                metadata={"implementation": "noop"},
            )
        return EvalResult(
            evaluator=evaluator,
            passed=passed,
            score=score,
            reason=reason,
            metadata=metadata or {},
        )
