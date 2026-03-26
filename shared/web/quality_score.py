"""品質スコア計算."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.web import EvidenceItem


def score_confidence(items: list[EvidenceItem], *, fallback_used: bool) -> float:
    """根拠とフォールバック情報から信頼度を算出する."""
    if not items:
        return 0.0
    avg_item_conf = sum(item.confidence for item in items) / len(items)
    coverage_bonus = min(len(items) * 0.08, 0.3)
    fallback_penalty = 0.1 if fallback_used else 0.0
    score = avg_item_conf + coverage_bonus - fallback_penalty
    return max(0.0, min(1.0, score))
