"""人間確認ポリシー.

重要指摘の判定ルールを設定ファイルから読み込み、
review.findings に `requires_human_review` を付与する。
"""

from __future__ import annotations

from functools import lru_cache
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field


POLICY_PATH = Path(__file__).parent.parent / "config" / "human_review_policy.yaml"


class HumanReviewPolicy(BaseModel):
    """人間確認ポリシー設定."""

    version: str = "1.0.0"
    signable_confidence_threshold_pct: int = Field(
        default=39,
        ge=0,
        le=100,
        description="署名可能と判断する分析信頼度の閾値（%）",
    )
    important_severities: list[str] = Field(default_factory=lambda: ["CRITICAL", "WARNING"])
    mandatory_categories: list[str] = Field(default_factory=lambda: ["RESPONSIBILITY_GAP"])
    important_hint: str = "重要指摘です。人間確認コメントを入力して妥当性を再判定してください。"

    def is_important(self, finding: dict[str, Any]) -> bool:
        """所見が人間確認必須か判定."""
        severity = str(finding.get("severity", "")).upper()
        category = str(finding.get("category", "")).upper()
        if severity in {item.upper() for item in self.important_severities}:
            return True
        return category in {item.upper() for item in self.mandatory_categories}


@lru_cache(maxsize=1)
def load_human_review_policy() -> HumanReviewPolicy:
    """設定ファイルからポリシーを読み込み."""
    if not POLICY_PATH.exists():
        return HumanReviewPolicy()

    with open(POLICY_PATH, encoding="utf-8") as file:
        payload = yaml.safe_load(file) or {}
    return HumanReviewPolicy(**payload)


def enrich_review_with_policy(review_data: Any) -> dict[str, Any]:
    """review データに `requires_human_review` を付与."""
    if review_data is None:
        review = {}
    elif hasattr(review_data, "model_dump"):
        review = review_data.model_dump()
    elif isinstance(review_data, dict):
        review = dict(review_data)
    else:
        review = {}
    findings = review.get("findings", [])
    if not isinstance(findings, list):
        review["findings"] = []
        return review

    policy = load_human_review_policy()
    enriched_findings: list[dict[str, Any]] = []
    for raw_item in findings:
        item = dict(raw_item) if isinstance(raw_item, dict) else {"description": str(raw_item)}
        item["requires_human_review"] = policy.is_important(item)
        if item["requires_human_review"]:
            item["human_review_hint"] = policy.important_hint
        enriched_findings.append(item)

    review["findings"] = enriched_findings
    return review
