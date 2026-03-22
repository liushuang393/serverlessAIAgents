"""GEO-specific quality checks used before publishing."""

from __future__ import annotations

import re

from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentDraftArtifact,
    EvidenceMatrixArtifact,
    GeoQAReport,
    normalize_content_language,
)


_QA_COPY: dict[str, dict[str, str]] = {
    "ja": {
        "issue.evidence_count": "公開判定に必要な根拠数が不足しています。",
        "fix.evidence_count": "追加の外部根拠を最低3件以上収集してください。",
        "issue.citation_ready": "引用準備完了率が基準未満です。",
        "fix.citation_ready": "URL・要約・抜粋が揃った証拠を補強してください。",
        "issue.fresh_ratio": "新鮮な根拠の比率が低すぎます。",
        "fix.fresh_ratio": "直近30日以内の情報源を追加してください。",
        "issue.unknowns": "未解決の unknowns が残っています。",
        "fix.unknowns": "不明点を解消するか、仮説として明示してください。",
        "issue.numeric": "数値表現を含むため人間レビューが必要です。",
        "fix.numeric": "数値の引用元を本文と構造化データに明示してください。",
        "issue.comparison": "比較表現を含むためレビューを推奨します。",
        "fix.comparison": "比較主張の根拠と対象条件を明示してください。",
    },
    "en": {
        "issue.evidence_count": "Insufficient evidence count for publish readiness.",
        "fix.evidence_count": "Collect at least three external evidence sources.",
        "issue.citation_ready": "Citation-ready ratio is below the threshold.",
        "fix.citation_ready": "Add evidence with URL, summary, and usable snippet.",
        "issue.fresh_ratio": "Fresh evidence ratio is too low.",
        "fix.fresh_ratio": "Add sources published within the last 30 days.",
        "issue.unknowns": "Unresolved unknowns are still present.",
        "fix.unknowns": "Resolve unknowns or state them explicitly as hypotheses.",
        "issue.numeric": "Numeric claims require human review before publishing.",
        "fix.numeric": "Attach citation sources for all numeric claims.",
        "issue.comparison": "Comparative claims require review before publishing.",
        "fix.comparison": "Clarify evidence and conditions for comparative claims.",
    },
    "zh": {
        "issue.evidence_count": "用于发布判定的依据数量不足。",
        "fix.evidence_count": "请补充至少 3 条外部依据。",
        "issue.citation_ready": "可引用依据占比低于阈值。",
        "fix.citation_ready": "请补齐具备 URL、摘要与摘录的依据。",
        "issue.fresh_ratio": "新鲜依据占比过低。",
        "fix.fresh_ratio": "请补充近 30 天内的信息来源。",
        "issue.unknowns": "仍存在未解决的 unknowns。",
        "fix.unknowns": "请消除不确定项或明确标注为假设。",
        "issue.numeric": "包含数值表述，发布前需要人工复核。",
        "fix.numeric": "请为数值结论补充可追溯引用来源。",
        "issue.comparison": "包含对比性表述，建议发布前复核。",
        "fix.comparison": "请明确对比结论的依据与适用条件。",
    },
}

_COMPARISON_KEYWORDS_GLOBAL: tuple[str, ...] = (
    "比較",
    "对比",
    "對比",
    "比较",
    "相比",
)
_COMPARISON_KEYWORDS_LOWER: tuple[str, ...] = (
    "vs",
    "versus",
    "compared to",
    "compared with",
)


class GeoQualityGate:
    """Apply citation, freshness, and risk checks to generated content."""

    def evaluate(
        self,
        *,
        task_id: str,
        draft: ContentDraftArtifact,
        evidence_matrix: EvidenceMatrixArtifact,
    ) -> GeoQAReport:
        """Run the quality evaluation and return a report."""
        entries = list(evidence_matrix.entries)
        evidence_count = float(len(entries))
        citation_ready_count = sum(1 for item in entries if item.citation_ready)
        fresh_count = sum(1 for item in entries if item.fresh)
        citation_ready_ratio = (citation_ready_count / evidence_count) if evidence_count else 0.0
        fresh_ratio = (fresh_count / evidence_count) if evidence_count else 0.0
        body_text = "\n".join(page.body_markdown for page in draft.pages)
        lower_body_text = body_text.lower()
        unknown_count = float(len(draft.unknowns) + len(evidence_matrix.unknowns))
        has_numeric_claims = bool(re.search(r"\d", body_text))
        has_comparison_claims = any(keyword in body_text for keyword in _COMPARISON_KEYWORDS_GLOBAL) or any(
            keyword in lower_body_text for keyword in _COMPARISON_KEYWORDS_LOWER
        )
        language = normalize_content_language(draft.target_language)
        copy = _QA_COPY[language]

        issues: list[str] = []
        fix_instructions: list[str] = []
        if evidence_count < 3:
            issues.append(copy["issue.evidence_count"])
            fix_instructions.append(copy["fix.evidence_count"])
        if citation_ready_ratio < 0.7:
            issues.append(copy["issue.citation_ready"])
            fix_instructions.append(copy["fix.citation_ready"])
        if fresh_ratio < 0.4:
            issues.append(copy["issue.fresh_ratio"])
            fix_instructions.append(copy["fix.fresh_ratio"])
        if unknown_count > 0:
            issues.append(copy["issue.unknowns"])
            fix_instructions.append(copy["fix.unknowns"])
        if has_numeric_claims:
            issues.append(copy["issue.numeric"])
            fix_instructions.append(copy["fix.numeric"])
        if has_comparison_claims:
            issues.append(copy["issue.comparison"])
            fix_instructions.append(copy["fix.comparison"])

        risk_level = "LOW"
        if has_numeric_claims or has_comparison_claims or unknown_count > 0 or citation_ready_ratio < 0.85:
            risk_level = "MEDIUM"
        if evidence_count == 0 or citation_ready_ratio < 0.4:
            risk_level = "HIGH"

        publish_ready = bool(draft.pages) and evidence_count > 0
        pass_or_fail = "PASS" if publish_ready and risk_level != "HIGH" else "REVIEW"
        return GeoQAReport(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:geo_qa", stage="geo_qa"),
            evidence=[item.model_dump(mode="json") for item in entries[:5]],
            unknowns=list(dict.fromkeys([*draft.unknowns, *evidence_matrix.unknowns])),
            pass_or_fail=pass_or_fail,
            issues=issues,
            fix_instructions=fix_instructions,
            publish_ready=publish_ready,
            risk_level=risk_level,
            metrics={
                "evidence_count": evidence_count,
                "citation_ready_ratio": round(citation_ready_ratio, 3),
                "fresh_ratio": round(fresh_ratio, 3),
                "unknown_count": unknown_count,
            },
        )
