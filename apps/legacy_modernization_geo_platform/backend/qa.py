"""GEO-specific quality checks used before publishing."""

from __future__ import annotations

import re

from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentDraftArtifact,
    EvidenceMatrixArtifact,
    GeoQAReport,
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
        unknown_count = float(len(draft.unknowns) + len(evidence_matrix.unknowns))
        has_numeric_claims = bool(re.search(r"\d", body_text))
        has_comparison_claims = "比較" in body_text or "vs" in body_text.lower()

        issues: list[str] = []
        fix_instructions: list[str] = []
        if evidence_count < 3:
            issues.append("公開判定に必要な根拠数が不足しています。")
            fix_instructions.append("追加の外部根拠を最低3件以上収集してください。")
        if citation_ready_ratio < 0.7:
            issues.append("引用準備完了率が基準未満です。")
            fix_instructions.append("URL・要約・抜粋が揃った証拠を補強してください。")
        if fresh_ratio < 0.4:
            issues.append("新鮮な根拠の比率が低すぎます。")
            fix_instructions.append("直近30日以内の情報源を追加してください。")
        if unknown_count > 0:
            issues.append("未解決の unknowns が残っています。")
            fix_instructions.append("不明点を解消するか、仮説として明示してください。")
        if has_numeric_claims:
            issues.append("数値表現を含むため人間レビューが必要です。")
            fix_instructions.append("数値の引用元を本文と構造化データに明示してください。")
        if has_comparison_claims:
            issues.append("比較表現を含むためレビューを推奨します。")
            fix_instructions.append("比較主張の根拠と対象条件を明示してください。")

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

