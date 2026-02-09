"""Decision Governance Engine - 契約レスポンス組み立て.

目的:
    既存の DecisionReport(v3.x) から、外部連携用の字段级契约（DecisionGovResponseV1）を
    生成する。

入出力:
    入力:
        - DecisionReport（Pydantic）または dict
    出力:
        - DecisionGovResponseV1

注意点:
    - 本クラスは「後方互換」を優先し、未知フィールドは破棄しない（sections は dict として保持）。
    - decision_role は v1 の暫定ルールで推定する（将来 EvidenceCoverage 等で強化可能）。
"""

from __future__ import annotations

from collections.abc import Mapping
from datetime import datetime
from typing import Any

from apps.decision_governance_engine.schemas.contract_schemas import (
    Claim,
    ClaimType,
    DecisionGovResponseV1,
    DecisionMode,
    DecisionRole,
)
from pydantic import BaseModel


def _to_mapping(obj: Any) -> Mapping[str, Any]:
    """Pydantic/Dict を Mapping に正規化.

    Args:
        obj: Pydantic BaseModel または dict

    Returns:
        Mapping[str, Any]
    """

    if isinstance(obj, BaseModel):
        return obj.model_dump()
    if isinstance(obj, Mapping):
        return obj
    return {}


class DecisionGovContractBuilder:
    """DecisionGovResponseV1 生成器."""

    @staticmethod
    def infer_decision_role(report: Any) -> DecisionRole:
        """DecisionReport から decision_role を推定.

        ルール（v1・暫定）:
            - Review が REJECT -> NO_GO
            - Review が REVISE -> DELAY
            - Review が PASS:
                - success_probability >= 0.70 -> GO
                - success_probability >= 0.45 -> PILOT
                - その他 -> DELAY

        Args:
            report: DecisionReport または dict

        Returns:
            DecisionRole
        """

        data = _to_mapping(report)
        review = _to_mapping(data.get("review", {}))
        verdict = str(review.get("overall_verdict", "")).upper()
        if verdict == "REJECT":
            return DecisionRole.NO_GO
        if verdict == "REVISE":
            return DecisionRole.DELAY

        # Fa の成功確率を参照（存在しない/不正な場合は None）
        fa = _to_mapping(data.get("fa", {}))
        rec_paths = fa.get("recommended_paths") or []
        success_probability: float | None = None
        if isinstance(rec_paths, list) and rec_paths:
            first = rec_paths[0] if isinstance(rec_paths[0], Mapping) else {}
            try:
                success_probability = float(first.get("success_probability"))
            except (TypeError, ValueError):
                success_probability = None

        if verdict == "PASS":
            if success_probability is not None and success_probability >= 0.70:
                return DecisionRole.GO
            if success_probability is not None and success_probability >= 0.45:
                return DecisionRole.PILOT
            return DecisionRole.DELAY

        # 判定不明時は安全側に PILOT
        return DecisionRole.PILOT

    @staticmethod
    def build_from_report(
        report: Any,
        *,
        mode: DecisionMode = DecisionMode.STANDARD,
        request_id: str = "",
    ) -> DecisionGovResponseV1:
        """DecisionGovResponseV1 を生成.

        Args:
            report: DecisionReport または dict
            mode: FAST/STANDARD/AUDIT
            request_id: 上流が発行したリクエストID（未提供なら空）

        Returns:
            DecisionGovResponseV1
        """

        data = _to_mapping(report)

        report_id = str(data.get("report_id", ""))
        created_at_raw = data.get("created_at")
        created_at = (
            created_at_raw
            if isinstance(created_at_raw, datetime)
            else datetime.now()
        )

        decision_role = DecisionGovContractBuilder.infer_decision_role(data)

        executive_summary = _to_mapping(data.get("executive_summary", {}))
        summary_bullets: list[str] = []
        one_line = str(executive_summary.get("one_line_decision", "")).strip()
        if one_line:
            summary_bullets.append(one_line)
        rec_action = str(executive_summary.get("recommended_action", "")).strip()
        if rec_action:
            summary_bullets.append(f"推奨: {rec_action}")
        first_step = str(executive_summary.get("first_step", "")).strip()
        if first_step:
            summary_bullets.append(f"最初の一歩: {first_step}")

        warnings: list[str] = []
        # v1 は Evidence 未接続のため、空の可能性が高い。利用者に明示。
        warnings.append("外部証拠（evidence）が未接続のため、claims は暫定（推論）です。")

        claims: list[Claim] = []
        if one_line:
            claims.append(
                Claim(
                    claim_id="claim.exec.one_line_decision",
                    type=ClaimType.INFERENCE,
                    text=one_line,
                    confidence=0.6,
                    evidence_refs=[],
                    related_section="executive_summary",
                )
            )

        return DecisionGovResponseV1(
            report_id=report_id,
            request_id=request_id,
            mode=mode,
            question=str(data.get("original_question", data.get("question", ""))),
            decision_role=decision_role,
            created_at=created_at,
            summary_bullets=summary_bullets[:10],
            dao=dict(_to_mapping(data.get("dao", {}))),
            fa=dict(_to_mapping(data.get("fa", {}))),
            shu=dict(_to_mapping(data.get("shu", {}))),
            qi=dict(_to_mapping(data.get("qi", {}))),
            review=dict(_to_mapping(data.get("review", {}))),
            evidence=[],
            claims=claims,
            warnings=warnings,
        )


__all__ = ["DecisionGovContractBuilder"]
