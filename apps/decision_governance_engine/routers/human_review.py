"""人間確認フロー API.

目的:
    - 決定レポートの承認/却下
    - 確認状態の更新
    - 確認履歴の取得

作成日: 2026-02-10
"""

from __future__ import annotations

import asyncio
import json
import logging
from datetime import UTC, datetime
from typing import Any
from uuid import UUID

from apps.decision_governance_engine.agents.review_agent import ReviewAgent
from apps.decision_governance_engine.repositories import DecisionRepository
from apps.decision_governance_engine.routers.report import _get_report_from_db, cache_report
from apps.decision_governance_engine.schemas.agent_schemas import (
    FindingCategory,
    FindingSeverity,
    ReviewFinding,
    ReviewOutput,
)
from apps.decision_governance_engine.schemas.output_schemas import HumanReview
from apps.decision_governance_engine.services.human_review_policy import (
    enrich_review_with_policy,
)
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from agentflow.providers import get_llm
from agentflow.utils import extract_json


logger = logging.getLogger(__name__)
DB_IO_TIMEOUT_SECONDS = 2.0

router = APIRouter(prefix="/human-review", tags=["human-review"])


class ReviewApprovalRequest(BaseModel):
    """確認承認リクエスト."""

    report_id: str = Field(..., description="レポートID")
    request_id: str | None = Field(default=None, description="履歴リクエストID（UUID）")
    approved: bool = Field(..., description="承認/却下")
    reviewer_name: str = Field(..., description="確認者名")
    reviewer_email: str | None = Field(default=None, description="確認者メール")
    review_notes: str | None = Field(default=None, max_length=500, description="確認コメント")


class ReviewApprovalResponse(BaseModel):
    """確認承認レスポンス."""

    success: bool = Field(..., description="成功フラグ")
    report_id: str = Field(..., description="レポートID")
    human_review: HumanReview = Field(..., description="更新後の確認情報")
    message: str = Field(..., description="メッセージ")


class FindingRecheckRequest(BaseModel):
    """重要指摘の再確認リクエスト."""

    report_id: str = Field(..., description="レポートID")
    finding_index: int = Field(..., ge=0, description="確認対象の指摘インデックス")
    confirmation_note: str = Field(..., min_length=10, max_length=2000, description="人間確認コメント")
    acknowledged: bool = Field(..., description="確認チェックボックス")
    request_id: str | None = Field(default=None, description="履歴リクエストID（UUID）")
    reviewer_name: str | None = Field(default=None, description="確認者名")


class FindingRecheckResponse(BaseModel):
    """重要指摘の再確認レスポンス."""

    success: bool = Field(..., description="API処理成功可否")
    resolved: bool = Field(..., description="指摘の解消判定")
    message: str = Field(..., description="結果メッセージ")
    issues: list[str] = Field(default_factory=list, description="不足点")
    updated_review: dict[str, Any] | None = Field(
        default=None,
        description="更新後 review 情報（解消時のみ）",
    )


class FindingNoteRequest(BaseModel):
    """重要指摘メモ（任意）保存リクエスト."""

    report_id: str = Field(..., description="レポートID")
    finding_index: int = Field(..., ge=0, description="対象の指摘インデックス")
    acknowledged: bool = Field(default=False, description="ユーザー確認チェック（任意）")
    memo: str | None = Field(
        default=None,
        max_length=2000,
        description="ユーザーメモ（任意）",
    )
    request_id: str | None = Field(default=None, description="履歴リクエストID（UUID）")
    reviewer_name: str | None = Field(default=None, description="確認者名")


class FindingNoteResponse(BaseModel):
    """重要指摘メモ保存レスポンス."""

    success: bool = Field(..., description="保存成功可否")
    message: str = Field(..., description="結果メッセージ")


# インメモリストレージ（本番環境では DB を使用）
_review_storage: dict[str, HumanReview] = {}


@router.post("/approve", response_model=ReviewApprovalResponse)
async def approve_decision(request: ReviewApprovalRequest) -> ReviewApprovalResponse:
    """決定レポートを承認/却下.

    Args:
        request: 承認リクエスト

    Returns:
        ReviewApprovalResponse: 承認結果

    Raises:
        HTTPException: レポートが見つからない場合
    """
    logger.info(
        f"人間確認リクエスト: report_id={request.report_id}, "
        f"approved={request.approved}, reviewer={request.reviewer_name}"
    )

    # 確認情報を作成
    human_review = HumanReview(
        requires_review=False,  # 確認済み
        approved=request.approved,
        reviewer_name=request.reviewer_name,
        reviewer_email=request.reviewer_email,
        review_notes=request.review_notes,
        reviewed_at=datetime.now(UTC).isoformat(),
    )

    # ストレージに保存
    _review_storage[request.report_id] = human_review

    await _persist_human_review_record(
        request_id_text=request.request_id,
        report_id_text=request.report_id,
        review_record={
            "event_type": "human_review_approval",
            "report_id": request.report_id,
            "approved": request.approved,
            "reviewer_name": request.reviewer_name,
            "reviewer_email": request.reviewer_email,
            "review_notes": request.review_notes,
            "reviewed_at": human_review.reviewed_at,
        },
    )

    message = "承認されました" if request.approved else "却下されました"

    return ReviewApprovalResponse(
        success=True,
        report_id=request.report_id,
        human_review=human_review,
        message=message,
    )


@router.get("/status/{report_id}", response_model=HumanReview)
async def get_review_status(report_id: str) -> HumanReview:
    """確認状態を取得.

    Args:
        report_id: レポートID

    Returns:
        HumanReview: 確認情報

    Raises:
        HTTPException: レポートが見つからない場合
    """
    if report_id not in _review_storage:
        raise HTTPException(status_code=404, detail=f"レポート {report_id} が見つかりません")

    return _review_storage[report_id]


@router.get("/pending", response_model=list[str])
async def get_pending_reviews() -> list[str]:
    """未確認のレポートIDリストを取得.

    Returns:
        list[str]: 未確認レポートIDのリスト
    """
    return [
        report_id for report_id, review in _review_storage.items() if review.requires_review and review.approved is None
    ]


@router.delete("/reset/{report_id}")
async def reset_review(report_id: str) -> dict[str, Any]:
    """確認状態をリセット（テスト用）.

    Args:
        report_id: レポートID

    Returns:
        dict: 成功メッセージ
    """
    if report_id in _review_storage:
        del _review_storage[report_id]
        return {"success": True, "message": f"レポート {report_id} の確認状態をリセットしました"}

    raise HTTPException(status_code=404, detail=f"レポート {report_id} が見つかりません")


def _build_default_finding(raw: Any) -> ReviewFinding:
    """所見データを安全に ReviewFinding へ変換."""
    if isinstance(raw, ReviewFinding):
        return raw

    data = raw if isinstance(raw, dict) else {}
    severity = data.get("severity", "INFO")
    category = data.get("category", "LOGIC_FLAW")
    try:
        normalized_severity = FindingSeverity(severity)
    except ValueError:
        normalized_severity = FindingSeverity.INFO
    try:
        normalized_category = FindingCategory(category)
    except ValueError:
        normalized_category = FindingCategory.LOGIC_FLAW

    return ReviewFinding(
        severity=normalized_severity,
        category=normalized_category,
        description=str(data.get("description", "")),
        affected_agent=str(data.get("affected_agent", "ReviewAgent")),
        suggested_revision=str(data.get("suggested_revision", "")),
        requires_human_review=bool(data.get("requires_human_review", False)),
        human_review_hint=str(data.get("human_review_hint", "")) or None,
    )


def _parse_review_output(raw_review: Any) -> ReviewOutput:
    """review フィールドを ReviewOutput として正規化."""
    if isinstance(raw_review, ReviewOutput):
        return raw_review
    if not isinstance(raw_review, dict):
        msg = "review データが不正です"
        raise ValueError(msg)

    enriched_review = enrich_review_with_policy(raw_review)
    findings = [_build_default_finding(item) for item in enriched_review.get("findings", [])]
    raw_confidence = enriched_review.get("confidence_score", 0.0)
    try:
        confidence = float(raw_confidence)
    except (TypeError, ValueError):
        confidence = 0.0
    return ReviewOutput(
        overall_verdict=enriched_review.get("overall_verdict", "REVISE"),
        findings=findings,
        confidence_score=confidence,
        final_warnings=[str(item) for item in enriched_review.get("final_warnings", []) if isinstance(item, str)],
    )


def _resolve_report_review(report: Any) -> ReviewOutput:
    """レポートから review データを取得."""
    if hasattr(report, "review"):
        return _parse_review_output(report.review)
    if isinstance(report, dict):
        return _parse_review_output(report.get("review", {}))

    msg = "レポート形式が不正です"
    raise ValueError(msg)


async def _evaluate_resolution_with_ai(
    finding: ReviewFinding,
    confirmation_note: str,
) -> tuple[bool, list[str]]:
    """指摘解消の妥当性を AI で判定."""
    prompt = f"""あなたは厳格なレビュー担当です。
以下の指摘に対して、人間の確認コメントが妥当かを判定してください。

【指摘内容】
- 重大度: {finding.severity.value}
- カテゴリ: {finding.category.value}
- 説明: {finding.description}
- 修正提案: {finding.suggested_revision}

【人間確認コメント】
{confirmation_note}

次のJSONのみ返してください:
{{
  "resolved": true/false,
  "issues": ["不足点1", "不足点2"]
}}"""

    try:
        llm_client = get_llm()
    except Exception as exc:
        logger.warning(f"LLM 初期化失敗のためルール判定へフォールバック: {exc}")
        llm_client = None

    if llm_client is None:
        return _evaluate_resolution_fallback(finding, confirmation_note)

    reviewer = ReviewAgent(llm_client=llm_client)
    try:
        response = await reviewer._call_llm(prompt)
        parsed = extract_json(response)
        if not isinstance(parsed, dict):
            return _evaluate_resolution_fallback(finding, confirmation_note)

        resolved = bool(parsed.get("resolved", False))
        issues_raw = parsed.get("issues", [])
        issues = [str(item) for item in issues_raw if isinstance(item, str)]
        if not resolved and not issues:
            issues = ["確認内容が指摘の解消条件を満たしていません。"]
        return resolved, issues[:3]
    except json.JSONDecodeError:
        return _evaluate_resolution_fallback(finding, confirmation_note)
    except Exception as exc:
        logger.warning(f"LLM 再確認判定に失敗したためフォールバック: {exc}")
        return _evaluate_resolution_fallback(finding, confirmation_note)


def _evaluate_resolution_fallback(
    finding: ReviewFinding,
    confirmation_note: str,
) -> tuple[bool, list[str]]:
    """LLM 利用不可時の簡易判定."""
    note = confirmation_note.strip()
    if len(note) < 30:
        return False, ["確認コメントが短すぎます。対応内容と判断理由を具体的に記載してください。"]

    required_keywords = ["対応", "責任", "期限", "承認", "確認", "実施"]
    if not any(keyword in note for keyword in required_keywords):
        return False, ["誰が・何を・いつまでに対応するかを明記してください。"]

    if finding.category == FindingCategory.RESPONSIBILITY_GAP and "RACI" not in note:
        return False, ["責任分担（RACI または同等の役割定義）を明記してください。"]

    return True, []


def _resolve_request_id(candidate: str | None) -> UUID | None:
    """文字列から UUID を安全に復元."""
    if not candidate:
        return None
    try:
        return UUID(candidate)
    except (ValueError, TypeError):
        return None


async def _resolve_request_id_from_report(report_id: str | None) -> UUID | None:
    """report_id から request_id を逆引き."""
    if not report_id:
        return None
    try:
        repo = DecisionRepository()
        record = await asyncio.wait_for(
            repo.find_by_report_case_id(report_id),
            timeout=DB_IO_TIMEOUT_SECONDS,
        )
        if record is None:
            return None
        return record.request_id
    except TimeoutError:
        logger.warning(f"report_id 逆引きがタイムアウト: report_id={report_id}")
        return None
    except Exception as exc:
        logger.warning(f"report_id 逆引きに失敗: report_id={report_id}, error={exc}")
        return None


async def _persist_human_review_record(
    request_id_text: str | None,
    report_id_text: str | None,
    review_record: dict[str, Any],
    updated_review: dict[str, Any] | None = None,
) -> None:
    """人間確認ログを DB に保存."""
    request_uuid = _resolve_request_id(request_id_text)
    if request_uuid is None:
        request_uuid = await _resolve_request_id_from_report(report_id_text)
    if request_uuid is None:
        logger.info("人間確認ログ保存をスキップ: request_id を特定できません")
        return

    try:
        repo = DecisionRepository()
        stored = await asyncio.wait_for(
            repo.append_human_review_record(
                request_id=request_uuid,
                review_record=review_record,
                updated_review=updated_review,
            ),
            timeout=DB_IO_TIMEOUT_SECONDS,
        )
        if not stored:
            logger.warning(f"人間確認ログ保存対象が見つかりません: request_id={request_uuid}")
    except TimeoutError:
        logger.warning(f"人間確認ログ保存がタイムアウト: request_id={request_uuid}")
    except Exception as exc:
        logger.warning(f"人間確認ログ保存に失敗: {exc}")


@router.post("/recheck-finding", response_model=FindingRecheckResponse)
async def recheck_finding(request: FindingRecheckRequest) -> FindingRecheckResponse:
    """重要指摘に対する人間確認を再評価し、review を更新."""
    if not request.acknowledged:
        raise HTTPException(status_code=400, detail="確認チェックボックスをオンにしてください")

    report = await _get_report_from_db(request.report_id)
    if report is None:
        raise HTTPException(
            status_code=404, detail=f"レポート {request.report_id} が見つかりません"
        )

    try:
        review = _resolve_report_review(report)
    except ValueError as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc

    if request.finding_index >= len(review.findings):
        raise HTTPException(status_code=400, detail="finding_index が範囲外です")

    target_finding = review.findings[request.finding_index]
    resolved, issues = await _evaluate_resolution_with_ai(
        finding=target_finding,
        confirmation_note=request.confirmation_note,
    )

    if not resolved:
        await _persist_human_review_record(
            request_id_text=request.request_id,
            report_id_text=request.report_id,
            review_record={
                "event_type": "finding_recheck",
                "resolved": False,
                "report_id": request.report_id,
                "finding_index": request.finding_index,
                "finding": target_finding.model_dump(),
                "confirmation_note": request.confirmation_note,
                "acknowledged": request.acknowledged,
                "reviewer_name": request.reviewer_name,
                "issues": issues,
                "reviewed_at": datetime.now(UTC).isoformat(),
            },
        )
        return FindingRecheckResponse(
            success=True,
            resolved=False,
            message="確認内容が不十分です。問題点を補足して再提出してください。",
            issues=issues,
            updated_review=None,
        )

    remaining_findings = [finding for idx, finding in enumerate(review.findings) if idx != request.finding_index]
    verdict, confidence = ReviewAgent.derive_verdict_and_confidence(
        findings=remaining_findings,
    )

    updated_review = ReviewOutput(
        overall_verdict=verdict,
        findings=remaining_findings,
        confidence_score=confidence,
        final_warnings=[
            f.description for f in remaining_findings if f.severity != FindingSeverity.INFO
        ],
    )

    if hasattr(report, "review"):
        report.review = updated_review
        cache_report(request.report_id, report)
        if request.request_id:
            cache_report(request.request_id, report)
    elif isinstance(report, dict):
        report["review"] = updated_review.model_dump()
        cache_report(request.report_id, report)
        if request.request_id:
            cache_report(request.request_id, report)

    logger.info(
        f"人間確認で指摘を解消: report_id={request.report_id}, "
        f"finding_index={request.finding_index}, reviewer={request.reviewer_name}"
    )
    await _persist_human_review_record(
        request_id_text=request.request_id,
        report_id_text=request.report_id,
        review_record={
            "event_type": "finding_recheck",
            "resolved": True,
            "report_id": request.report_id,
            "finding_index": request.finding_index,
            "finding": target_finding.model_dump(),
            "confirmation_note": request.confirmation_note,
            "acknowledged": request.acknowledged,
            "reviewer_name": request.reviewer_name,
            "issues": [],
            "reviewed_at": datetime.now(UTC).isoformat(),
        },
        updated_review=updated_review.model_dump(),
    )
    return FindingRecheckResponse(
        success=True,
        resolved=True,
        message="確認内容が妥当と判断され、判定を再計算しました。",
        issues=[],
        updated_review=updated_review.model_dump(),
    )


@router.post("/log-finding-note", response_model=FindingNoteResponse)
async def log_finding_note(request: FindingNoteRequest) -> FindingNoteResponse:
    """重要指摘に対する任意メモを保存."""
    report = await _get_report_from_db(request.report_id)
    if report is None:
        raise HTTPException(
            status_code=404, detail=f"レポート {request.report_id} が見つかりません"
        )

    try:
        review = _resolve_report_review(report)
    except ValueError as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc

    if request.finding_index >= len(review.findings):
        raise HTTPException(status_code=400, detail="finding_index が範囲外です")

    target_finding = review.findings[request.finding_index]
    memo_text = (request.memo or "").strip()

    await _persist_human_review_record(
        request_id_text=request.request_id,
        report_id_text=request.report_id,
        review_record={
            "event_type": "finding_note",
            "report_id": request.report_id,
            "finding_index": request.finding_index,
            "finding": target_finding.model_dump(),
            "acknowledged": request.acknowledged,
            "memo": memo_text,
            "reviewer_name": request.reviewer_name,
            "reviewed_at": datetime.now(UTC).isoformat(),
        },
    )
    return FindingNoteResponse(
        success=True,
        message="メモを保存しました",
    )


__all__ = ["router"]
