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
from collections.abc import Mapping
from datetime import UTC, datetime
from typing import Any
from uuid import UUID

from apps.decision_governance_engine.agents.review_agent import ReviewAgent
from apps.decision_governance_engine.repositories import DecisionRepository
from apps.decision_governance_engine.routers.report import _get_report_from_db, cache_report
from apps.decision_governance_engine.schemas.agent_schemas import (
    CheckpointItem,
    FindingCategory,
    FindingSeverity,
    ReviewFinding,
    ReviewOutput,
    ReviewVerdict,
)
from apps.decision_governance_engine.schemas.output_schemas import HumanReview
from apps.decision_governance_engine.services.human_review_policy import (
    enrich_review_with_policy,
    load_human_review_policy,
)
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from agentflow.core.agent_factory import AgentFactorySpec
from agentflow.core.agent_factory import create as create_agent
from agentflow.providers import get_llm
from agentflow.utils import extract_json


logger = logging.getLogger(__name__)
DB_IO_TIMEOUT_SECONDS = 2.0
DEFAULT_FINDING_CHECK_BOOST = 5.0
MAX_LLM_BONUS_PCT = 15.0
METRIC_TARGET_CONFIDENCE = "confidence"
METRIC_TARGET_FEASIBILITY = "feasibility"
METRIC_TARGET_BOTH = "both"

router = APIRouter(prefix="/api/human-review", tags=["human-review"])


class ReviewApprovalRequest(BaseModel):
    """確認承認リクエスト."""

    report_id: str = Field(..., description="レポートID")
    request_id: str | None = Field(default=None, description="履歴リクエストID（UUID）")
    approved: bool = Field(..., description="承認/却下")
    reviewer_name: str = Field(..., description="確認者名")
    reviewer_email: str | None = Field(default=None, description="確認者メール")
    review_notes: str | None = Field(
        default=None, max_length=500, description="確認コメント"
    )


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
    confirmation_note: str = Field(
        ..., min_length=10, max_length=2000, description="人間確認コメント"
    )
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


class CheckpointApplyItem(BaseModel):
    """チェックポイント反映項目."""

    item_id: str = Field(..., description="チェックポイントID")
    checked: bool = Field(default=False, description="チェック状態")
    annotation: str | None = Field(default=None, max_length=50, description="注釈（任意）")


class FindingConfirmationItem(BaseModel):
    """指摘事項の確認反映項目."""

    finding_index: int = Field(..., ge=0, description="対象 finding のインデックス")
    checked: bool = Field(default=False, description="確認チェック状態")
    note: str | None = Field(default=None, max_length=2000, description="補足メモ（任意）")


class CheckpointApplyRequest(BaseModel):
    """チェックポイント反映リクエスト."""

    report_id: str = Field(..., description="レポートID")
    request_id: str | None = Field(default=None, description="履歴リクエストID（UUID）")
    items: list[CheckpointApplyItem] = Field(
        default_factory=list,
        description="反映するチェックポイント項目",
    )
    finding_confirmations: list[FindingConfirmationItem] = Field(
        default_factory=list,
        description="指摘事項ごとの確認チェックと補足メモ",
    )
    reviewer_name: str | None = Field(default=None, description="操作実行者（任意）")


class CheckpointApplyResponse(BaseModel):
    """チェックポイント反映レスポンス."""

    success: bool = Field(..., description="成功可否")
    message: str = Field(..., description="結果メッセージ")
    base_confidence_pct: int = Field(..., description="反映前の信頼度（%）")
    checkpoint_boost_pct: int = Field(..., description="チェックポイント固定加点（%）")
    finding_boost_pct: int = Field(..., description="指摘確認固定加点（%）")
    llm_bonus_pct: int = Field(..., description="補足メモのLLM加点（%）")
    bonus_reasons: list[str] = Field(default_factory=list, description="LLM加点の理由")
    recalculated_confidence_pct: int = Field(..., description="反映後の信頼度（%）")
    base_feasibility_pct: int = Field(..., description="反映前の戦略可行度（%）")
    checkpoint_feasibility_boost_pct: int = Field(..., description="戦略可行度へのチェックポイント加点（%）")
    finding_feasibility_boost_pct: int = Field(..., description="戦略可行度への指摘確認加点（%）")
    llm_feasibility_bonus_pct: int = Field(..., description="戦略可行度へのLLM加点（%）")
    recalculated_feasibility_pct: int = Field(..., description="反映後の戦略可行度（%）")
    threshold_pct: int = Field(..., description="署名可能閾値（%）")
    signature_eligible: bool = Field(..., description="署名可能か")
    applied_contributions: list[dict[str, Any]] = Field(
        default_factory=list,
        description="適用した加点内訳（confidence/feasibility/both）",
    )
    updated_review: dict[str, Any] = Field(..., description="更新後 review")


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
        raise HTTPException(
            status_code=404, detail=f"レポート {report_id} が見つかりません"
        )

    return _review_storage[report_id]


@router.get("/pending", response_model=list[str])
async def get_pending_reviews() -> list[str]:
    """未確認のレポートIDリストを取得.

    Returns:
        list[str]: 未確認レポートIDのリスト
    """
    return [
        report_id
        for report_id, review in _review_storage.items()
        if review.requires_review and review.approved is None
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
        failure_point=str(data.get("failure_point", "")),
        impact_scope=str(data.get("impact_scope", "")),
        minimal_patch=data.get("minimal_patch") if isinstance(data.get("minimal_patch"), dict) else None,
        score_improvements=data.get("score_improvements", []) if isinstance(data.get("score_improvements"), list) else [],
        action_type=data.get("action_type", "RECALC"),
    )


def _parse_review_output(raw_review: Any) -> ReviewOutput:
    """review フィールドを ReviewOutput として正規化."""
    if isinstance(raw_review, ReviewOutput):
        return raw_review
    if not isinstance(raw_review, dict):
        msg = "review データが不正です"
        raise ValueError(msg)

    enriched_review = enrich_review_with_policy(raw_review)
    findings = [
        _build_default_finding(item)
        for item in enriched_review.get("findings", [])
    ]
    raw_confidence = enriched_review.get("confidence_score", 0.0)
    try:
        confidence = float(raw_confidence)
    except (TypeError, ValueError):
        confidence = 0.0
    raw_checkpoint_items = enriched_review.get("checkpoint_items", [])
    checkpoint_items = (
        [item for item in raw_checkpoint_items if isinstance(item, dict)]
        if isinstance(raw_checkpoint_items, list)
        else []
    )
    raw_confidence_breakdown = enriched_review.get("confidence_breakdown")
    confidence_breakdown = (
        raw_confidence_breakdown
        if isinstance(raw_confidence_breakdown, dict)
        else None
    )

    return ReviewOutput(
        overall_verdict=enriched_review.get("overall_verdict", "REVISE"),
        findings=findings,
        confidence_score=confidence,
        final_warnings=[
            str(item)
            for item in enriched_review.get("final_warnings", [])
            if isinstance(item, str)
        ],
        confidence_breakdown=confidence_breakdown,
        checkpoint_items=checkpoint_items,
        auto_recalc_enabled=bool(enriched_review.get("auto_recalc_enabled", True)),
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

    reviewer = create_agent(
        AgentFactorySpec(
            class_name="ReviewAgent",
            module_path="apps.decision_governance_engine.agents.review_agent",
            init_kwargs={"llm_client": llm_client},
            agent_type="reviewer",
        )
    )
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
            logger.warning(f"人間確認ログ保存対象が見つかりません: request_id={request_uuid}。骨組み履歴を作成します。")

            seed_review: dict[str, Any] = {}
            if isinstance(updated_review, dict):
                seed_review = {**updated_review}
            seed_review.setdefault("human_review_records", [])
            fallback_question = f"[human-review] report_id={report_id_text or 'unknown'}"

            await asyncio.wait_for(
                repo.upsert_stage(
                    request_id=request_uuid,
                    question=fallback_question,
                    stage_name="review",
                    stage_result=seed_review,
                ),
                timeout=DB_IO_TIMEOUT_SECONDS,
            )

            stored = await asyncio.wait_for(
                repo.append_human_review_record(
                    request_id=request_uuid,
                    review_record=review_record,
                    updated_review=updated_review,
                ),
                timeout=DB_IO_TIMEOUT_SECONDS,
            )
            if stored:
                logger.info(f"人間確認ログ保存の自己修復に成功: request_id={request_uuid}")
            else:
                logger.warning(f"人間確認ログ保存の自己修復に失敗: request_id={request_uuid}")
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
        raise HTTPException(status_code=404, detail=f"レポート {request.report_id} が見つかりません")

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

    remaining_findings = [
        finding
        for idx, finding in enumerate(review.findings)
        if idx != request.finding_index
    ]
    verdict, confidence = ReviewAgent.derive_verdict_and_confidence(
        findings=remaining_findings,
    )

    updated_review = ReviewOutput(
        overall_verdict=verdict,
        findings=remaining_findings,
        confidence_score=confidence,
        final_warnings=[f.description for f in remaining_findings if f.severity != FindingSeverity.INFO],
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
        raise HTTPException(status_code=404, detail=f"レポート {request.report_id} が見つかりません")

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


def _extract_finding_boost_delta(finding: ReviewFinding) -> float:
    """所見チェック時の固定加点を算出."""
    positive_deltas = [
        float(item.delta)
        for item in finding.score_improvements
        if float(item.delta) > 0
    ]
    if not positive_deltas:
        return DEFAULT_FINDING_CHECK_BOOST
    return max(positive_deltas)


def _clamp_bonus_pct(value: float) -> float:
    """LLM加点を許容範囲へ丸める."""
    return max(0.0, min(MAX_LLM_BONUS_PCT, value))


def _clamp_pct(value: float) -> int:
    """百分率を 0-100 の整数へ丸める."""
    return max(0, min(100, round(value)))


def _safe_float(value: Any, default: float) -> float:
    """安全に float へ変換."""
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _to_mapping(data: Any) -> Mapping[str, Any]:
    """Pydantic/辞書を Mapping に正規化."""
    if hasattr(data, "model_dump"):
        dumped = data.model_dump()
        return dumped if isinstance(dumped, Mapping) else {}
    if isinstance(data, Mapping):
        return data
    return {}


def _normalize_str_list(value: Any) -> list[str]:
    """入力を文字列配列へ正規化."""
    if not isinstance(value, list):
        return []
    return [str(item).strip() for item in value if str(item).strip()]


def _infer_checkpoint_target_metric(item: CheckpointItem) -> str:
    """Checkpoint が影響する指標（confidence/feasibility/both）を推定."""
    component = (item.target_component or "").strip().lower()
    label = item.label.strip()

    if component in {"risk_coverage", "implementation_feasibility", "logic_consistency"}:
        return METRIC_TARGET_BOTH

    if "撤退" in label or "最悪ケース" in label or "ゲート" in label:
        return METRIC_TARGET_BOTH

    if component == "input_sufficiency":
        return METRIC_TARGET_CONFIDENCE

    return METRIC_TARGET_CONFIDENCE


def _infer_finding_target_metric(finding: ReviewFinding) -> str:
    """Finding が影響する指標（confidence/feasibility/both）を推定."""
    if finding.category == FindingCategory.TIMELINE_UNREALISTIC:
        return METRIC_TARGET_FEASIBILITY

    if finding.category in {
        FindingCategory.OVER_OPTIMISM,
        FindingCategory.RESPONSIBILITY_GAP,
    }:
        return METRIC_TARGET_BOTH

    keywords = ("feasib", "可行", "実装", "timeline", "期間", "risk", "リスク", "依存", "cost", "コスト")
    for improvement in finding.score_improvements:
        target_score = improvement.target_score.lower()
        if any(keyword in target_score for keyword in keywords):
            return METRIC_TARGET_BOTH

    return METRIC_TARGET_CONFIDENCE


def _derive_path_feasibility_pct(
    path: Mapping[str, Any],
    judgment_framework: Mapping[str, Any],
) -> float:
    """単一パスの戦略可行度（%）を算出."""
    legacy_probability = _safe_float(path.get("success_probability"), -1.0)
    if legacy_probability > 0:
        return max(0.0, min(100.0, legacy_probability * 100.0))

    conditional_eval = _to_mapping(path.get("conditional_evaluation", {}))
    success_conditions = _normalize_str_list(conditional_eval.get("success_conditions", []))
    risk_factors = _normalize_str_list(conditional_eval.get("risk_factors", []))
    failure_modes = _normalize_str_list(conditional_eval.get("failure_modes", []))
    has_probability_basis = bool(str(conditional_eval.get("probability_basis", "")).strip())

    success_ratio = min(1.0, len(success_conditions) / 3.0)
    risk_ratio = min(1.0, len(risk_factors) / 3.0)
    failure_ratio = min(1.0, len(failure_modes) / 3.0)
    downside_ratio = (risk_ratio + failure_ratio) / 2.0

    reversibility = str(path.get("reversibility", "MEDIUM")).upper()
    reversibility_ratio = {
        "HIGH": 0.85,
        "MEDIUM": 0.65,
        "LOW": 0.45,
    }.get(reversibility, 0.6)

    path_id = str(path.get("path_id", "")).strip()
    gate_results = judgment_framework.get("gate_results", {})
    gate_pass_ratio = 0.6
    if isinstance(gate_results, Mapping) and path_id:
        path_gates = gate_results.get(path_id, [])
        if isinstance(path_gates, list) and path_gates:
            pass_count = sum(1 for gate in path_gates if bool(gate))
            gate_pass_ratio = pass_count / len(path_gates)

    should_scores = judgment_framework.get("should_scores", {})
    should_ratio = 0.6
    if isinstance(should_scores, Mapping) and path_id:
        path_scores = should_scores.get(path_id, [])
        if isinstance(path_scores, list):
            normalized_scores = [max(1.0, min(5.0, _safe_float(score, 3.0))) for score in path_scores]
            if normalized_scores:
                should_ratio = (sum(normalized_scores) / len(normalized_scores)) / 5.0

    basis_bonus = 0.05 if has_probability_basis else 0.0
    score_ratio = (
        0.30 * success_ratio
        + 0.20 * gate_pass_ratio
        + 0.15 * should_ratio
        + 0.20 * reversibility_ratio
        + 0.15 * max(0.0, 1.0 - downside_ratio)
        + basis_bonus
    )
    return max(0.0, min(100.0, score_ratio * 100.0))


def _calculate_base_feasibility_pct(report: Any) -> int:
    """レポートから戦略可行度（%）の基礎点を算出."""
    report_map = _to_mapping(report)
    fa = _to_mapping(report_map.get("fa", {}))
    recommended_paths = fa.get("recommended_paths", [])
    if not isinstance(recommended_paths, list) or not recommended_paths:
        return 0

    judgment_framework = _to_mapping(fa.get("judgment_framework", {}))
    path_scores = [
        _derive_path_feasibility_pct(_to_mapping(path), judgment_framework)
        for path in recommended_paths
    ]
    if not path_scores:
        return 0
    return _clamp_pct(max(path_scores))


def _has_any_keyword(text: str, keywords: tuple[str, ...]) -> bool:
    lowered = text.lower()
    return any(keyword.lower() in lowered for keyword in keywords)


def _evaluate_finding_bonus_fallback(
    checked_items: list[dict[str, Any]],
) -> tuple[float, list[str]]:
    """LLM利用不可時のルール加点."""
    if not checked_items:
        return 0.0, []

    reasons: list[str] = []
    total_bonus = 0.0

    owner_keywords = ("責任", "担当", "owner", "raci", "负责人", "責任者")
    deadline_keywords = ("期限", "deadline", "期日", "截至", "by ", "までに")
    verification_keywords = ("検証", "指標", "kpi", "metric", "evidence", "验收", "確認")
    risk_keywords = ("risk", "リスク", "风险", "mitigation", "回避", "監視", "フォロー")

    for item in checked_items:
        finding_index = int(item.get("finding_index", -1))
        note = str(item.get("note", "")).strip()
        if not note:
            reasons.append(f"所見#{finding_index + 1}: 補足メモが空のためLLM加点は0点")
            continue

        criteria_count = 0
        criteria_count += 1 if _has_any_keyword(note, owner_keywords) else 0
        criteria_count += 1 if _has_any_keyword(note, deadline_keywords) else 0
        criteria_count += 1 if _has_any_keyword(note, verification_keywords) else 0
        criteria_count += 1 if _has_any_keyword(note, risk_keywords) else 0

        per_bonus = min(4.0, criteria_count * 1.0)
        total_bonus += per_bonus
        reasons.append(
            f"所見#{finding_index + 1}: 責任者/期限/検証基準/リスク閉ループの {criteria_count}/4 を満たし +{round(per_bonus)}点"
        )

    return _clamp_bonus_pct(total_bonus), reasons[:5]


async def _evaluate_finding_bonus_with_ai(
    checked_items: list[dict[str, Any]],
) -> tuple[float, list[str]]:
    """補足メモに対するLLM加点を算出."""
    if not checked_items:
        return 0.0, []

    try:
        llm_client = get_llm()
    except Exception as exc:
        logger.warning(f"LLM 初期化失敗のため補足加点をルール評価へフォールバック: {exc}")
        llm_client = None

    if llm_client is None:
        return _evaluate_finding_bonus_fallback(checked_items)

    prompt = f"""あなたは意思決定提案の品質レビュー担当です。
以下の「確認済み所見メモ」を評価し、補足加点を計算してください。

評価ルーブリック（各所見 0-4点）:
1. 責任者・担当ロールが具体化されているか
2. 期限・マイルストーンが明確か
3. 検証可能な完了条件（KPI/証跡/判定基準）があるか
4. リスク閉ループ（再発防止/監視/フォロー）があるか

出力JSONのみ:
{{
  "bonus_pct": 0-15の数値,
  "reasons": ["短い理由1", "短い理由2"],
  "item_scores": [
    {{
      "finding_index": 0,
      "score": 0-4,
      "bonus_pct": 0-5,
      "reason": "所見ごとの根拠"
    }}
  ]
}}

確認済み所見メモ:
{json.dumps(checked_items, ensure_ascii=False)}
"""

    reviewer = create_agent(
        AgentFactorySpec(
            class_name="ReviewAgent",
            module_path="apps.decision_governance_engine.agents.review_agent",
            init_kwargs={"llm_client": llm_client},
            agent_type="reviewer",
        )
    )
    try:
        response = await reviewer._call_llm(prompt)
        parsed = extract_json(response)
        if not isinstance(parsed, dict):
            return _evaluate_finding_bonus_fallback(checked_items)

        raw_bonus = parsed.get("bonus_pct", 0.0)
        try:
            bonus_pct = _clamp_bonus_pct(float(raw_bonus))
        except (TypeError, ValueError):
            bonus_pct = 0.0

        reasons_raw = parsed.get("reasons", [])
        reasons = [str(item) for item in reasons_raw if isinstance(item, str) and item.strip()]
        if not reasons:
            reasons = ["補足メモの具体性に応じて加点を適用しました。"]

        return bonus_pct, reasons[:5]
    except json.JSONDecodeError:
        return _evaluate_finding_bonus_fallback(checked_items)
    except Exception as exc:
        logger.warning(f"LLM 補足加点評価に失敗したためフォールバック: {exc}")
        return _evaluate_finding_bonus_fallback(checked_items)


@router.post("/apply-checkpoints", response_model=CheckpointApplyResponse)
async def apply_checkpoints(
    request: CheckpointApplyRequest,
) -> CheckpointApplyResponse:
    """チェックポイントを反映して信頼度・判定を再計算し、結果を永続化."""
    if not request.items and not request.finding_confirmations:
        raise HTTPException(status_code=400, detail="反映対象のチェック項目がありません")

    report = await _get_report_from_db(request.report_id)
    if report is None:
        raise HTTPException(status_code=404, detail=f"レポート {request.report_id} が見つかりません")

    try:
        review = _resolve_report_review(report)
    except ValueError as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc

    item_map = {item.item_id: item for item in request.items}
    updated_checkpoint_items: list[CheckpointItem] = []
    for item in review.checkpoint_items:
        incoming = item_map.get(item.item_id)
        updated_checkpoint_items.append(
            item.model_copy(
                update={
                    "checked": incoming.checked if incoming is not None else item.checked,
                    "annotation": (incoming.annotation or "").strip() if incoming is not None else item.annotation,
                }
            )
        )

    policy = load_human_review_policy()
    threshold_pct = int(policy.signable_confidence_threshold_pct)

    base_confidence_pct = _clamp_pct(review.confidence_score * 100)
    base_feasibility_pct = _calculate_base_feasibility_pct(report)

    checkpoint_confidence_boost = 0.0
    checkpoint_feasibility_boost = 0.0
    applied_contributions: list[dict[str, Any]] = []
    for item in updated_checkpoint_items:
        if not item.checked:
            continue
        target_metric = _infer_checkpoint_target_metric(item)
        confidence_delta = float(item.score_boost)
        feasibility_delta = float(item.score_boost) if target_metric in {
            METRIC_TARGET_FEASIBILITY,
            METRIC_TARGET_BOTH,
        } else 0.0
        checkpoint_confidence_boost += confidence_delta
        checkpoint_feasibility_boost += feasibility_delta
        applied_contributions.append(
            {
                "source": "checkpoint",
                "item_key": item.item_id,
                "label": item.label,
                "target_metric": target_metric,
                "confidence_boost_pct": _clamp_pct(confidence_delta),
                "feasibility_boost_pct": _clamp_pct(feasibility_delta),
                "note": (item.annotation or "").strip() or None,
            }
        )

    finding_map = {int(item.finding_index): item for item in request.finding_confirmations}
    checked_finding_payloads: list[dict[str, Any]] = []
    finding_confidence_boost = 0.0
    finding_feasibility_boost = 0.0
    feasibility_note_count = 0
    for finding_index, finding in enumerate(review.findings):
        incoming_finding = finding_map.get(finding_index)
        checked = bool(incoming_finding.checked) if incoming_finding is not None else False
        note_text = (incoming_finding.note or "").strip() if incoming_finding is not None else ""
        if not checked:
            continue

        target_metric = _infer_finding_target_metric(finding)
        confidence_delta = _extract_finding_boost_delta(finding)
        feasibility_delta = confidence_delta if target_metric in {
            METRIC_TARGET_FEASIBILITY,
            METRIC_TARGET_BOTH,
        } else 0.0
        finding_confidence_boost += confidence_delta
        finding_feasibility_boost += feasibility_delta
        if note_text and target_metric in {METRIC_TARGET_FEASIBILITY, METRIC_TARGET_BOTH}:
            feasibility_note_count += 1
        applied_contributions.append(
            {
                "source": "finding",
                "item_key": f"finding_{finding_index}",
                "label": finding.description,
                "target_metric": target_metric,
                "confidence_boost_pct": _clamp_pct(confidence_delta),
                "feasibility_boost_pct": _clamp_pct(feasibility_delta),
                "note": note_text or None,
            }
        )
        if note_text:
            checked_finding_payloads.append(
                {
                    "finding_index": finding_index,
                    "severity": finding.severity.value,
                    "category": finding.category.value,
                    "description": finding.description,
                    "suggested_revision": finding.suggested_revision,
                    "note": note_text,
                    "target_metric": target_metric,
                }
            )

    llm_bonus, bonus_reasons = await _evaluate_finding_bonus_with_ai(checked_finding_payloads)
    llm_feasibility_bonus = (
        llm_bonus * (feasibility_note_count / len(checked_finding_payloads))
        if checked_finding_payloads
        else 0.0
    )
    total_confidence_score = (
        base_confidence_pct
        + checkpoint_confidence_boost
        + finding_confidence_boost
        + llm_bonus
    )
    total_feasibility_score = (
        base_feasibility_pct
        + checkpoint_feasibility_boost
        + finding_feasibility_boost
        + llm_feasibility_bonus
    )
    recalculated_confidence_pct = _clamp_pct(total_confidence_score)
    recalculated_feasibility_pct = _clamp_pct(total_feasibility_score)
    recalculated_confidence_score = round(recalculated_confidence_pct / 100.0, 2)
    signature_eligible = recalculated_confidence_pct >= threshold_pct

    if checked_finding_payloads:
        llm_target_metric = METRIC_TARGET_BOTH if feasibility_note_count > 0 else METRIC_TARGET_CONFIDENCE
        applied_contributions.append(
            {
                "source": "llm_bonus",
                "item_key": "llm_bonus",
                "label": "補足メモ評価による追加加点",
                "target_metric": llm_target_metric,
                "confidence_boost_pct": _clamp_pct(llm_bonus),
                "feasibility_boost_pct": _clamp_pct(llm_feasibility_bonus),
                "note": " / ".join(bonus_reasons) if bonus_reasons else None,
            }
        )

    if signature_eligible:
        next_verdict = ReviewVerdict.PASS
    elif review.overall_verdict == ReviewVerdict.PASS:
        next_verdict = ReviewVerdict.REVISE
    else:
        next_verdict = review.overall_verdict

    updated_review = ReviewOutput(
        overall_verdict=next_verdict,
        findings=review.findings,
        confidence_score=recalculated_confidence_score,
        final_warnings=review.final_warnings,
        confidence_breakdown=review.confidence_breakdown,
        checkpoint_items=updated_checkpoint_items,
        auto_recalc_enabled=review.auto_recalc_enabled,
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

    await _persist_human_review_record(
        request_id_text=request.request_id,
        report_id_text=request.report_id,
        review_record={
            "event_type": "apply_checkpoints",
            "report_id": request.report_id,
            "reviewer_name": request.reviewer_name,
            "base_confidence_pct": base_confidence_pct,
            "checkpoint_boost_pct": round(checkpoint_confidence_boost),
            "finding_boost_pct": round(finding_confidence_boost),
            "llm_bonus_pct": round(llm_bonus),
            "base_feasibility_pct": base_feasibility_pct,
            "checkpoint_feasibility_boost_pct": round(checkpoint_feasibility_boost),
            "finding_feasibility_boost_pct": round(finding_feasibility_boost),
            "llm_feasibility_bonus_pct": round(llm_feasibility_bonus),
            "bonus_reasons": bonus_reasons,
            "recalculated_confidence_pct": recalculated_confidence_pct,
            "recalculated_feasibility_pct": recalculated_feasibility_pct,
            "threshold_pct": threshold_pct,
            "signature_eligible": signature_eligible,
            "applied_contributions": applied_contributions,
            "applied_items": [item.model_dump() for item in updated_checkpoint_items],
            "finding_confirmations": [item.model_dump() for item in request.finding_confirmations],
            "reviewed_at": datetime.now(UTC).isoformat(),
        },
        updated_review=updated_review.model_dump(),
    )

    return CheckpointApplyResponse(
        success=True,
        message=(
            "チェック項目を反映して信頼度と戦略可行度を再計算しました。"
            if signature_eligible
            else "チェック項目を反映しましたが、まだ署名閾値に達していません。"
        ),
        base_confidence_pct=base_confidence_pct,
        checkpoint_boost_pct=round(checkpoint_confidence_boost),
        finding_boost_pct=round(finding_confidence_boost),
        llm_bonus_pct=round(llm_bonus),
        bonus_reasons=bonus_reasons,
        recalculated_confidence_pct=recalculated_confidence_pct,
        base_feasibility_pct=base_feasibility_pct,
        checkpoint_feasibility_boost_pct=round(checkpoint_feasibility_boost),
        finding_feasibility_boost_pct=round(finding_feasibility_boost),
        llm_feasibility_bonus_pct=round(llm_feasibility_bonus),
        recalculated_feasibility_pct=recalculated_feasibility_pct,
        threshold_pct=threshold_pct,
        signature_eligible=signature_eligible,
        applied_contributions=applied_contributions,
        updated_review=updated_review.model_dump(),
    )


__all__ = ["router"]
