"""レポート関連APIルーター.

エンドポイント:
    - GET /api/report/{report_id}/pdf: PDF出力
    - GET /api/report/{report_id}/html: HTML出力
    - GET /api/report/{report_id}/components: A2UIコンポーネント
    - GET /api/report/{report_id}/agent/{agent_id}: 個別Agent出力
    - POST /api/report/{report_id}/sign: 署名
    - GET /api/report/{report_id}/signature: 署名情報取得
"""

import io
import logging
from datetime import datetime
from typing import Any

from apps.decision_governance_engine.routers.auth import UserInfo, require_auth
from apps.decision_governance_engine.services.human_review_policy import (
    enrich_review_with_policy,
    load_human_review_policy,
)
from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field


logger = logging.getLogger("decision_api.report")

router = APIRouter(prefix="/api/report", tags=["レポート"])


# ========================================
# スキーマ定義
# ========================================


class SignatureRequest(BaseModel):
    """署名リクエスト."""

    report_id: str = Field(..., description="レポートID")
    confirmation: bool = Field(..., description="意思決定確認フラグ")


class SignatureResponse(BaseModel):
    """署名レスポンス."""

    success: bool
    message: str
    signature: dict[str, Any] | None = None


# 署名済みレポート保存（本番ではDB使用）
_signed_reports: dict[str, dict[str, Any]] = {}

# レポートキャッシュ（PROP-* 形式の report_id → DecisionReport）
# SSE完了時にキャッシュし、PDF出力時に参照する
_report_cache: dict[str, Any] = {}

# キャッシュ上限（メモリ保護）
_REPORT_CACHE_MAX_SIZE = 100


def cache_report(report_id: str, report: Any) -> None:
    """レポートをキャッシュに保存.

    Args:
        report_id: レポートID（PROP-* 形式）
        report: DecisionReport オブジェクトまたは dict
    """
    # キャッシュ上限を超えた場合、最も古いエントリを削除
    if len(_report_cache) >= _REPORT_CACHE_MAX_SIZE:
        oldest_key = next(iter(_report_cache))
        del _report_cache[oldest_key]
    _report_cache[report_id] = report
    logger.info(f"レポートをキャッシュに保存: {report_id} (cache_size={len(_report_cache)})")


def _extract_review_confidence_pct(report: Any) -> int:
    """レポートから分析信頼度（%）を取得."""
    if report is None:
        return 0

    review_data: Any = None
    if hasattr(report, "review"):
        review_data = report.review
    elif isinstance(report, dict):
        review_data = report.get("review")

    raw_confidence: Any = 0.0
    if isinstance(review_data, dict):
        raw_confidence = review_data.get("confidence_score", 0.0)
    elif hasattr(review_data, "confidence_score"):
        raw_confidence = getattr(review_data, "confidence_score", 0.0)

    try:
        confidence = float(raw_confidence)
    except (TypeError, ValueError):
        confidence = 0.0

    confidence = max(0.0, min(1.0, confidence))
    return round(confidence * 100)


# ========================================
# エンドポイント
# ========================================


async def _get_report_from_db(report_id: str) -> Any:
    """キャッシュまたはDBから決策レポートを取得.

    検索順序:
        1. メモリキャッシュ（PROP-* 形式対応）
        2. DB検索（UUID 形式の場合のみ）

    Args:
        report_id: レポートID（PROP-* 形式または UUID 文字列）

    Returns:
        DecisionReport | None: レポートオブジェクト、または取得失敗時はNone
    """
    from uuid import UUID

    from apps.decision_governance_engine.repositories import DecisionRepository
    from apps.decision_governance_engine.schemas.output_schemas import (
        DecisionReport,
    )

    # キャッシュから取得を試みる（PROP-* 形式対応）
    cached = _report_cache.get(report_id)
    if cached is not None:
        logger.info(f"レポートをキャッシュから取得: {report_id}")
        return cached

    # UUID 形式の場合は request_id で検索
    try:
        request_uuid = UUID(report_id)
    except (ValueError, AttributeError, TypeError):
        request_uuid = None

    try:
        repo = DecisionRepository()
        if request_uuid is not None:
            record = await repo.find_by_request_id(request_uuid)
        else:
            record = await repo.find_by_report_case_id(report_id)
        if not record:
            logger.info(f"No record found for report_id/request_id: {report_id}")
            return None

        # DB記録からレポートを構築
        dao_data = record.dao_result or {"problem_type": "UNKNOWN", "essence": "N/A"}
        fa_data = record.fa_result or {
            "recommended_paths": [],
            "rejected_paths": [],
            "decision_criteria": [],
        }
        shu_data = record.shu_result or {"phases": [], "first_action": "N/A", "dependencies": []}
        qi_data = record.qi_result or {
            "implementations": [],
            "tool_recommendations": [],
            "integration_points": [],
            "technical_debt_warnings": [],
        }
        review_data = record.review_result or {
            "overall_verdict": "PASS",
            "confidence_score": record.confidence or 0.0,
            "findings": [],
            "final_warnings": [],
        }
        review_data = enrich_review_with_policy(review_data)

        # 提案書タイトルと署名欄を復元（当日のハッシュでIDが安定する）
        created_at = record.created_at or datetime.utcnow()
        problem_type = dao_data.get("problem_type", "")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value

        from apps.decision_governance_engine.schemas.output_schemas import (
            generate_proposal_title,
            generate_signature_block,
        )
        from apps.decision_governance_engine.services.report_generator import ReportGenerator

        proposal_title = generate_proposal_title(record.question, str(problem_type), now=created_at)
        if record.report_case_id:
            proposal_title = proposal_title.model_copy(update={"case_id": record.report_case_id})
        signature_block = generate_signature_block(None, now=created_at)
        report_case_id = record.report_case_id or proposal_title.case_id

        # ExecutiveSummaryを構築（ReportGeneratorと同一ロジックを使用）
        generator = ReportGenerator()
        summary = generator._generate_executive_summary(dao_data, fa_data, shu_data)

        return DecisionReport(
            report_id=report_case_id,
            created_at=created_at,
            proposal_title=proposal_title,
            dao=dao_data,
            fa=fa_data,
            shu=shu_data,
            qi=qi_data,
            review=review_data,
            executive_summary=summary,
            signature_block=signature_block,
            original_question=record.question,
        )
    except Exception as e:
        logger.error(
            f"Failed to get report from DB for report_id={report_id}: {type(e).__name__}: {e}",
            exc_info=True,
        )
        return None


@router.get("/{report_id}/pdf")
async def export_report_pdf(report_id: str) -> StreamingResponse:
    """レポートをPDF形式でエクスポート.

    Args:
        report_id: レポートID（UUID文字列）

    Returns:
        StreamingResponse: PDFファイル

    Raises:
        HTTPException: レポート取得またはPDF生成に失敗した場合

    注意:
        - システム理念「変数・返回値強化」に基づき、全エラーを適切に処理
    """
    from apps.decision_governance_engine.services.pdf_generator import PDFGeneratorService
    from fastapi import HTTPException

    report = await _get_report_from_db(report_id)
    if not report:
        logger.error(f"Report not found: {report_id}")
        raise HTTPException(
            status_code=404,
            detail=f"Report not found: {report_id}",
        )

    try:
        generator = PDFGeneratorService()
        pdf_bytes = generator.generate_pdf(report)

        content_type = "application/pdf"
        filename = f"decision_report_{report_id}.pdf"

        return StreamingResponse(
            io.BytesIO(pdf_bytes),
            media_type=content_type,
            headers={"Content-Disposition": f"attachment; filename={filename}"},
        )
    except Exception as e:
        logger.error(
            f"PDF export failed for report_id={report_id}: {type(e).__name__}: {e}",
            exc_info=True,
        )
        raise HTTPException(
            status_code=500,
            detail=f"PDF生成に失敗しました: {e}",
        ) from e


@router.get("/{report_id}/html")
async def export_report_html(report_id: str) -> StreamingResponse:
    """レポートをHTML形式でエクスポート."""
    from apps.decision_governance_engine.services.pdf_generator import PDFGeneratorService

    report = await _get_report_from_db(report_id)
    if not report:
        logger.error(f"Report not found: {report_id}")
        raise HTTPException(
            status_code=404,
            detail=f"Report not found: {report_id}",
        )

    try:
        generator = PDFGeneratorService()
        html_bytes = generator.generate_html(report)
        filename = f"decision_report_{report_id}.html"
        return StreamingResponse(
            io.BytesIO(html_bytes),
            media_type="text/html; charset=utf-8",
            headers={"Content-Disposition": f"attachment; filename={filename}"},
        )
    except Exception as e:
        logger.error(
            f"HTML export failed for report_id={report_id}: {type(e).__name__}: {e}",
            exc_info=True,
        )
        raise HTTPException(
            status_code=500,
            detail=f"HTML生成に失敗しました: {e}",
        ) from e


@router.get("/{report_id}/components")
async def get_report_components(report_id: str) -> dict[str, Any]:
    """レポートのA2UIコンポーネントを取得."""
    from apps.decision_governance_engine.services.ui_components import DecisionUIComponentBuilder

    report = await _get_report_from_db(report_id)
    if not report:
        raise HTTPException(status_code=404, detail=f"Report not found: {report_id}")

    builder = DecisionUIComponentBuilder()
    components = builder.build_report_view(report)

    return {
        "report_id": report_id,
        "components": [c.to_dict() for c in components],
    }


@router.get("/{report_id}/agent/{agent_id}")
async def get_agent_output_component(report_id: str, agent_id: str) -> dict[str, Any]:
    """特定AgentのA2UI出力コンポーネントを取得."""
    from apps.decision_governance_engine.services.ui_components import DecisionUIComponentBuilder

    report = await _get_report_from_db(report_id)
    if not report:
        raise HTTPException(status_code=404, detail=f"Report not found: {report_id}")

    builder = DecisionUIComponentBuilder()

    component_map = {
        "dao": lambda: builder._build_dao_card(report.dao),
        "fa": lambda: builder._build_fa_card(report.fa),
        "shu": lambda: builder._build_shu_card(report.shu),
        "qi": lambda: builder._build_qi_card(report.qi),
        "review": lambda: builder._build_review_card(report.review),
        "summary": lambda: builder._build_summary_card(report),
    }

    if agent_id not in component_map:
        raise HTTPException(status_code=404, detail=f"Unknown agent: {agent_id}")

    component = component_map[agent_id]()
    return {
        "report_id": report_id,
        "agent_id": agent_id,
        "component": component.to_dict(),
    }


@router.post("/{report_id}/sign", response_model=SignatureResponse)
async def sign_report(
    report_id: str,
    req: SignatureRequest,
    user: UserInfo = Depends(require_auth),
) -> SignatureResponse:
    """レポートに電子署名を行う."""
    if not req.confirmation:
        return SignatureResponse(success=False, message="署名確認が必要です")

    if report_id in _signed_reports:
        existing = _signed_reports[report_id]
        return SignatureResponse(
            success=False,
            message=f"このレポートは既に {existing['signed_by']} により署名済みです",
        )

    report = await _get_report_from_db(report_id)
    if report is None:
        return SignatureResponse(
            success=False,
            message="レポートが見つからないため署名できません。",
        )

    threshold_pct = load_human_review_policy().signable_confidence_threshold_pct
    confidence_pct = _extract_review_confidence_pct(report)
    if confidence_pct < threshold_pct:
        return SignatureResponse(
            success=False,
            message=(
                f"分析信頼度が{threshold_pct}%未満のため署名できません。"
                f"現在: {confidence_pct}%。検証タブで補強項目を反映してください。"
            ),
        )

    signed_at = datetime.now()
    signature_data = {
        "report_id": report_id,
        "signed_by": user.display_name,
        "signer_id": user.user_id,
        "department": user.department,
        "position": user.position,
        "signed_at": signed_at.isoformat(),
        "signed_at_display": signed_at.strftime("%Y年%m月%d日 %H:%M"),
    }

    _signed_reports[report_id] = signature_data
    logger.info(f"Report signed: {report_id} by {user.display_name}")

    return SignatureResponse(success=True, message="署名が完了しました", signature=signature_data)


@router.get("/{report_id}/signature")
async def get_report_signature(report_id: str) -> dict[str, Any]:
    """レポートの署名情報を取得."""
    if report_id not in _signed_reports:
        return {"signed": False, "signature": None}
    return {"signed": True, "signature": _signed_reports[report_id]}
