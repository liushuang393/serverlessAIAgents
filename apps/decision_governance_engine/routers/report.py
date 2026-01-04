# -*- coding: utf-8 -*-
"""レポート関連APIルーター.

エンドポイント:
    - GET /api/report/{report_id}/pdf: PDF出力
    - GET /api/report/{report_id}/components: A2UIコンポーネント
    - GET /api/report/{report_id}/agent/{agent_id}: 個別Agent出力
    - POST /api/report/{report_id}/sign: 署名
    - GET /api/report/{report_id}/signature: 署名情報取得
"""

import io
import logging
from datetime import datetime
from typing import Any

from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from apps.decision_governance_engine.routers.auth import UserInfo, require_auth

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


def _get_sample_report(report_id: str) -> Any:
    """デモ用サンプルレポートを取得."""
    from apps.decision_governance_engine.schemas.output_schemas import (
        DecisionReport,
        ExecutiveSummary,
    )
    return DecisionReport(
        report_id=report_id,
        dao={"problem_type": "TRADE_OFF", "essence": "サンプル本質"},
        fa={"recommended_paths": [{"name": "推奨案A", "description": "説明"}]},
        shu={"phases": [{"phase_number": 1, "name": "準備", "duration": "2週間"}], "first_action": "MTG設定"},
        qi={"implementations": [], "tool_recommendations": []},
        review={"overall_verdict": "PASS", "confidence_score": 0.85},
        executive_summary=ExecutiveSummary(
            one_line_decision="推奨案Aを選択",
            recommended_action="段階的に実行",
            key_risks=["リスク1"],
            first_step="キックオフMTG",
            estimated_impact="目標達成見込み",
        ),
    )


# ========================================
# エンドポイント
# ========================================

@router.get("/{report_id}/pdf")
async def export_report_pdf(report_id: str) -> StreamingResponse:
    """レポートをPDF形式でエクスポート."""
    from apps.decision_governance_engine.services.pdf_generator import PDFGeneratorService

    sample_report = _get_sample_report(report_id)
    generator = PDFGeneratorService()
    pdf_bytes = generator.generate_pdf(sample_report)

    content_type = "application/pdf" if generator._has_reportlab else "text/html"
    filename = f"decision_report_{report_id}.{'pdf' if generator._has_reportlab else 'html'}"

    return StreamingResponse(
        io.BytesIO(pdf_bytes),
        media_type=content_type,
        headers={"Content-Disposition": f"attachment; filename={filename}"},
    )


@router.get("/{report_id}/components")
async def get_report_components(report_id: str) -> dict[str, Any]:
    """レポートのA2UIコンポーネントを取得."""
    from apps.decision_governance_engine.services.ui_components import DecisionUIComponentBuilder

    sample_report = _get_sample_report(report_id)
    builder = DecisionUIComponentBuilder()
    components = builder.build_report_view(sample_report)

    return {
        "report_id": report_id,
        "components": [c.to_dict() for c in components],
    }


@router.get("/{report_id}/agent/{agent_id}")
async def get_agent_output_component(report_id: str, agent_id: str) -> dict[str, Any]:
    """特定AgentのA2UI出力コンポーネントを取得."""
    from apps.decision_governance_engine.services.ui_components import DecisionUIComponentBuilder

    sample_report = _get_sample_report(report_id)
    builder = DecisionUIComponentBuilder()

    component_map = {
        "dao": lambda: builder._build_dao_card(sample_report.dao),
        "fa": lambda: builder._build_fa_card(sample_report.fa),
        "shu": lambda: builder._build_shu_card(sample_report.shu),
        "qi": lambda: builder._build_qi_card(sample_report.qi),
        "review": lambda: builder._build_review_card(sample_report.review),
        "summary": lambda: builder._build_summary_card(sample_report),
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

