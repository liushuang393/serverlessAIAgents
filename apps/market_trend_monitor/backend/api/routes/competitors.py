"""競合追跡・成熟度・レポートAPI."""

import logging
from typing import Any

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field


router = APIRouter(prefix="/api", tags=["戦略機能"])
logger = logging.getLogger(__name__)

# サービスインスタンスは遅延初期化
_competitor_agent = None
_maturity_service = None
_stakeholder_service = None


def _get_competitor_agent():
    global _competitor_agent
    if _competitor_agent is None:
        from apps.market_trend_monitor.backend.agents.competitor_tracking_agent import (
            CompetitorTrackingAgent,
        )
        _competitor_agent = CompetitorTrackingAgent()
    return _competitor_agent


def _get_maturity_service():
    global _maturity_service
    if _maturity_service is None:
        from apps.market_trend_monitor.backend.services.maturity_assessment_service import (
            MaturityAssessmentService,
        )
        _maturity_service = MaturityAssessmentService()
    return _maturity_service


def _get_stakeholder_service():
    global _stakeholder_service
    if _stakeholder_service is None:
        from apps.market_trend_monitor.backend.services.stakeholder_report_service import (
            StakeholderReportService,
        )
        _stakeholder_service = StakeholderReportService()
    return _stakeholder_service


# ============================================================
# 競合追跡エンドポイント
# ============================================================


@router.get("/competitors")
async def list_competitors() -> dict:
    """競合プロファイル一覧を取得."""
    agent = _get_competitor_agent()
    profiles = agent.list_profiles()
    return {
        "competitors": [p.to_dict() for p in profiles],
        "total": len(profiles),
    }


@router.get("/competitors/{name}")
async def get_competitor(name: str) -> dict:
    """競合プロファイルを取得."""
    agent = _get_competitor_agent()
    profile = agent.get_profile(name)
    if not profile:
        raise HTTPException(status_code=404, detail="Competitor not found")
    return profile.to_dict()


class PositioningRequest(BaseModel):
    """ポジショニング比較リクエスト."""

    our_strengths: list[str] = Field(min_length=1)


@router.post("/competitors/positioning")
async def compare_positioning(request: PositioningRequest) -> dict:
    """市場ポジショニング比較."""
    agent = _get_competitor_agent()
    result = await agent.compare_positioning(request.our_strengths)
    return result


# ============================================================
# 成熟度エンドポイント
# ============================================================


@router.get("/maturity")
async def list_maturity() -> dict:
    """技術成熟度ランドスケープを取得."""
    service = _get_maturity_service()
    assessments = service.list_assessments()
    return {
        "assessments": [a.to_dict() for a in assessments],
        "total": len(assessments),
    }


@router.get("/maturity/{technology}")
async def get_maturity(technology: str) -> dict:
    """技術成熟度を取得."""
    service = _get_maturity_service()
    assessment = service.get_assessment(technology)
    if not assessment:
        raise HTTPException(status_code=404, detail="Technology assessment not found")
    return assessment.to_dict()


# ============================================================
# ステークホルダーレポートエンドポイント
# ============================================================


class ReportGenerateRequest(BaseModel):
    """レポート生成リクエスト."""

    report_type: str = Field(default="executive", pattern="^(executive|technical|weekly_digest)$")
    topic: str | None = None
    period_days: int = Field(default=7, ge=1, le=90)


@router.get("/stakeholder-reports")
async def list_stakeholder_reports(
    report_type: str | None = Query(default=None),
) -> dict:
    """ステークホルダーレポート一覧を取得."""
    service = _get_stakeholder_service()
    reports = service.list_reports(report_type=report_type)
    return {
        "reports": [r.to_dict() for r in reports],
        "total": len(reports),
    }


@router.get("/stakeholder-reports/{report_id}")
async def get_stakeholder_report(report_id: str) -> dict:
    """ステークホルダーレポートを取得."""
    service = _get_stakeholder_service()
    report = service.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")
    return report.to_dict()
