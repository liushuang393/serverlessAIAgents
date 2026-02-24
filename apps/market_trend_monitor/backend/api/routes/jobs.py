"""ジョブ状態API.

バックグラウンド収集ジョブの状態をポーリングするためのエンドポイント。
フロントエンドは画面遷移後もこのAPIで進捗・完了を確認できる。
"""

import logging

from apps.market_trend_monitor.backend.db import async_session
from apps.market_trend_monitor.backend.db.models import CollectJobModel
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from sqlalchemy import desc, select


router = APIRouter(prefix="/api", tags=["ジョブ管理"])
logger = logging.getLogger(__name__)


class JobResponse(BaseModel):
    """ジョブ状態レスポンス."""

    job_id: str
    status: str
    progress: int
    current_step: str
    started_at: str
    completed_at: str | None = None
    articles_count: int | None = None
    trends_count: int | None = None
    error: str | None = None


def _to_response(job: CollectJobModel) -> JobResponse:
    return JobResponse(
        job_id=job.id,
        status=job.status,
        progress=job.progress,
        current_step=job.current_step,
        started_at=job.started_at.isoformat(),
        completed_at=job.completed_at.isoformat() if job.completed_at else None,
        articles_count=job.articles_count,
        trends_count=job.trends_count,
        error=job.error,
    )


@router.get(
    "/jobs/latest",
    responses={404: {"description": "ジョブが存在しない"}},
)
async def get_latest_job() -> JobResponse:
    """最新の収集ジョブ状態を取得."""
    async with async_session() as session:
        result = await session.execute(
            select(CollectJobModel)
            .order_by(desc(CollectJobModel.started_at))
            .limit(1)
        )
        job = result.scalar_one_or_none()

    if job is None:
        raise HTTPException(status_code=404, detail="ジョブが見つかりません")
    return _to_response(job)


@router.get(
    "/jobs/{job_id}",
    responses={404: {"description": "指定ジョブが存在しない"}},
)
async def get_job(job_id: str) -> JobResponse:
    """指定ジョブIDの状態を取得."""
    async with async_session() as session:
        result = await session.execute(
            select(CollectJobModel).where(CollectJobModel.id == job_id)
        )
        job = result.scalar_one_or_none()

    if job is None:
        raise HTTPException(status_code=404, detail=f"ジョブ {job_id} が見つかりません")
    return _to_response(job)
