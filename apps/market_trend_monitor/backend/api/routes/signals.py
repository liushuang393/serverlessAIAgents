"""信号評価API."""

from apps.market_trend_monitor.backend.api.state import signal_service
from apps.market_trend_monitor.backend.models import SignalGrade
from fastapi import APIRouter, HTTPException


router = APIRouter(tags=["信号評価"])


@router.get("/api/signals")
async def list_signals(min_grade: str | None = None) -> dict:
    """信号一覧を取得."""
    if min_grade:
        try:
            grade = SignalGrade(min_grade)
        except ValueError as exc:
            raise HTTPException(status_code=400, detail="Invalid min_grade") from exc
    else:
        grade = None
    signals = signal_service.list_signals(min_grade=grade)
    return {"signals": [s.to_dict() for s in signals], "total": len(signals)}


@router.get("/api/signals/{signal_id}")
async def get_signal(signal_id: str) -> dict:
    """信号詳細を取得."""
    signal = signal_service.get_signal(signal_id)
    if not signal:
        raise HTTPException(status_code=404, detail="Signal not found")
    return signal.to_dict()


@router.get("/api/signals/dashboard")
async def get_signal_dashboard() -> dict:
    """信号ダッシュボード統計を取得."""
    return signal_service.get_dashboard_stats()
