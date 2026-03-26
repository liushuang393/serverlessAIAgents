"""ヘルスチェック & メトリクス API.

Phase 13: /health, /ready, /metrics エンドポイント。
"""

from typing import Any

from apps.market_trend_monitor.backend.services.metrics_service import metrics_service
from fastapi import APIRouter
from fastapi.responses import JSONResponse


router = APIRouter(tags=["メトリクス"])


def _as_dict(value: object) -> dict[str, Any]:
    """辞書戻り値を正規化."""
    return value if isinstance(value, dict) else {}


@router.get("/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック."""
    return _as_dict(metrics_service.get_health())


@router.get("/ready")
async def readiness_check() -> JSONResponse:
    """レディネスチェック."""
    if metrics_service.is_ready():
        return JSONResponse({"status": "ready"})
    return JSONResponse({"status": "not_ready"}, status_code=503)


@router.get("/metrics")
async def get_metrics() -> dict[str, Any]:
    """パイプラインメトリクスを取得."""
    return _as_dict(metrics_service.get_metrics())
