"""予測復盤API."""

from datetime import date
from typing import Any

from apps.market_trend_monitor.backend.api.state import prediction_service, store
from apps.market_trend_monitor.backend.models import PredictionOutcome
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


router = APIRouter(tags=["予測復盤"])


class PredictionCreateRequest(BaseModel):
    """予測作成リクエスト."""

    statement: str
    confidence: float = Field(default=0.5, ge=0.0, le=1.0)
    target_date: str
    claim_id: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)


class PredictionReviewRequest(BaseModel):
    """予測復盤リクエスト."""

    actual_outcome: str
    outcome: str
    notes: str = ""


class PredictionBootstrapRequest(BaseModel):
    """トレンドから予測を自動生成するリクエスト."""

    horizon_days: int = Field(default=30, ge=1, le=180)
    limit: int = Field(default=8, ge=1, le=50)


@router.get("/api/predictions")
async def list_predictions(reviewed: bool | None = None) -> dict:
    """予測一覧を取得."""
    predictions = prediction_service.list_predictions(reviewed=reviewed)
    return {"predictions": [p.to_dict() for p in predictions], "total": len(predictions)}


@router.get("/api/predictions/accuracy")
async def get_prediction_accuracy() -> dict:
    """精度統計を取得."""
    return prediction_service.get_accuracy_stats()


@router.get("/api/predictions/calibration")
async def get_calibration_metrics() -> dict:
    """Phase 12: 予測キャリブレーション（Brier Score）を取得."""
    return prediction_service.get_calibration_metrics()


@router.get("/api/predictions/{prediction_id}")
async def get_prediction(prediction_id: str) -> dict:
    """予測詳細を取得."""
    prediction = prediction_service.get_prediction(prediction_id)
    if not prediction:
        raise HTTPException(status_code=404, detail="Prediction not found")
    review = prediction_service.get_review_for_prediction(prediction_id)
    return {
        "prediction": prediction.to_dict(),
        "review": review.to_dict() if review else None,
    }


@router.post("/api/predictions")
async def create_prediction(request: PredictionCreateRequest) -> dict:
    """予測を作成."""
    target = date.fromisoformat(request.target_date)
    prediction = prediction_service.create_prediction(
        claim_id=request.claim_id or "",
        statement=request.statement,
        confidence=request.confidence,
        target_date=target,
        metadata=request.metadata,
    )
    await prediction_service.persist_prediction(prediction)
    return prediction.to_dict()


@router.post("/api/predictions/bootstrap")
async def bootstrap_predictions(request: PredictionBootstrapRequest) -> dict:
    """最新トレンドから予測を一括生成."""
    trends = await store.list_trends(limit=max(request.limit * 3, request.limit))
    if not trends:
        return {
            "status": "empty",
            "message": "トレンドデータがありません。先にデータ収集を実行してください。",
            "created": [],
            "created_count": 0,
            "skipped_count": 0,
            "source_trends": 0,
        }

    result = prediction_service.bootstrap_from_trends(
        trends,
        horizon_days=request.horizon_days,
        limit=request.limit,
    )
    created_ids = list(result.get("created_ids", []))
    if created_ids:
        await prediction_service.persist_predictions_by_ids(created_ids)
    created_count = int(result.get("created_count", 0))
    status = "success" if created_count > 0 else "noop"
    message = (
        f"{created_count}件の予測を生成しました"
        if created_count > 0
        else "新規生成対象がありませんでした（既存予測と重複）"
    )
    return {
        "status": status,
        "message": message,
        "created": result.get("created", []),
        "created_count": created_count,
        "skipped_count": int(result.get("skipped_count", 0)),
        "source_trends": len(trends),
    }


@router.post("/api/predictions/{prediction_id}/review")
async def review_prediction(
    prediction_id: str,
    request: PredictionReviewRequest,
) -> dict:
    """予測を復盤."""
    try:
        outcome = PredictionOutcome(request.outcome)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail="Invalid outcome") from exc
    review = prediction_service.review_prediction(
        prediction_id=prediction_id,
        actual_outcome=request.actual_outcome,
        outcome=outcome,
        notes=request.notes,
    )
    if not review:
        raise HTTPException(status_code=404, detail="Prediction not found")
    prediction = prediction_service.get_prediction(prediction_id)
    if prediction:
        await prediction_service.persist_prediction(prediction)
    await prediction_service.persist_review(review)
    return review.to_dict()
