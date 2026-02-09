"""予測復盤API."""

from datetime import date
from typing import Any

from apps.market_trend_monitor.backend.api.state import prediction_service
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


@router.get("/api/predictions")
async def list_predictions(reviewed: bool | None = None) -> dict:
    """予測一覧を取得."""
    predictions = prediction_service.list_predictions(reviewed=reviewed)
    return {"predictions": [p.to_dict() for p in predictions], "total": len(predictions)}


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
    return prediction.to_dict()


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
    return review.to_dict()


@router.get("/api/predictions/accuracy")
async def get_prediction_accuracy() -> dict:
    """精度統計を取得."""
    return prediction_service.get_accuracy_stats()
