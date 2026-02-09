"""データ収集API."""

from typing import Any

from apps.market_trend_monitor.backend.api.state import store
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.workflow import run as run_workflow
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


router = APIRouter(tags=["データ収集"])


class CollectRequest(BaseModel):
    """収集リクエスト."""

    keywords: list[str] = Field(default_factory=list, description="検索キーワード")
    sources: list[str] = Field(default_factory=list, description="データソース")


class CollectResponse(BaseModel):
    """収集レスポンス."""

    status: str = Field(default="success")
    articles_count: int = 0
    trends_count: int = 0
    message: str = ""


@router.post("/api/collect", response_model=CollectResponse)
async def collect(request: CollectRequest) -> CollectResponse:
    """手動データ収集をトリガー."""
    try:
        keywords = request.keywords or config.collector.keywords
        sources = request.sources or config.collector.sources
        result: dict[str, Any] = await run_workflow(
            {"keywords": keywords, "sources": sources}
        )
        await store.update_from_flow(result)

        collector = result.get("collector", {})
        analyzer = result.get("analyzer", {})

        articles_count = len(collector.get("articles", []))
        trends_count = len(analyzer.get("trends", []))

        return CollectResponse(
            status="success",
            articles_count=articles_count,
            trends_count=trends_count,
            message="データ収集が完了しました",
        )
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc
