"""データ収集API."""

import asyncio
import logging
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    ReporterAgent,
)
from apps.market_trend_monitor.backend.api.state import store
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.models import DateRange
from apps.market_trend_monitor.backend.services.metrics_service import metrics_service
from apps.market_trend_monitor.backend.services.registry import (
    evidence_service,
    prediction_service,
)
from apps.market_trend_monitor.backend.workflow import run as run_workflow
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


router = APIRouter(prefix="/api", tags=["データ収集"])
logger = logging.getLogger(__name__)


class CollectRequest(BaseModel):
    """収集リクエスト."""

    keywords: list[str] = Field(default_factory=list, description="検索キーワード")
    sources: list[str] = Field(default_factory=list, description="データソース")
    date_range: DateRange | None = Field(default=None, description="収集対象期間")


class CollectResponse(BaseModel):
    """収集レスポンス."""

    status: str = Field(default="success")
    articles_count: int = 0
    trends_count: int = 0
    message: str = ""


def _extract_flow_results(result: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    """フロー結果から collector/analyzer を抽出."""
    collector = result.get("collector", {})
    analyzer = result.get("analyzer", {})

    if not isinstance(collector, dict):
        collector = {}
    if not isinstance(analyzer, dict):
        analyzer = {}

    return collector, analyzer


async def _run_fallback_collection(
    keywords: list[str],
    sources: list[str],
    date_range: dict[str, str] | None,
) -> dict[str, Any]:
    """フォールバック収集を実行（collector + analyzer + reporter）."""
    collector_agent = CollectorAgent()
    analyzer_agent = AnalyzerAgent(evidence_service=evidence_service)
    reporter_agent = ReporterAgent()

    await collector_agent.initialize()
    await analyzer_agent.initialize()
    await reporter_agent.initialize()

    try:
        collector_result = await collector_agent.run(
            {"keywords": keywords, "sources": sources, "date_range": date_range}
        )
        analyzer_result = await analyzer_agent.run(
            {
                "articles": collector_result.get("articles", []),
                "enable_sentiment": config.analyzer.enable_sentiment_analysis,
            }
        )
        reporter_result = await reporter_agent.run(
            {
                "trends": analyzer_result.get("trends", []),
                "summary": analyzer_result.get("summary", ""),
                "period": datetime.now().strftime("%Y-W%U"),
            }
        )
        return {
            "collector": collector_result,
            "analyzer": analyzer_result,
            "reporter": reporter_result,
        }
    finally:
        await collector_agent.cleanup()
        await analyzer_agent.cleanup()
        await reporter_agent.cleanup()


async def _ensure_reporter_result(result: dict[str, Any]) -> dict[str, Any]:
    """reporter 結果が欠落している場合に補完する."""
    reporter = result.get("reporter", {})
    analyzer = result.get("analyzer", {})

    if isinstance(reporter, dict) and isinstance(reporter.get("report"), dict):
        return result

    if not isinstance(analyzer, dict):
        return result

    reporter_agent = ReporterAgent()
    try:
        await reporter_agent.initialize()
        reporter_result = await reporter_agent.run(
            {
                "trends": analyzer.get("trends", []),
                "summary": analyzer.get("summary", ""),
                "period": datetime.now().strftime("%Y-W%U"),
            }
        )
    except Exception as exc:
        logger.warning("reporter補完に失敗: %s", exc)
        return result
    finally:
        await reporter_agent.cleanup()

    merged = dict(result)
    merged["reporter"] = reporter_result
    return merged


async def _auto_create_predictions_from_trends(trends: list[dict[str, Any]]) -> int:
    """分析結果トレンドから予測を自動生成."""
    if not trends:
        return 0
    result = prediction_service.bootstrap_from_trends(
        trends=trends,
        horizon_days=30,
        limit=8,
    )
    created_ids = list(result.get("created_ids", []))
    if created_ids:
        await prediction_service.persist_predictions_by_ids(created_ids)
    created_count = int(result.get("created_count", 0))
    if created_count > 0:
        metrics_service.record_predictions(created_count)
    return created_count


# ワークフロー全体のタイムアウト（秒）
_WORKFLOW_TIMEOUT_SECONDS = 180


@router.post("/collect", response_model=CollectResponse)
async def collect(request: CollectRequest) -> CollectResponse:
    """手動データ収集をトリガー."""
    try:
        keywords = request.keywords or config.collector.keywords
        sources = request.sources or config.collector.sources
        date_range_payload = (
            request.date_range.model_dump(exclude_none=True)
            if request.date_range is not None
            else None
        )

        # ワークフロー実行（タイムアウト保護付き）
        try:
            result: dict[str, Any] = await asyncio.wait_for(
                run_workflow(
                    {"keywords": keywords, "sources": sources, "date_range": date_range_payload}
                ),
                timeout=_WORKFLOW_TIMEOUT_SECONDS,
            )
        except asyncio.TimeoutError:
            logger.warning(
                "ワークフローが %d秒でタイムアウト、フォールバック収集を実行",
                _WORKFLOW_TIMEOUT_SECONDS,
            )
            result = {}

        collector, analyzer = _extract_flow_results(result)

        # ワークフロー失敗/タイムアウト時にフォールバック収集を実行
        if not collector and not analyzer:
            result = await _run_fallback_collection(
                keywords,
                sources,
                date_range_payload,
            )
            collector, analyzer = _extract_flow_results(result)

        result = await _ensure_reporter_result(result)

        await store.update_from_flow(result)

        articles_count = len(collector.get("articles", []))
        trends_count = len(analyzer.get("trends", []))
        metrics_service.record_articles(articles_count)
        created_predictions = await _auto_create_predictions_from_trends(
            list(analyzer.get("trends", []))
        )
        if created_predictions > 0:
            logger.info("予測を自動生成: %s件", created_predictions)

        if articles_count == 0 and trends_count == 0:
            return CollectResponse(
                status="empty",
                articles_count=0,
                trends_count=0,
                message="データ収集は完了しましたが、該当データが見つかりませんでした",
            )

        return CollectResponse(
            status="success",
            articles_count=articles_count,
            trends_count=trends_count,
            message="データ収集が完了しました",
        )
    except Exception as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc
