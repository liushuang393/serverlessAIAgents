"""ワークフロー定義.

AgentFlow 統一入口で構築。

v1.1 新機能:
    - AI安全防護統合（幻覚検出、データ脱敏）

使用例:
    >>> from apps.market_trend_monitor.backend.workflow import flow
    >>> result = await flow.run({"keywords": ["AI", "LLM"]})
    >>> # ストリーム実行
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)
"""

import logging
from collections.abc import AsyncIterator
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    EvidenceLedgerAgent,
    NotifierAgent,
    RedTeamAgent,
    ReporterAgent,
    SignalScorerAgent,
)
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.services.metrics_service import metrics_service
from apps.market_trend_monitor.backend.services.registry import (
    adaptive_scoring_service,
    bayesian_confidence_service,
    evidence_service,
    prediction_service,
    signal_service,
    source_reliability_tracker,
)

from agentflow import Flow, create_flow
from agentflow.security import SafetyMixin


logger = logging.getLogger(__name__)

# ============================================================
# Flow 作成（FlowBuilder パターン）
# ============================================================

def _safe_get_result(ctx: Any, node_id: str) -> dict[str, Any]:
    """Phase 13: エラー回復付きの結果取得."""
    try:
        result = ctx.get_result(node_id)
        return result if isinstance(result, dict) else {}
    except Exception as e:
        logger.warning("ノード '%s' の結果取得失敗: %s", node_id, e)
        return {}


def _map_evidence_input(ctx: Any) -> dict[str, Any]:
    """証拠台帳の入力を構築."""
    collector = _safe_get_result(ctx, "collector")
    return {"articles": collector.get("articles", [])}


def _map_analyzer_input(ctx: Any) -> dict[str, Any]:
    """分析の入力を構築."""
    collector = _safe_get_result(ctx, "collector")
    return {
        "articles": collector.get("articles", []),
        "enable_sentiment": config.analyzer.enable_sentiment_analysis,
    }


def _map_signal_input(ctx: Any) -> dict[str, Any]:
    """信号評価の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    collector = _safe_get_result(ctx, "collector")
    trends = analyzer.get("trends", [])
    articles = collector.get("articles", [])

    evidence_counts: dict[str, int] = {}
    source_types: dict[str, list[str]] = {}

    for trend in trends:
        trend_id = trend.get("id", "")
        topic = trend.get("topic", "")
        matched = [
            article for article in articles if topic in article.get("keywords", [])
        ]
        evidence_counts[trend_id] = (
            len(matched) if matched else int(trend.get("articles_count", 0))
        )
        sources = {article.get("source") for article in matched if article.get("source")}
        if not sources:
            sources = {article.get("source") for article in articles if article.get("source")}
        source_types[trend_id] = sorted(sources)

    return {
        "trends": trends,
        "evidence_counts": evidence_counts,
        "source_types": source_types,
    }


def _map_reporter_input(ctx: Any) -> dict[str, Any]:
    """レポート生成の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    return {
        "trends": analyzer.get("trends", []),
        "summary": analyzer.get("summary", ""),
        "period": datetime.now().strftime("%Y-W%U"),
    }


def _map_redteam_input(ctx: Any) -> dict[str, Any]:
    """Red Team の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    trends = analyzer.get("trends", [])
    now = datetime.now().isoformat()

    claims: list[dict[str, Any]] = []
    for trend in trends:
        trend_id = trend.get("id", "")
        claims.append(
            {
                "id": f"claim-{trend_id}",
                "statement": f"トレンド「{trend.get('topic', '')}」が重要である",
                "level": "hypothesis",
                "confidence": float(trend.get("score", 0.0)),
                "evidence_ids": [],
                "counter_evidence_ids": [],
                "created_at": now,
                "updated_at": now,
                "metadata": {"trend_id": trend_id},
            }
        )

    return {"claims": claims}


def _map_notifier_input(ctx: Any) -> dict[str, Any]:
    """通知の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    return {
        "trends": analyzer.get("trends", []),
        "alert_threshold": config.notifier.alert_growth_rate_threshold,
    }


flow: Flow = (
    create_flow("market-trend-monitor", name="Market Trend Monitor")
    .then(
        CollectorAgent(source_reliability_tracker=source_reliability_tracker),
        EvidenceLedgerAgent(evidence_service=evidence_service),
        AnalyzerAgent(evidence_service=evidence_service),
        SignalScorerAgent(signal_service=signal_service),
        ReporterAgent(),
        RedTeamAgent(),
        NotifierAgent(),
        ids=[
            "collector",
            "evidence_ledger",
            "analyzer",
            "signal_scorer",
            "reporter",
            "red_team",
            "notifier",
        ],
        input_mappers={
            "evidence_ledger": _map_evidence_input,
            "analyzer": _map_analyzer_input,
            "signal_scorer": _map_signal_input,
            "reporter": _map_reporter_input,
            "red_team": _map_redteam_input,
            "notifier": _map_notifier_input,
        },
    )
    .build()
)


# ============================================================
# 便利関数
# ============================================================


async def run(input_data: dict[str, Any] | None = None) -> dict[str, Any]:
    """ワークフローを実行.

    Args:
        input_data: 入力データ（オプション）

    Returns:
        実行結果（NotifierAgentの出力）
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    logger.info("市場動向ワークフローを開始")
    metrics_service.start_run()
    try:
        result = await flow.run(input_data)
        metrics_service.end_run(success=True)
        logger.info("市場動向ワークフローが完了")
        return result
    except Exception as exc:
        metrics_service.end_run(success=False, error=str(exc))
        raise


async def run_stream(
    input_data: dict[str, Any] | None = None,
) -> AsyncIterator[dict[str, Any]]:
    """ストリームモードで実行（SSE用）.

    Yields:
        イベント: node_start, node_complete, result
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    async for event in flow.run_stream(input_data):
        yield event


async def cleanup() -> None:
    """クリーンアップ."""
    await flow.cleanup()


class MarketTrendWorkflow(SafetyMixin):
    """Market Trend Monitor ワークフロー（安全防護付き）."""

    def __init__(self, f: Flow | None = None, enable_safety: bool = True) -> None:
        self._flow = f or flow
        self.init_safety(enabled=enable_safety)

    async def run(self, input_data: dict[str, Any] | None = None) -> dict[str, Any]:
        return await run(input_data)

    async def run_with_safety(
        self,
        input_data: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """安全検査付きで実行."""
        if input_data is None:
            input_data = {}

        keywords = input_data.get("keywords", [])
        if keywords and self.safety_enabled:
            for keyword in keywords:
                check = await self.check_input_safety(keyword)
                if not check.is_safe:
                    logger.warning("入力キーワードに安全上の問題: %s", keyword)

        result = await run(input_data)

        if self.safety_enabled and "report" in result:
            report_text = str(result.get("report", ""))
            output_check = await self.check_output_safety(report_text)
            if output_check.needs_review:
                result["_safety_warnings"] = output_check.warnings
                logger.warning("出力に幻覚の可能性: %s", output_check.warnings)

        return result

    async def initialize(self) -> None:
        pass  # auto_initialize=True なので不要

    async def cleanup(self) -> None:
        await cleanup()

    @property
    def flow(self) -> Flow:
        return self._flow


workflow = MarketTrendWorkflow(flow)
