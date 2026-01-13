"""ワークフロー定義.

AgentFlow 統一入口で構築。

使用例:
    >>> from apps.market_trend_monitor.backend.workflow import flow
    >>> result = await flow.run({"keywords": ["AI", "LLM"]})
    >>> # ストリーム実行
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)
"""

import logging
from collections.abc import AsyncIterator
from typing import Any

from agentflow import Flow, create_flow

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    NotifierAgent,
    ReporterAgent,
)
from apps.market_trend_monitor.backend.config import config

logger = logging.getLogger(__name__)

# ============================================================
# Flow 作成（FlowBuilder パターン）
# ============================================================

flow: Flow = (
    create_flow("market-trend-monitor", name="Market Trend Monitor")
    .then(CollectorAgent(), AnalyzerAgent(), ReporterAgent(), NotifierAgent())
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

    logger.info("Starting market trend workflow")
    result = await flow.run(input_data)
    logger.info("Market trend workflow completed")
    return result


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


# 後方互換: workflow オブジェクト
class _WorkflowCompat:
    """後方互換用ラッパー."""

    def __init__(self, f: Flow) -> None:
        self._flow = f

    async def run(self, input_data: dict[str, Any] | None = None) -> dict[str, Any]:
        return await run(input_data)

    async def initialize(self) -> None:
        pass  # auto_initialize=True なので不要

    async def cleanup(self) -> None:
        await cleanup()

    @property
    def flow(self) -> Flow:
        return self._flow


workflow = _WorkflowCompat(flow)

