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
from typing import Any

from agentflow import Flow, create_flow
from agentflow.security import SafetyMixin, AISafetyGuard

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
class _WorkflowCompat(SafetyMixin):
    """後方互換用ラッパー（AI安全防護付き）.

    v1.1 新機能:
        - AI安全防護（幻覚検出、データ脱敏）
    """

    def __init__(self, f: Flow, enable_safety: bool = True) -> None:
        self._flow = f
        # AI安全防護初期化
        self.init_safety(enabled=enable_safety)

    async def run(self, input_data: dict[str, Any] | None = None) -> dict[str, Any]:
        return await run(input_data)

    async def run_with_safety(
        self,
        input_data: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """安全検査付きで実行.

        入力の安全検査を行い、結果に幻覚警告を付加。

        Args:
            input_data: 入力データ

        Returns:
            実行結果（安全警告付き）
        """
        if input_data is None:
            input_data = {}

        # キーワード入力の安全検査
        keywords = input_data.get("keywords", [])
        if keywords and self.safety_enabled:
            for keyword in keywords:
                check = await self.check_input_safety(keyword)
                if not check.is_safe:
                    logger.warning("入力キーワードに安全上の問題: %s", keyword)

        # 実行
        result = await run(input_data)

        # 出力の幻覚検査（レポート部分）
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


workflow = _WorkflowCompat(flow)

