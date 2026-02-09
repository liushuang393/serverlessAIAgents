"""ワークフロー定義 (Engine Pattern - 推奨方式).

AgentFlow v0.2.0+ の推奨パターン: PipelineEngine を使用。

v1.1 新機能:
    - PipelineEngine による統一的なマルチステージ処理
    - AI安全防護統合（幻覚検出、データ脱敏）
    - ストリーミング対応

使用例:
    >>> from apps.market_trend_monitor.backend.workflow_engine import engine
    >>> result = await engine.run({"keywords": ["AI", "LLM"]})
    >>> # ストリーム実行
    >>> async for event in engine.run_stream(inputs):
    ...     print(event)
"""

import logging
from typing import Any

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    NotifierAgent,
    ReporterAgent,
)
from apps.market_trend_monitor.backend.config import config

from agentflow import PipelineEngine


logger = logging.getLogger(__name__)

# ============================================================
# Engine 作成（PipelineEngine パターン - 推奨）
# ============================================================

# PipelineEngine: マルチステージパイプライン処理
# 各ステージが明確に分離され、エラーハンドリングと観測性が向上
engine = PipelineEngine(
    name="market-trend-monitor",
    stages=[
        {
            "name": "collect",
            "agent": CollectorAgent,  # クラスを渡す（Engine が自動インスタンス化）
            "description": "データ収集ステージ: 複数ソースから市場動向を収集",
        },
        {
            "name": "analyze",
            "agent": AnalyzerAgent,
            "description": "分析ステージ: トレンド抽出とセンチメント分析",
        },
        {
            "name": "report",
            "agent": ReporterAgent,
            "description": "レポート生成ステージ: 読みやすいレポートを作成",
        },
        {
            "name": "notify",
            "agent": NotifierAgent,
            "description": "通知ステージ: 重要な変化を検知して通知",
        },
    ],
    # ストリーミング有効化（SSE/WebSocket対応）
    enable_streaming=True,
)


# ============================================================
# 便利関数（後方互換）
# ============================================================


async def run(input_data: dict[str, Any] | None = None) -> dict[str, Any]:
    """ワークフローを実行.

    Args:
        input_data: 入力データ（オプション）
            - keywords: 検索キーワード
            - sources: データソース

    Returns:
        実行結果（NotifierAgentの出力）
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    logger.info("Starting market trend workflow (Engine Pattern)")
    result = await engine.run(input_data)
    logger.info("Market trend workflow completed")
    return result


async def run_stream(input_data: dict[str, Any] | None = None):
    """ストリームモードで実行（SSE用）.

    Args:
        input_data: 入力データ（オプション）

    Yields:
        イベント: stage_start, stage_complete, result
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    logger.info("Starting market trend workflow stream (Engine Pattern)")
    async for event in engine.run_stream(input_data):
        yield event


async def cleanup() -> None:
    """クリーンアップ."""
    logger.info("Cleaning up market trend workflow engine")
    # PipelineEngine は自動的にリソース管理を行う


# ============================================================
# 後方互換用エイリアス
# ============================================================

# 古いコードとの互換性のため、engine を workflow としても公開
workflow = engine
