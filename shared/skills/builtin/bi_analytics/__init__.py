"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings

from kernel.skills.builtin.bi_analytics.analyzer import AnalysisResult, BIAnalyzer
from kernel.skills.builtin.bi_analytics.connector import (
    ConnectionConfig,
    ConnectorType,
    DataConnector,
    DataFrame,
    MemoryConnector,
)
from kernel.skills.builtin.bi_analytics.visualizer import Chart, ChartGenerator, ChartType


warnings.warn(
    "shared.skills.builtin.bi_analytics は非推奨です。kernel.skills.builtin.bi_analytics を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = [
    "AnalysisResult",
    "BIAnalyzer",
    "Chart",
    "ChartGenerator",
    "ChartType",
    "ConnectionConfig",
    "ConnectorType",
    "DataConnector",
    "DataFrame",
    "MemoryConnector",
]
