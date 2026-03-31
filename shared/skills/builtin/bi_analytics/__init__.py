"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_analyzer = import_module("kernel.skills.builtin.bi_analytics.analyzer")
_connector = import_module("kernel.skills.builtin.bi_analytics.connector")
_visualizer = import_module("kernel.skills.builtin.bi_analytics.visualizer")

AnalysisResult = _analyzer.AnalysisResult
BIAnalyzer = _analyzer.BIAnalyzer
ConnectionConfig = _connector.ConnectionConfig
ConnectorType = _connector.ConnectorType
DataConnector = _connector.DataConnector
DataFrame = _connector.DataFrame
MemoryConnector = _connector.MemoryConnector
Chart = _visualizer.Chart
ChartGenerator = _visualizer.ChartGenerator
ChartType = _visualizer.ChartType


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
