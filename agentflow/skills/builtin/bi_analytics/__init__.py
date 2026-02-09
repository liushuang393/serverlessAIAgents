"""BI Analytics Skill.

データ分析、可視化、レポート生成を提供。

Example:
    >>> from agentflow.skills.builtin.bi_analytics import BIAnalyzer
    >>> analyzer = BIAnalyzer()
    >>> result = await analyzer.analyze(data, analysis_type="trend")
"""

from agentflow.skills.builtin.bi_analytics.analyzer import (
    AnalysisResult,
    AnalysisType,
    BIAnalyzer,
)
from agentflow.skills.builtin.bi_analytics.connector import (
    ConnectionConfig,
    ConnectorType,
    DataConnector,
)
from agentflow.skills.builtin.bi_analytics.visualizer import (
    Chart,
    ChartGenerator,
    ChartType,
)


__all__ = [
    "AnalysisResult",
    "AnalysisType",
    # Analyzer
    "BIAnalyzer",
    "Chart",
    # Visualizer
    "ChartGenerator",
    "ChartType",
    "ConnectionConfig",
    "ConnectorType",
    # Connector
    "DataConnector",
]
