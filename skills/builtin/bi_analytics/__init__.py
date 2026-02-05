# -*- coding: utf-8 -*-
"""BI Analytics Skill.

データ分析、可視化、レポート生成を提供。

Example:
    >>> from agentflow.skills.builtin.bi_analytics import BIAnalyzer
    >>> analyzer = BIAnalyzer()
    >>> result = await analyzer.analyze(data, analysis_type="trend")
"""

from agentflow.skills.builtin.bi_analytics.analyzer import (
    BIAnalyzer,
    AnalysisType,
    AnalysisResult,
)
from agentflow.skills.builtin.bi_analytics.connector import (
    DataConnector,
    ConnectorType,
    ConnectionConfig,
)
from agentflow.skills.builtin.bi_analytics.visualizer import (
    ChartGenerator,
    ChartType,
    Chart,
)

__all__ = [
    # Analyzer
    "BIAnalyzer",
    "AnalysisType",
    "AnalysisResult",
    # Connector
    "DataConnector",
    "ConnectorType",
    "ConnectionConfig",
    # Visualizer
    "ChartGenerator",
    "ChartType",
    "Chart",
]
