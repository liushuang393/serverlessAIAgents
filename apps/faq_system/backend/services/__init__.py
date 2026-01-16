# -*- coding: utf-8 -*-
"""FAQ System Services.

強化版 FAQ システムのサービス群。

- GlossaryService: 術語辞書管理
- CitationService: 引用ソース管理
- FeedbackService: フィードバック収集・分析
- CoverageDashboard: カバレッジ可視化
"""

# フレームワーク層のサービスを再エクスポート
from agentflow.agents import FAQAgent as FAQService
from agentflow.agents import FAQAgentConfig as FAQConfig

# FAQ System 固有サービス
from apps.faq_system.backend.services.glossary_service import (
    GlossaryService,
    GlossaryConfig,
    Term,
    TermEntry,
    TermType,
)
from apps.faq_system.backend.services.citation_service import (
    CitationService,
    CitationServiceConfig,
    Citation,
    SourceInfo,
    SourceType,
    CitationStyle,
)
from apps.faq_system.backend.services.feedback_service import (
    FeedbackService,
    FeedbackServiceConfig,
    Feedback,
    FeedbackType,
    FeedbackStatus,
    FeedbackStats,
    ImprovementSuggestion,
)
from apps.faq_system.backend.services.coverage_dashboard import (
    CoverageDashboard,
    CoverageDashboardConfig,
    CoverageReport,
    CoverageStats,
    TopicCoverage,
    GapAnalysis,
    CoverageLevel,
    QueryLog,
)

__all__ = [
    # フレームワーク層
    "FAQService",
    "FAQConfig",
    # 術語辞書
    "GlossaryService",
    "GlossaryConfig",
    "Term",
    "TermEntry",
    "TermType",
    # 引用
    "CitationService",
    "CitationServiceConfig",
    "Citation",
    "SourceInfo",
    "SourceType",
    "CitationStyle",
    # フィードバック
    "FeedbackService",
    "FeedbackServiceConfig",
    "Feedback",
    "FeedbackType",
    "FeedbackStatus",
    "FeedbackStats",
    "ImprovementSuggestion",
    # カバレッジ
    "CoverageDashboard",
    "CoverageDashboardConfig",
    "CoverageReport",
    "CoverageStats",
    "TopicCoverage",
    "GapAnalysis",
    "CoverageLevel",
    "QueryLog",
]
