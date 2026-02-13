"""FAQ System Services.

強化版 FAQ システムのサービス群。

- GlossaryService: 術語辞書管理
- CitationService: 引用ソース管理
- FeedbackService: フィードバック収集・分析
- CoverageDashboard: カバレッジ可視化
"""

# フレームワーク層のサービスを再エクスポート
from apps.faq_system.backend.services.citation_service import (
    Citation,
    CitationService,
    CitationServiceConfig,
    CitationStyle,
    SourceInfo,
    SourceType,
)
from apps.faq_system.backend.services.coverage_dashboard import (
    CoverageDashboard,
    CoverageDashboardConfig,
    CoverageLevel,
    CoverageReport,
    CoverageStats,
    GapAnalysis,
    QueryLog,
    TopicCoverage,
)
from apps.faq_system.backend.services.feedback_service import (
    Feedback,
    FeedbackService,
    FeedbackServiceConfig,
    FeedbackStats,
    FeedbackStatus,
    FeedbackType,
    ImprovementSuggestion,
)
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService

# FAQ System 固有サービス
from apps.faq_system.backend.services.glossary_service import (
    GlossaryConfig,
    GlossaryService,
    Term,
    TermEntry,
    TermType,
)

from agentflow.agents import FAQAgent as FAQService
from agentflow.agents import FAQAgentConfig as FAQConfig


__all__ = [
    "Citation",
    # 引用
    "CitationService",
    "CitationServiceConfig",
    "CitationStyle",
    # カバレッジ
    "CoverageDashboard",
    "CoverageDashboardConfig",
    "CoverageLevel",
    "CoverageReport",
    "CoverageStats",
    "ChatHistoryService",
    "FAQConfig",
    # フレームワーク層
    "FAQService",
    "Feedback",
    # フィードバック
    "FeedbackService",
    "FeedbackServiceConfig",
    "FeedbackStats",
    "FeedbackStatus",
    "FeedbackType",
    "GapAnalysis",
    "GlossaryConfig",
    # 術語辞書
    "GlossaryService",
    "ImprovementSuggestion",
    "QueryLog",
    "SourceInfo",
    "SourceType",
    "Term",
    "TermEntry",
    "TermType",
    "TopicCoverage",
]
