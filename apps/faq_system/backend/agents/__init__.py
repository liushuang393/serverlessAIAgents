"""FAQ System Agents.

強化版 FAQ システムのエージェント群。

- InternalKBAgent: 社内KB（RBAC制御 + 保守モード）
- ExternalKBAgent: 対客KB（公開情報専用）
- MaintenanceAgent: メンテナンス支援
- AnalyticsAgent: 高層データ分析
"""

from apps.faq_system.backend.agents.analytics_agent import (
    AnalyticsAgent,
    AnalyticsConfig,
    AnalyticsResponse,
    EvidenceChain,
    NL2SQLEnhancementConfig,
    SQLGuardrails,
)
from apps.faq_system.backend.agents.enhanced_faq_agent import (
    EnhancedFAQAgent,
    EnhancedFAQConfig,
    FAQResponse,
)
from apps.faq_system.backend.agents.external_kb_agent import (
    ExternalKBAgent,
    ExternalKBConfig,
    ExternalKBResponse,
)
from apps.faq_system.backend.agents.internal_kb_agent import (
    Citation,
    InternalKBAgent,
    InternalKBConfig,
    InternalKBResponse,
)
from apps.faq_system.backend.agents.maintenance_agent import (
    DiffResult,
    ImpactAnalysis,
    MaintenanceAgent,
    MaintenanceConfig,
    MaintenanceResponse,
)


__all__ = [
    # 分析（NL2SQL 増強対応）
    "AnalyticsAgent",
    "AnalyticsConfig",
    "AnalyticsResponse",
    "Citation",
    "DiffResult",
    # 既存
    "EnhancedFAQAgent",
    "EnhancedFAQConfig",
    "EvidenceChain",
    # 対客KB
    "ExternalKBAgent",
    "ExternalKBConfig",
    "ExternalKBResponse",
    "FAQResponse",
    "ImpactAnalysis",
    # 社内KB
    "InternalKBAgent",
    "InternalKBConfig",
    "InternalKBResponse",
    # メンテナンス
    "MaintenanceAgent",
    "MaintenanceConfig",
    "MaintenanceResponse",
    "NL2SQLEnhancementConfig",
    "SQLGuardrails",
]
