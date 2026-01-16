# -*- coding: utf-8 -*-
"""FAQ System Agents.

強化版 FAQ システムのエージェント群。

- InternalKBAgent: 社内KB（RBAC制御 + 保守モード）
- ExternalKBAgent: 対客KB（公開情報専用）
- MaintenanceAgent: メンテナンス支援
- AnalyticsAgent: 高層データ分析
"""

from apps.faq_system.backend.agents.enhanced_faq_agent import (
    EnhancedFAQAgent,
    EnhancedFAQConfig,
    FAQResponse,
)
from apps.faq_system.backend.agents.internal_kb_agent import (
    InternalKBAgent,
    InternalKBConfig,
    InternalKBResponse,
    Citation,
)
from apps.faq_system.backend.agents.external_kb_agent import (
    ExternalKBAgent,
    ExternalKBConfig,
    ExternalKBResponse,
)
from apps.faq_system.backend.agents.maintenance_agent import (
    MaintenanceAgent,
    MaintenanceConfig,
    MaintenanceResponse,
    DiffResult,
    ImpactAnalysis,
)
from apps.faq_system.backend.agents.analytics_agent import (
    AnalyticsAgent,
    AnalyticsConfig,
    AnalyticsResponse,
    SQLGuardrails,
    EvidenceChain,
)

__all__ = [
    # 既存
    "EnhancedFAQAgent",
    "EnhancedFAQConfig",
    "FAQResponse",
    # 社内KB
    "InternalKBAgent",
    "InternalKBConfig",
    "InternalKBResponse",
    "Citation",
    # 対客KB
    "ExternalKBAgent",
    "ExternalKBConfig",
    "ExternalKBResponse",
    # メンテナンス
    "MaintenanceAgent",
    "MaintenanceConfig",
    "MaintenanceResponse",
    "DiffResult",
    "ImpactAnalysis",
    # 分析
    "AnalyticsAgent",
    "AnalyticsConfig",
    "AnalyticsResponse",
    "SQLGuardrails",
    "EvidenceChain",
]
