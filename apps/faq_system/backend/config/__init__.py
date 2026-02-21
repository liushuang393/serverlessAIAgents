"""FAQ システム設定モジュール."""

from apps.faq_system.backend.config.knowledge_base import (
    KnowledgeBaseRegistry,
    KnowledgeBaseSettings,
    KnowledgeBaseType,
    KnowledgeBaseUpdateRequest,
    kb_registry,
)


__all__ = [
    "KnowledgeBaseRegistry",
    "KnowledgeBaseSettings",
    "KnowledgeBaseType",
    "KnowledgeBaseUpdateRequest",
    "kb_registry",
]
