"""Layer 5 registry 実装の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "AgentAggregatorService": ("platform.services.agent_aggregator", "AgentAggregatorService"),
    "MCPRegistryService": ("platform.services.mcp_registry", "MCPRegistryService"),
    "RAGOverviewService": ("platform.services.rag_overview", "RAGOverviewService"),
    "SkillCatalogService": ("platform.services.skill_catalog", "SkillCatalogService"),
    "StudioService": ("platform.services.studio_service", "StudioService"),
}


def __getattr__(name: str) -> Any:
    """registry 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
