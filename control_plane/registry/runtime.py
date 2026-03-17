"""Control-plane registry 実装の互換窓口."""

from __future__ import annotations

from control_plane._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "AgentAggregatorService": ("control_plane.services.agent_aggregator", "AgentAggregatorService"),
    "MCPRegistryService": ("control_plane.services.mcp_registry", "MCPRegistryService"),
    "RAGOverviewService": ("control_plane.services.rag_overview", "RAGOverviewService"),
    "SkillCatalogService": ("control_plane.services.skill_catalog", "SkillCatalogService"),
    "StudioService": ("control_plane.services.studio_service", "StudioService"),
}


def __getattr__(name: str) -> Any:
    """registry 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
