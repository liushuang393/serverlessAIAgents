"""Control-plane services."""

from __future__ import annotations

from typing import Any

from control_plane.services.agent_aggregator import AgentAggregatorService
from control_plane.services.app_discovery import AppDiscoveryService
from control_plane.services.app_lifecycle import (
    AppLifecycleManager,
    AppStatus,
    HealthCheckResult,
)
from control_plane.services.app_registry import AppRegistryService
from control_plane.services.app_scaffolder import AppScaffolderService
from control_plane.services.capability_registry import CapabilityRegistry
from control_plane.services.component_library import (
    ComponentEntry,
    ComponentLibrary,
    ComponentType,
    ComponentVisibility,
)
from control_plane.services.framework_audit import FrameworkAuditService
from control_plane.services.framework_env import FrameworkEnvService
from control_plane.services.gallery_service import GalleryService
from control_plane.services.mcp_registry import MCPRegistryService
from control_plane.services.port_allocator import PortAllocatorService
from control_plane.services.publish_orchestrator import PublishOrchestrator
from control_plane.services.rag_overview import RAGOverviewService
from control_plane.services.skill_catalog import SkillCatalogService
from control_plane.services.studio_service import StudioService

# NOTE: tenant_dashboard is lazily imported to avoid circular import:
#   dashboards.service → publish.runtime → services.__init__ → tenant_dashboard → dashboards.service
from control_plane.services.tenant_invitation import TenantInvitationService


# Lazy-loaded symbols to break circular imports
_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "TenantDashboard": ("control_plane.services.tenant_dashboard", "TenantDashboard"),
    "get_tenant_dashboard": ("control_plane.services.tenant_dashboard", "get_tenant_dashboard"),
}


def __getattr__(name: str) -> Any:
    """Lazily resolve symbols that would cause circular imports."""
    if name in _LAZY_IMPORTS:
        import importlib

        module_path, symbol_name = _LAZY_IMPORTS[name]
        module = importlib.import_module(module_path)
        return getattr(module, symbol_name)
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = [
    "AgentAggregatorService",
    "AppDiscoveryService",
    "AppLifecycleManager",
    "AppRegistryService",
    "AppScaffolderService",
    "AppStatus",
    "CapabilityRegistry",
    "ComponentEntry",
    "ComponentLibrary",
    "ComponentType",
    "ComponentVisibility",
    "FrameworkAuditService",
    "FrameworkEnvService",
    "GalleryService",
    "HealthCheckResult",
    "MCPRegistryService",
    "PortAllocatorService",
    "PublishOrchestrator",
    "RAGOverviewService",
    "SkillCatalogService",
    "StudioService",
    "TenantDashboard",
    "TenantInvitationService",
    "get_tenant_dashboard",
]
