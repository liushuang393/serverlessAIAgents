"""Layer 5 dashboard 実装の互換窓口."""

from __future__ import annotations

from typing import Any

from platform._legacy import resolve_export


_EXPORT_MAP = {
    "RecentActivity": ("platform.dashboards.service", "RecentActivity"),
    "TenantDashboard": ("platform.dashboards.service", "TenantDashboard"),
    "TenantStats": ("platform.dashboards.service", "TenantStats"),
    "TopComponent": ("platform.dashboards.service", "TopComponent"),
    "UsageTrend": ("platform.dashboards.service", "UsageTrend"),
    "get_tenant_dashboard": ("platform.dashboards.service", "get_tenant_dashboard"),
}


def __getattr__(name: str) -> Any:
    """dashboard 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
