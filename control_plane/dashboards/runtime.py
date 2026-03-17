"""Control-plane dashboard 実装の互換窓口."""

from __future__ import annotations

from typing import Any

from control_plane._legacy import resolve_export


_EXPORT_MAP = {
    "RecentActivity": ("control_plane.dashboards.service", "RecentActivity"),
    "TenantDashboard": ("control_plane.dashboards.service", "TenantDashboard"),
    "TenantStats": ("control_plane.dashboards.service", "TenantStats"),
    "TopComponent": ("control_plane.dashboards.service", "TopComponent"),
    "UsageTrend": ("control_plane.dashboards.service", "UsageTrend"),
    "get_tenant_dashboard": ("control_plane.dashboards.service", "get_tenant_dashboard"),
}


def __getattr__(name: str) -> Any:
    """dashboard 系サービスを遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
