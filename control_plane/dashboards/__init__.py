"""Control-plane dashboards 公開 API."""

from control_plane.dashboards.runtime import (
    RecentActivity,
    TenantDashboard,
    TenantStats,
    TopComponent,
    UsageTrend,
    get_tenant_dashboard,
)
from control_plane.dashboards.service import DashboardCard, DashboardService


__all__ = [
    "DashboardCard",
    "DashboardService",
    "RecentActivity",
    "TenantDashboard",
    "TenantStats",
    "TopComponent",
    "UsageTrend",
    "get_tenant_dashboard",
]
