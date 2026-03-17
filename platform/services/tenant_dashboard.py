"""互換 facade.

Tenant dashboard の正規実装は top-level `platform.dashboards.service` に移設済み。
"""

from platform.dashboards.service import (
    RecentActivity,
    TenantDashboard,
    TenantStats,
    TopComponent,
    UsageTrend,
    get_tenant_dashboard,
)


__all__ = [
    "RecentActivity",
    "TenantDashboard",
    "TenantStats",
    "TopComponent",
    "UsageTrend",
    "get_tenant_dashboard",
]
