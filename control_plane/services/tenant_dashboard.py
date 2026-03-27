"""互換 facade.

Tenant dashboard の正規実装は `control_plane.dashboards.service`。
旧 platform app 再編時の互換 import 窓口として残している。
"""

from control_plane.dashboards.service import (
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
