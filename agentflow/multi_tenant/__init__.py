"""マルチテナント対応モジュール.

エンタープライズ環境でのテナント分離を提供。

Features:
- テナントコンテキスト管理
- データ分離（shared/dedicated モード）
- リソース制限
- テナント別ストレージプレフィックス

Example:
    >>> from agentflow.multi_tenant import TenantContext, TenantManager
    >>>
    >>> # テナントコンテキストを作成
    >>> tenant = TenantContext(tenant_id="acme-corp")
    >>>
    >>> # Coordinator に注入
    >>> coordinator = DeepAgentCoordinator(tenant_context=tenant)
    >>>
    >>> # テナント情報を取得
    >>> print(tenant.storage_prefix)  # "tenant:acme-corp:"
"""

from agentflow.multi_tenant.context import ResourceLimits, TenantContext
from agentflow.multi_tenant.manager import TenantManager


__all__ = [
    "ResourceLimits",
    "TenantContext",
    "TenantManager",
]
