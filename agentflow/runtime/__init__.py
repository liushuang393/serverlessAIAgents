"""Runtime utilities for AgentFlow.

マルチテナント環境向けの実行時管理機能を提供します。

主要コンポーネント:
- RuntimeContext: 実行時コンテキスト
- RequestScope: リクエストスコープ
- TenantIsolation: テナント分離
- ResourcePool: リソースプール
- QuotaManager: 配額管理
- LifecycleManager: ライフサイクル管理
"""

from agentflow.runtime.context import (
    RuntimeContext,
    get_env,
    get_runtime_context,
    resolve_settings,
    set_runtime_context,
    use_runtime_context,
)
from agentflow.runtime.init import init_agentflow
from agentflow.runtime.lifecycle import (
    HookInfo,
    HookPriority,
    LifecycleEvent,
    LifecycleManager,
    LifecyclePhase,
    get_lifecycle_manager,
)
from agentflow.runtime.quota_manager import (
    QuotaExceededError,
    QuotaLimit,
    QuotaManager,
    QuotaPeriod,
    QuotaType,
    QuotaUsage,
    TenantQuota,
    get_quota_manager,
)
from agentflow.runtime.request_scope import (
    RequestScope,
    get_current_scope,
    get_current_scope_or_raise,
    require_scope,
)
from agentflow.runtime.resource_pool import (
    PoolConfig,
    PooledResource,
    PoolStats,
    ResourceFactory,
    ResourcePool,
)
from agentflow.runtime.tenant_isolation import (
    TenantConfig,
    TenantContext,
    TenantIsolation,
    get_current_tenant,
    get_current_tenant_or_raise,
    get_tenant_id,
    get_tenant_isolation,
    require_tenant,
)


__all__ = [
    # Lifecycle
    "HookInfo",
    "HookPriority",
    "LifecycleEvent",
    "LifecycleManager",
    "LifecyclePhase",
    # Resource Pool
    "PoolConfig",
    "PoolStats",
    "PooledResource",
    # Quota Manager
    "QuotaExceededError",
    "QuotaLimit",
    "QuotaManager",
    "QuotaPeriod",
    "QuotaType",
    "QuotaUsage",
    # Request Scope
    "RequestScope",
    "ResourceFactory",
    "ResourcePool",
    # Context
    "RuntimeContext",
    # Tenant Isolation
    "TenantConfig",
    "TenantContext",
    "TenantIsolation",
    "TenantQuota",
    "get_current_scope",
    "get_current_scope_or_raise",
    "get_current_tenant",
    "get_current_tenant_or_raise",
    "get_env",
    "get_lifecycle_manager",
    "get_quota_manager",
    "get_runtime_context",
    "get_tenant_id",
    "get_tenant_isolation",
    "init_agentflow",
    "require_scope",
    "require_tenant",
    "resolve_settings",
    "set_runtime_context",
    "use_runtime_context",
]
