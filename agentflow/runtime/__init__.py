# -*- coding: utf-8 -*-
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
from agentflow.runtime.request_scope import (
    RequestScope,
    get_current_scope,
    get_current_scope_or_raise,
    require_scope,
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
from agentflow.runtime.resource_pool import (
    PoolConfig,
    PoolStats,
    PooledResource,
    ResourceFactory,
    ResourcePool,
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
from agentflow.runtime.lifecycle import (
    HookInfo,
    HookPriority,
    LifecycleEvent,
    LifecycleManager,
    LifecyclePhase,
    get_lifecycle_manager,
)

__all__ = [
    # Context
    "RuntimeContext",
    "get_runtime_context",
    "set_runtime_context",
    "use_runtime_context",
    "resolve_settings",
    "get_env",
    "init_agentflow",
    # Request Scope
    "RequestScope",
    "get_current_scope",
    "get_current_scope_or_raise",
    "require_scope",
    # Tenant Isolation
    "TenantConfig",
    "TenantContext",
    "TenantIsolation",
    "get_current_tenant",
    "get_current_tenant_or_raise",
    "get_tenant_id",
    "get_tenant_isolation",
    "require_tenant",
    # Resource Pool
    "PoolConfig",
    "PoolStats",
    "PooledResource",
    "ResourceFactory",
    "ResourcePool",
    # Quota Manager
    "QuotaExceededError",
    "QuotaLimit",
    "QuotaManager",
    "QuotaPeriod",
    "QuotaType",
    "QuotaUsage",
    "TenantQuota",
    "get_quota_manager",
    # Lifecycle
    "HookInfo",
    "HookPriority",
    "LifecycleEvent",
    "LifecycleManager",
    "LifecyclePhase",
    "get_lifecycle_manager",
]
