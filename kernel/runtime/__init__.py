"""kernel.runtime — マルチテナント環境向けの実行時管理機能.

循環インポートを回避するため遅延インポートを使用。

主要コンポーネント:
- RuntimeContext: 実行時コンテキスト
- RequestScope: リクエストスコープ
- TenantIsolation: テナント分離
- ResourcePool: リソースプール
- QuotaManager: 配額管理
- LifecycleManager: ライフサイクル管理
"""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from kernel.runtime.app_manifest import (
        AppRuntimeHosts,
        AppRuntimePorts,
        AppRuntimeUrls,
        ResolvedAppRuntime,
        resolve_app_runtime,
    )
    from kernel.runtime.bootstrap import AppCapabilityBootstrapper
    from kernel.runtime.capability_bundle import CapabilityBundle
    from kernel.runtime.context import (
        RuntimeContext,
        get_env,
        get_runtime_context,
        resolve_settings,
        set_runtime_context,
        use_runtime_context,
    )
    from kernel.runtime.config_watcher import ConfigWatcher
    from kernel.runtime.init import init_agentflow, init_bizcore
    from kernel.runtime.lifecycle import (
        HookInfo,
        HookPriority,
        LifecycleEvent,
        LifecycleManager,
        LifecyclePhase,
        get_lifecycle_manager,
    )
    from kernel.runtime.lightning import (
        LightningEventRecord,
        LightningStore,
        LightningTracer,
        MemoryLightningStore,
        PromptRewardSample,
        RewardSignal,
        TrajectoryAdapter,
        TransitionSample,
    )
    from kernel.runtime.store import (
        MemoryRunStore,
        RunRecord,
        RunStore,
    )
    from kernel.runtime.lightning_backend import (
        LightningRuntimeConfig,
        LightningTrainingRequest,
        LightningTrainingResult,
        build_optimized_llm_profile,
        is_microsoft_lightning_available,
        resolve_lightning_store,
        train_with_lightning_backend,
    )
    from kernel.runtime.quota_manager import (
        QuotaExceededError,
        QuotaLimit,
        QuotaManager,
        QuotaPeriod,
        QuotaType,
        QuotaUsage,
        TenantQuota,
        get_quota_manager,
    )
    from kernel.runtime.request_scope import (
        RequestScope,
        get_current_scope,
        get_current_scope_or_raise,
        require_scope,
    )
    from kernel.runtime.resource_pool import (
        PoolConfig,
        PooledResource,
        PoolStats,
        ResourceFactory,
        ResourcePool,
    )
    from kernel.runtime.tenant_isolation import (
        TenantConfig,
        TenantContext,
        TenantIsolation,
        get_current_tenant,
        get_current_tenant_or_raise,
        get_tenant_id,
        get_tenant_isolation,
        require_tenant,
    )
    from kernel.runtime.websocket import (
        WSClient,
        WSHandler,
        WSMessage,
        WSMessageType,
        WSMiddleware,
        WSNext,
        WebSocketHub,
        WebSocketProtocol,
    )


__all__ = [
    "AppRuntimeHosts",
    "AppRuntimePorts",
    "AppRuntimeUrls",
    "AppCapabilityBootstrapper",
    "CapabilityBundle",
    "ConfigWatcher",
    "HookInfo",
    "HookPriority",
    "LifecycleEvent",
    "LifecycleManager",
    "LifecyclePhase",
    "LightningEventRecord",
    "LightningRuntimeConfig",
    "LightningStore",
    "LightningTracer",
    "LightningTrainingRequest",
    "LightningTrainingResult",
    "MemoryLightningStore",
    "PoolConfig",
    "PoolStats",
    "PooledResource",
    "PromptRewardSample",
    "QuotaExceededError",
    "QuotaLimit",
    "QuotaManager",
    "QuotaPeriod",
    "QuotaType",
    "QuotaUsage",
    "RequestScope",
    "ResolvedAppRuntime",
    "ResourceFactory",
    "ResourcePool",
    "RewardSignal",
    "RuntimeContext",
    "TenantConfig",
    "TenantContext",
    "TenantIsolation",
    "TenantQuota",
    "TrajectoryAdapter",
    "TransitionSample",
    "WSClient",
    "WSHandler",
    "WSMessage",
    "WSMessageType",
    "WSMiddleware",
    "WSNext",
    "WebSocketHub",
    "WebSocketProtocol",
    "build_optimized_llm_profile",
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
    "init_bizcore",
    "init_agentflow",
    "is_microsoft_lightning_available",
    "require_scope",
    "require_tenant",
    "resolve_app_runtime",
    "resolve_lightning_store",
    "resolve_settings",
    "set_runtime_context",
    "train_with_lightning_backend",
    "use_runtime_context",
    # store
    "MemoryRunStore",
    "RunRecord",
    "RunStore",
]


_SUBMODULE_MAP: dict[str, str] = {
    "AppRuntimeHosts": "kernel.runtime.app_manifest",
    "AppRuntimePorts": "kernel.runtime.app_manifest",
    "AppRuntimeUrls": "kernel.runtime.app_manifest",
    "AppCapabilityBootstrapper": "kernel.runtime.bootstrap",
    "CapabilityBundle": "kernel.runtime.capability_bundle",
    "ConfigWatcher": "kernel.runtime.config_watcher",
    "ResolvedAppRuntime": "kernel.runtime.app_manifest",
    "resolve_app_runtime": "kernel.runtime.app_manifest",
    "RuntimeContext": "kernel.runtime.context",
    "get_env": "kernel.runtime.context",
    "get_runtime_context": "kernel.runtime.context",
    "resolve_settings": "kernel.runtime.context",
    "set_runtime_context": "kernel.runtime.context",
    "use_runtime_context": "kernel.runtime.context",
    "init_bizcore": "kernel.runtime.init",
    "init_agentflow": "kernel.runtime.init",
    "LightningEventRecord": "kernel.runtime.lightning",
    "LightningStore": "kernel.runtime.lightning",
    "LightningTracer": "kernel.runtime.lightning",
    "MemoryLightningStore": "kernel.runtime.lightning",
    "PromptRewardSample": "kernel.runtime.lightning",
    "RewardSignal": "kernel.runtime.lightning",
    "TrajectoryAdapter": "kernel.runtime.lightning",
    "TransitionSample": "kernel.runtime.lightning",
    "LightningRuntimeConfig": "kernel.runtime.lightning_backend",
    "LightningTrainingRequest": "kernel.runtime.lightning_backend",
    "LightningTrainingResult": "kernel.runtime.lightning_backend",
    "build_optimized_llm_profile": "kernel.runtime.lightning_backend",
    "is_microsoft_lightning_available": "kernel.runtime.lightning_backend",
    "resolve_lightning_store": "kernel.runtime.lightning_backend",
    "train_with_lightning_backend": "kernel.runtime.lightning_backend",
    "MemoryRunStore": "kernel.runtime.store",
    "RunRecord": "kernel.runtime.store",
    "RunStore": "kernel.runtime.store",
    "HookInfo": "kernel.runtime.lifecycle",
    "HookPriority": "kernel.runtime.lifecycle",
    "LifecycleEvent": "kernel.runtime.lifecycle",
    "LifecycleManager": "kernel.runtime.lifecycle",
    "LifecyclePhase": "kernel.runtime.lifecycle",
    "get_lifecycle_manager": "kernel.runtime.lifecycle",
    "QuotaExceededError": "kernel.runtime.quota_manager",
    "QuotaLimit": "kernel.runtime.quota_manager",
    "QuotaManager": "kernel.runtime.quota_manager",
    "QuotaPeriod": "kernel.runtime.quota_manager",
    "QuotaType": "kernel.runtime.quota_manager",
    "QuotaUsage": "kernel.runtime.quota_manager",
    "TenantQuota": "kernel.runtime.quota_manager",
    "get_quota_manager": "kernel.runtime.quota_manager",
    "RequestScope": "kernel.runtime.request_scope",
    "get_current_scope": "kernel.runtime.request_scope",
    "get_current_scope_or_raise": "kernel.runtime.request_scope",
    "require_scope": "kernel.runtime.request_scope",
    "PoolConfig": "kernel.runtime.resource_pool",
    "PooledResource": "kernel.runtime.resource_pool",
    "PoolStats": "kernel.runtime.resource_pool",
    "ResourceFactory": "kernel.runtime.resource_pool",
    "ResourcePool": "kernel.runtime.resource_pool",
    "TenantConfig": "kernel.runtime.tenant_isolation",
    "TenantContext": "kernel.runtime.tenant_isolation",
    "TenantIsolation": "kernel.runtime.tenant_isolation",
    "get_current_tenant": "kernel.runtime.tenant_isolation",
    "get_current_tenant_or_raise": "kernel.runtime.tenant_isolation",
    "get_tenant_id": "kernel.runtime.tenant_isolation",
    "get_tenant_isolation": "kernel.runtime.tenant_isolation",
    "require_tenant": "kernel.runtime.tenant_isolation",
    "WSClient": "kernel.runtime.websocket",
    "WSHandler": "kernel.runtime.websocket",
    "WSMessage": "kernel.runtime.websocket",
    "WSMessageType": "kernel.runtime.websocket",
    "WSMiddleware": "kernel.runtime.websocket",
    "WSNext": "kernel.runtime.websocket",
    "WebSocketHub": "kernel.runtime.websocket",
    "WebSocketProtocol": "kernel.runtime.websocket",
}


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    if name in _SUBMODULE_MAP:
        mod = importlib.import_module(_SUBMODULE_MAP[name])
        return getattr(mod, name)
    msg = f"module 'kernel.runtime' has no attribute {name!r}"
    raise AttributeError(msg)
