"""Tenant Isolation - 租户隔离.

多租户环境下的数据和资源隔离。

使用例:
    >>> isolation = TenantIsolation()
    >>> async with isolation.tenant_context("tenant-123"):
    ...     # 此作用域内的所有操作都限定在该租户内
    ...     data = await get_tenant_data()
"""

from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from contextvars import ContextVar
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


_logger = logging.getLogger(__name__)


@dataclass
class TenantConfig:
    """租户配置.

    Attributes:
        tenant_id: 租户ID
        name: 租户名称
        tier: 服务层级
        max_requests_per_minute: 每分钟最大请求数
        max_tokens_per_day: 每日最大token数
        allowed_models: 允许使用的模型
        features: 启用的功能
        metadata: 元数据
    """

    tenant_id: str
    name: str = ""
    tier: str = "free"  # free, basic, pro, enterprise
    max_requests_per_minute: int = 60
    max_tokens_per_day: int = 100000
    allowed_models: list[str] = field(default_factory=list)
    features: set[str] = field(default_factory=set)
    metadata: dict[str, Any] = field(default_factory=dict)

    def has_feature(self, feature: str) -> bool:
        """检查是否启用了某功能."""
        return feature in self.features

    def is_model_allowed(self, model: str) -> bool:
        """检查是否允许使用某模型."""
        if not self.allowed_models:
            return True  # 未限制
        return model in self.allowed_models

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "tenant_id": self.tenant_id,
            "name": self.name,
            "tier": self.tier,
            "max_requests_per_minute": self.max_requests_per_minute,
            "max_tokens_per_day": self.max_tokens_per_day,
            "allowed_models": self.allowed_models,
            "features": list(self.features),
            "metadata": self.metadata,
        }


@dataclass
class TenantContext:
    """租户上下文.

    Attributes:
        config: 租户配置
        data_prefix: 数据前缀（用于隔离存储）
        cache_namespace: 缓存命名空间
    """

    config: TenantConfig
    data_prefix: str = ""
    cache_namespace: str = ""

    def __post_init__(self) -> None:
        """初始化."""
        if not self.data_prefix:
            self.data_prefix = f"tenant:{self.config.tenant_id}:"
        if not self.cache_namespace:
            self.cache_namespace = f"cache:{self.config.tenant_id}:"

    def prefix_key(self, key: str) -> str:
        """给key添加租户前缀."""
        return f"{self.data_prefix}{key}"

    def cache_key(self, key: str) -> str:
        """给缓存key添加租户前缀."""
        return f"{self.cache_namespace}{key}"


# 当前租户上下文
_current_tenant: ContextVar[TenantContext | None] = ContextVar(
    "agentflow_tenant_context", default=None
)


class TenantIsolation:
    """租户隔离管理器.

    管理多租户环境下的隔离和配置。
    """

    def __init__(self) -> None:
        """初始化."""
        self._configs: dict[str, TenantConfig] = {}
        self._default_config = TenantConfig(tenant_id="default")

    def register_tenant(self, config: TenantConfig) -> None:
        """注册租户配置.

        Args:
            config: 租户配置
        """
        self._configs[config.tenant_id] = config
        _logger.debug(f"Registered tenant: {config.tenant_id}")

    def get_tenant_config(self, tenant_id: str) -> TenantConfig:
        """获取租户配置.

        Args:
            tenant_id: 租户ID

        Returns:
            租户配置
        """
        return self._configs.get(tenant_id, self._default_config)

    def set_default_config(self, config: TenantConfig) -> None:
        """设置默认配置.

        Args:
            config: 默认租户配置
        """
        self._default_config = config

    @asynccontextmanager
    async def tenant_context(
        self,
        tenant_id: str,
        config: TenantConfig | None = None,
    ) -> AsyncIterator[TenantContext]:
        """创建租户上下文.

        Args:
            tenant_id: 租户ID
            config: 可选的租户配置覆盖

        Yields:
            租户上下文
        """
        tenant_config = config or self.get_tenant_config(tenant_id)
        context = TenantContext(config=tenant_config)

        token = _current_tenant.set(context)
        _logger.debug(f"Entered tenant context: {tenant_id}")

        try:
            yield context
        finally:
            _current_tenant.reset(token)
            _logger.debug(f"Exited tenant context: {tenant_id}")

    def list_tenants(self) -> list[str]:
        """列出所有已注册的租户."""
        return list(self._configs.keys())


def get_current_tenant() -> TenantContext | None:
    """获取当前租户上下文."""
    return _current_tenant.get()


def get_current_tenant_or_raise() -> TenantContext:
    """获取当前租户上下文（不存在则抛出异常）."""
    context = get_current_tenant()
    if context is None:
        msg = "No active tenant context"
        raise RuntimeError(msg)
    return context


def get_tenant_id() -> str | None:
    """获取当前租户ID."""
    context = get_current_tenant()
    if context is None:
        return None
    return context.config.tenant_id


def require_tenant() -> TenantContext:
    """获取当前租户上下文（别名）."""
    return get_current_tenant_or_raise()


# 全局实例
_isolation = TenantIsolation()


def get_tenant_isolation() -> TenantIsolation:
    """获取全局租户隔离管理器."""
    return _isolation


__all__ = [
    "TenantConfig",
    "TenantContext",
    "TenantIsolation",
    "get_current_tenant",
    "get_current_tenant_or_raise",
    "get_tenant_id",
    "get_tenant_isolation",
    "require_tenant",
]
