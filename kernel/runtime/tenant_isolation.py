"""Tenant Isolation - テナント隔離.

マルチテナント環境でのデータおよびリソース隔離。

使用例:
    >>> isolation = TenantIsolation()
    >>> async with isolation.tenant_context("tenant-123"):
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
    """テナント設定.

    Attributes:
        tenant_id: テナントID
        name: テナント名
        tier: サービス層級
        max_requests_per_minute: 毎分最大リクエスト数
        max_tokens_per_day: 日次最大トークン数
        allowed_models: 許可モデル
        features: 有効機能
        metadata: メタデータ
    """

    tenant_id: str
    name: str = ""
    tier: str = "free"
    max_requests_per_minute: int = 60
    max_tokens_per_day: int = 100000
    allowed_models: list[str] = field(default_factory=list)
    features: set[str] = field(default_factory=set)
    metadata: dict[str, Any] = field(default_factory=dict)

    def has_feature(self, feature: str) -> bool:
        """機能が有効か確認."""
        return feature in self.features

    def is_model_allowed(self, model: str) -> bool:
        """モデルが許可されているか確認."""
        if not self.allowed_models:
            return True
        return model in self.allowed_models

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
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
    """テナントコンテキスト.

    Attributes:
        config: テナント設定
        data_prefix: データプレフィックス（隔離ストレージ用）
        cache_namespace: キャッシュネームスペース
    """

    config: TenantConfig
    data_prefix: str = ""
    cache_namespace: str = ""

    def __post_init__(self) -> None:
        """初期化."""
        if not self.data_prefix:
            self.data_prefix = f"tenant:{self.config.tenant_id}:"
        if not self.cache_namespace:
            self.cache_namespace = f"cache:{self.config.tenant_id}:"

    def prefix_key(self, key: str) -> str:
        """キーにテナントプレフィックスを追加."""
        return f"{self.data_prefix}{key}"

    def cache_key(self, key: str) -> str:
        """キャッシュキーにテナントプレフィックスを追加."""
        return f"{self.cache_namespace}{key}"


_current_tenant: ContextVar[TenantContext | None] = ContextVar("agentflow_tenant_context", default=None)


class TenantIsolation:
    """テナント隔離管理器.

    マルチテナント環境での隔離と設定を管理。
    """

    def __init__(self) -> None:
        """初期化."""
        self._configs: dict[str, TenantConfig] = {}
        self._default_config = TenantConfig(tenant_id="default")

    def register_tenant(self, config: TenantConfig) -> None:
        """テナント設定を登録."""
        self._configs[config.tenant_id] = config
        _logger.debug(f"Registered tenant: {config.tenant_id}")

    def get_tenant_config(self, tenant_id: str) -> TenantConfig:
        """テナント設定を取得."""
        return self._configs.get(tenant_id, self._default_config)

    def set_default_config(self, config: TenantConfig) -> None:
        """デフォルト設定を設定."""
        self._default_config = config

    @asynccontextmanager
    async def tenant_context(
        self,
        tenant_id: str,
        config: TenantConfig | None = None,
    ) -> AsyncIterator[TenantContext]:
        """テナントコンテキストを作成."""
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
        """登録済み全テナントを列挙."""
        return list(self._configs.keys())


def get_current_tenant() -> TenantContext | None:
    """現在のテナントコンテキストを取得."""
    return _current_tenant.get()


def get_current_tenant_or_raise() -> TenantContext:
    """現在のテナントコンテキストを取得（存在しない場合例外）."""
    context = get_current_tenant()
    if context is None:
        msg = "No active tenant context"
        raise RuntimeError(msg)
    return context


def get_tenant_id() -> str | None:
    """現在のテナントIDを取得."""
    context = get_current_tenant()
    if context is None:
        return None
    return context.config.tenant_id


def require_tenant() -> TenantContext:
    """現在のテナントコンテキストを取得（エイリアス）."""
    return get_current_tenant_or_raise()


# グローバルインスタンス
_isolation = TenantIsolation()


def get_tenant_isolation() -> TenantIsolation:
    """グローバルテナント隔離管理器を取得."""
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
