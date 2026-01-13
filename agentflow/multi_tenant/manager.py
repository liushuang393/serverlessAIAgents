# -*- coding: utf-8 -*-
"""テナントマネージャー.

テナントの管理と切り替えを行う。
"""

from __future__ import annotations

import logging
from contextvars import ContextVar
from typing import Any

from agentflow.multi_tenant.context import TenantContext, ResourceLimits, IsolationLevel

logger = logging.getLogger(__name__)

# コンテキスト変数（非同期セーフなテナント状態管理）
_current_tenant: ContextVar[TenantContext | None] = ContextVar("current_tenant", default=None)


class TenantManager:
    """テナントマネージャー.

    テナントの作成、取得、切り替えを管理。
    スレッドセーフな ContextVar を使用。

    Example:
        >>> manager = TenantManager()
        >>> await manager.create_tenant("acme-corp", "Acme Corporation")
        >>> manager.set_current("acme-corp")
        >>> tenant = manager.get_current()
    """

    def __init__(self) -> None:
        """初期化."""
        self._tenants: dict[str, TenantContext] = {}
        self._logger = logging.getLogger(__name__)

    async def create_tenant(
        self,
        tenant_id: str,
        organization_name: str | None = None,
        organization_id: str | None = None,
        isolation_level: IsolationLevel = IsolationLevel.SHARED,
        resource_limits: ResourceLimits | None = None,
        config: dict[str, Any] | None = None,
    ) -> TenantContext:
        """テナントを作成.

        Args:
            tenant_id: テナントID（一意）
            organization_name: 組織名
            organization_id: 組織ID
            isolation_level: 分離レベル
            resource_limits: リソース制限
            config: カスタム設定

        Returns:
            作成されたテナントコンテキスト

        Raises:
            ValueError: テナントIDが既に存在する場合
        """
        if tenant_id in self._tenants:
            raise ValueError(f"テナント '{tenant_id}' は既に存在します")

        tenant = TenantContext(
            tenant_id=tenant_id,
            organization_id=organization_id,
            organization_name=organization_name,
            isolation_level=isolation_level,
            resource_limits=resource_limits or ResourceLimits.default(),
            config=config or {},
        )

        self._tenants[tenant_id] = tenant
        self._logger.info(f"テナント作成: {tenant_id}")

        return tenant

    def get_tenant(self, tenant_id: str) -> TenantContext | None:
        """テナントを取得.

        Args:
            tenant_id: テナントID

        Returns:
            テナントコンテキスト（存在しない場合はNone）
        """
        return self._tenants.get(tenant_id)

    def list_tenants(self) -> list[str]:
        """テナント一覧を取得.

        Returns:
            テナントIDのリスト
        """
        return list(self._tenants.keys())

    def delete_tenant(self, tenant_id: str) -> bool:
        """テナントを削除.

        Args:
            tenant_id: テナントID

        Returns:
            削除成功の場合True
        """
        if tenant_id in self._tenants:
            del self._tenants[tenant_id]
            self._logger.info(f"テナント削除: {tenant_id}")
            return True
        return False

    def set_current(self, tenant_id: str) -> None:
        """現在のテナントを設定.

        ContextVar を使用してスレッドセーフに設定。

        Args:
            tenant_id: テナントID

        Raises:
            ValueError: テナントが存在しない場合
        """
        tenant = self.get_tenant(tenant_id)
        if tenant is None:
            raise ValueError(f"テナント '{tenant_id}' が見つかりません")

        _current_tenant.set(tenant)
        self._logger.debug(f"現在のテナントを設定: {tenant_id}")

    def get_current(self) -> TenantContext | None:
        """現在のテナントを取得.

        Returns:
            現在のテナントコンテキスト（設定されていない場合はNone）
        """
        return _current_tenant.get()

    def clear_current(self) -> None:
        """現在のテナントをクリア."""
        _current_tenant.set(None)

    def update_limits(
        self,
        tenant_id: str,
        limits: ResourceLimits,
    ) -> bool:
        """テナントのリソース制限を更新.

        Args:
            tenant_id: テナントID
            limits: 新しいリソース制限

        Returns:
            更新成功の場合True
        """
        tenant = self.get_tenant(tenant_id)
        if tenant is None:
            return False

        tenant.resource_limits = limits
        self._logger.info(f"テナント制限更新: {tenant_id}")
        return True

    def apply_filter(self, query: dict[str, Any]) -> dict[str, Any]:
        """クエリにテナントフィルタを適用.

        データベースクエリなどにテナントIDフィルタを追加。

        Args:
            query: 元のクエリ

        Returns:
            テナントフィルタ付きクエリ
        """
        tenant = self.get_current()
        if tenant is None:
            return query

        # 新しい辞書を作成してテナントIDを追加
        filtered = query.copy()
        filtered["tenant_id"] = tenant.tenant_id

        return filtered


# グローバルインスタンス（シングルトン的使用）
_default_manager: TenantManager | None = None


def get_tenant_manager() -> TenantManager:
    """デフォルトのテナントマネージャーを取得.

    Returns:
        TenantManager インスタンス
    """
    global _default_manager
    if _default_manager is None:
        _default_manager = TenantManager()
    return _default_manager


def get_current_tenant() -> TenantContext | None:
    """現在のテナントを取得（便利関数）.

    Returns:
        現在のテナントコンテキスト
    """
    return _current_tenant.get()


def require_tenant() -> TenantContext:
    """現在のテナントを取得（必須）.

    Returns:
        現在のテナントコンテキスト

    Raises:
        RuntimeError: テナントが設定されていない場合
    """
    tenant = _current_tenant.get()
    if tenant is None:
        raise RuntimeError("テナントが設定されていません")
    return tenant
