"""マルチテナント単体テスト."""

from __future__ import annotations

import pytest

from agentflow.multi_tenant import ResourceLimits, TenantContext, TenantManager
from agentflow.multi_tenant.context import IsolationLevel
from agentflow.multi_tenant.manager import get_current_tenant, require_tenant


class TestResourceLimits:
    """ResourceLimits テスト."""

    def test_default(self) -> None:
        """デフォルト制限."""
        limits = ResourceLimits.default()
        assert limits.max_requests_per_minute == 60
        assert limits.max_concurrent_tasks == 10

    def test_unlimited(self) -> None:
        """無制限."""
        limits = ResourceLimits.unlimited()
        assert limits.max_requests_per_minute == 999999

    def test_enterprise(self) -> None:
        """エンタープライズ制限."""
        limits = ResourceLimits.enterprise()
        assert limits.max_requests_per_minute == 300
        assert limits.max_concurrent_tasks == 50

    def test_to_dict(self) -> None:
        """辞書変換."""
        limits = ResourceLimits(max_requests_per_minute=100)
        data = limits.to_dict()
        assert data["max_requests_per_minute"] == 100


class TestTenantContext:
    """TenantContext テスト."""

    def test_create(self) -> None:
        """作成."""
        tenant = TenantContext(
            tenant_id="acme-corp",
            organization_name="Acme Corporation",
        )
        assert tenant.tenant_id == "acme-corp"
        assert tenant.organization_name == "Acme Corporation"
        assert tenant.isolation_level == IsolationLevel.SHARED

    def test_storage_prefix(self) -> None:
        """ストレージプレフィックス."""
        tenant = TenantContext(tenant_id="acme-corp")
        assert tenant.storage_prefix == "tenant:acme-corp:"

    def test_prefixed_key(self) -> None:
        """プレフィックス付きキー."""
        tenant = TenantContext(tenant_id="acme-corp")
        assert tenant.prefixed_key("users") == "tenant:acme-corp:users"
        # 既にプレフィックスがある場合は二重付与しない
        assert tenant.prefixed_key("tenant:acme-corp:users") == "tenant:acme-corp:users"

    def test_strip_prefix(self) -> None:
        """プレフィックス除去."""
        tenant = TenantContext(tenant_id="acme-corp")
        assert tenant.strip_prefix("tenant:acme-corp:users") == "users"
        assert tenant.strip_prefix("other:key") == "other:key"

    def test_to_dict_from_dict(self) -> None:
        """辞書変換・復元."""
        tenant = TenantContext(
            tenant_id="test",
            organization_name="Test Org",
            isolation_level=IsolationLevel.DEDICATED,
            resource_limits=ResourceLimits(max_requests_per_minute=200),
            config={"feature_x": True},
        )

        data = tenant.to_dict()
        restored = TenantContext.from_dict(data)

        assert restored.tenant_id == "test"
        assert restored.organization_name == "Test Org"
        assert restored.isolation_level == IsolationLevel.DEDICATED
        assert restored.resource_limits.max_requests_per_minute == 200
        assert restored.config["feature_x"] is True


class TestTenantManager:
    """TenantManager テスト."""

    @pytest.fixture
    def manager(self) -> TenantManager:
        """マネージャーのフィクスチャ."""
        return TenantManager()

    @pytest.mark.asyncio
    async def test_create_tenant(self, manager: TenantManager) -> None:
        """テナント作成."""
        tenant = await manager.create_tenant(
            tenant_id="acme-corp",
            organization_name="Acme Corporation",
        )
        assert tenant.tenant_id == "acme-corp"
        assert "acme-corp" in manager.list_tenants()

    @pytest.mark.asyncio
    async def test_create_duplicate_tenant(self, manager: TenantManager) -> None:
        """重複テナント作成."""
        await manager.create_tenant("acme-corp")
        with pytest.raises(ValueError, match="既に存在します"):
            await manager.create_tenant("acme-corp")

    @pytest.mark.asyncio
    async def test_get_tenant(self, manager: TenantManager) -> None:
        """テナント取得."""
        await manager.create_tenant("acme-corp")

        tenant = manager.get_tenant("acme-corp")
        assert tenant is not None
        assert tenant.tenant_id == "acme-corp"

        assert manager.get_tenant("nonexistent") is None

    @pytest.mark.asyncio
    async def test_delete_tenant(self, manager: TenantManager) -> None:
        """テナント削除."""
        await manager.create_tenant("acme-corp")

        assert manager.delete_tenant("acme-corp") is True
        assert manager.get_tenant("acme-corp") is None
        assert manager.delete_tenant("acme-corp") is False

    @pytest.mark.asyncio
    async def test_list_tenants(self, manager: TenantManager) -> None:
        """テナント一覧."""
        await manager.create_tenant("tenant-1")
        await manager.create_tenant("tenant-2")

        tenants = manager.list_tenants()
        assert len(tenants) == 2
        assert "tenant-1" in tenants
        assert "tenant-2" in tenants

    @pytest.mark.asyncio
    async def test_set_get_current(self, manager: TenantManager) -> None:
        """現在のテナント設定・取得."""
        await manager.create_tenant("acme-corp")

        # 設定前
        assert manager.get_current() is None

        # 設定
        manager.set_current("acme-corp")
        current = manager.get_current()
        assert current is not None
        assert current.tenant_id == "acme-corp"

        # クリア
        manager.clear_current()
        assert manager.get_current() is None

    @pytest.mark.asyncio
    async def test_set_current_nonexistent(self, manager: TenantManager) -> None:
        """存在しないテナントの設定."""
        with pytest.raises(ValueError, match="見つかりません"):
            manager.set_current("nonexistent")

    @pytest.mark.asyncio
    async def test_update_limits(self, manager: TenantManager) -> None:
        """リソース制限更新."""
        await manager.create_tenant("acme-corp")

        new_limits = ResourceLimits(max_requests_per_minute=500)
        assert manager.update_limits("acme-corp", new_limits) is True

        tenant = manager.get_tenant("acme-corp")
        assert tenant is not None
        assert tenant.resource_limits.max_requests_per_minute == 500

        assert manager.update_limits("nonexistent", new_limits) is False

    @pytest.mark.asyncio
    async def test_apply_filter(self, manager: TenantManager) -> None:
        """クエリフィルタ."""
        await manager.create_tenant("acme-corp")
        manager.set_current("acme-corp")

        query = {"name": "test", "status": "active"}
        filtered = manager.apply_filter(query)

        assert filtered["name"] == "test"
        assert filtered["status"] == "active"
        assert filtered["tenant_id"] == "acme-corp"

        # テナント未設定の場合
        manager.clear_current()
        unfiltered = manager.apply_filter(query)
        assert "tenant_id" not in unfiltered


class TestHelperFunctions:
    """ヘルパー関数テスト."""

    @pytest.fixture(autouse=True)
    def clear_tenant(self) -> None:
        """各テスト後にテナントをクリア."""
        yield
        from agentflow.multi_tenant.manager import _current_tenant

        _current_tenant.set(None)

    def test_get_current_tenant(self) -> None:
        """get_current_tenant."""
        assert get_current_tenant() is None

    def test_require_tenant_not_set(self) -> None:
        """require_tenant（未設定）."""
        with pytest.raises(RuntimeError, match="テナントが設定されていません"):
            require_tenant()

    @pytest.mark.asyncio
    async def test_require_tenant_set(self) -> None:
        """require_tenant（設定済み）."""
        manager = TenantManager()
        await manager.create_tenant("test")
        manager.set_current("test")

        tenant = require_tenant()
        assert tenant.tenant_id == "test"
