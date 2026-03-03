"""ScopeResolver のユニットテスト."""
from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver


class TestScopeResolver:
    """ScopeResolver のテスト."""

    @pytest.fixture
    def mock_auth_client(self) -> MagicMock:
        client = MagicMock()
        client.base_url = "http://auth-service:8010"
        return client

    @pytest.fixture
    def resolver(self, mock_auth_client: MagicMock) -> ScopeResolver:
        return ScopeResolver(auth_client=mock_auth_client)

    def test_build_collection_name(self, resolver: ScopeResolver) -> None:
        """collection_tpl を展開して collection 名を生成."""
        name = resolver.build_collection_name(
            tpl="{app}__{tenant}__{scope}",
            app_name="faq_system",
            tenant_id="default",
            scope="manager",
        )
        assert name == "faq_system__default__manager"

    def test_build_collection_name_missing_tenant(self, resolver: ScopeResolver) -> None:
        """tenant_id が None の場合 'default' にフォールバック."""
        name = resolver.build_collection_name(
            tpl="{app}__{tenant}__{scope}",
            app_name="faq_system",
            tenant_id=None,
            scope="common",
        )
        assert name == "faq_system__default__common"

    def test_collection_target_defaults(self) -> None:
        """CollectionTarget のデフォルト値を検証."""
        target = CollectionTarget(collection="test", scope="common")
        assert target.backend_key == "shared"
        assert target.vector_url is None
        assert target.permission_level == "read"
        assert target.metadata == {}
