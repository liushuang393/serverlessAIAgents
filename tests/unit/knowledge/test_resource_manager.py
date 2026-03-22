"""ResourceManager のユニットテスト."""

from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from infrastructure.security.auth_client.client import AuthClient
from shared.rag.resource_manager import ResourceDefinition, ResourceManager


class TestResourceManager:
    """ResourceManager のテスト."""

    @pytest.fixture
    def mock_auth_client(self) -> MagicMock:
        client = MagicMock()
        client.base_url = "http://auth-service:8010"
        return client

    @pytest.fixture
    def manager(self, mock_auth_client: MagicMock) -> ResourceManager:
        return ResourceManager(auth_client=mock_auth_client)

    def test_resource_definition_dataclass(self) -> None:
        """ResourceDefinition データクラスの基本検証."""
        rd = ResourceDefinition(
            id="rd-1",
            resource_type="vector_db",
            resource_id="faq__default__common",
            display_name="FAQ 共通",
            app_name="faq_system",
            scope="common",
            backend_key="shared",
        )
        assert rd.resource_type == "vector_db"
        assert rd.scope == "common"
        assert rd.is_active is True

    def test_manager_base_url(self, manager: ResourceManager) -> None:
        """base_url が auth_client から取得される."""
        assert manager._base_url == "http://auth-service:8010"

    def test_to_definition(self) -> None:
        """dict → ResourceDefinition 変換."""
        data = {
            "id": "rd-abc",
            "resource_type": "vector_db",
            "resource_id": "test__id",
            "display_name": "Test",
            "app_name": "test_app",
            "scope": "common",
            "backend_key": "shared",
            "metadata": {"key": "value"},
            "is_active": True,
        }
        rd = ResourceManager._to_definition(data)
        assert rd.id == "rd-abc"
        assert rd.metadata == {"key": "value"}

    def test_real_auth_client_config_base_url_is_used(self) -> None:
        """実際の AuthClient でも config.base_url を解決できる."""
        manager = ResourceManager(auth_client=AuthClient(base_url="http://auth-service:8010/"))
        assert manager._base_url == "http://auth-service:8010"
