"""AgentFlow マーケットプレイスのテスト."""

from datetime import datetime
from pathlib import Path

import pytest

from agentflow.marketplace import AgentRegistryEntry, LocalRegistry, MarketplaceClient


class TestLocalRegistry:
    """LocalRegistry クラスのテスト."""

    def test_registry_initialization(self, tmp_path: Path) -> None:
        """レジストリが正しく初期化されることをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)

        assert registry.registry_path == registry_path
        assert registry_path.exists()

    def test_add_agent(self, tmp_path: Path) -> None:
        """エージェントを追加できることをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at=datetime.now().isoformat(),
        )

        registry.add_agent(entry)

        # エージェントが追加されたことを確認
        retrieved = registry.get_agent("test-agent")
        assert retrieved is not None
        assert retrieved.id == "test-agent"
        assert retrieved.name == "Test Agent"

    def test_remove_agent(self, tmp_path: Path) -> None:
        """エージェントを削除できることをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at=datetime.now().isoformat(),
        )

        registry.add_agent(entry)
        assert registry.is_installed("test-agent")

        # エージェントを削除
        success = registry.remove_agent("test-agent")
        assert success
        assert not registry.is_installed("test-agent")

    def test_remove_nonexistent_agent(self, tmp_path: Path) -> None:
        """存在しないエージェントの削除が失敗することをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        success = registry.remove_agent("nonexistent")
        assert not success

    def test_get_agent(self, tmp_path: Path) -> None:
        """エージェント情報を取得できることをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at=datetime.now().isoformat(),
        )

        registry.add_agent(entry)

        retrieved = registry.get_agent("test-agent")
        assert retrieved is not None
        assert retrieved.id == "test-agent"

    def test_get_nonexistent_agent(self, tmp_path: Path) -> None:
        """存在しないエージェントの取得が None を返すことをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        retrieved = registry.get_agent("nonexistent")
        assert retrieved is None

    def test_list_agents(self, tmp_path: Path) -> None:
        """エージェント一覧を取得できることをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        # 複数のエージェントを追加
        for i in range(3):
            entry = AgentRegistryEntry(
                id=f"agent-{i}",
                name=f"Agent {i}",
                version="1.0.0",
                author="Test Author",
                category="test",
                description=f"Test agent {i}",
                install_path=f"/path/to/agent-{i}",
                installed_at=datetime.now().isoformat(),
            )
            registry.add_agent(entry)

        agents = registry.list_agents()
        assert len(agents) == 3
        assert all(isinstance(agent, AgentRegistryEntry) for agent in agents)

    def test_is_installed(self, tmp_path: Path) -> None:
        """エージェントのインストール状態を確認できることをテスト."""
        registry = LocalRegistry(registry_path=tmp_path / "registry.yaml")

        entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at=datetime.now().isoformat(),
        )

        assert not registry.is_installed("test-agent")

        registry.add_agent(entry)
        assert registry.is_installed("test-agent")


class TestMarketplaceClient:
    """MarketplaceClient クラスのテスト."""

    def test_client_initialization(self, tmp_path: Path) -> None:
        """クライアントが正しく初期化されることをテスト."""
        install_dir = tmp_path / "agents"
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)

        client = MarketplaceClient(
            install_dir=install_dir,
            registry=registry,
        )

        assert client.install_dir == install_dir
        assert install_dir.exists()
        assert client.registry == registry

        client.close()

    def test_search_all(self, tmp_path: Path) -> None:
        """全エージェントを検索できることをテスト."""
        client = MarketplaceClient(install_dir=tmp_path / "agents")

        results = client.search()
        assert len(results) > 0
        assert all(hasattr(agent, "id") for agent in results)

        client.close()

    def test_search_with_query(self, tmp_path: Path) -> None:
        """クエリでエージェントを検索できることをテスト."""
        client = MarketplaceClient(install_dir=tmp_path / "agents")

        results = client.search(query="PDF")
        assert len(results) > 0
        assert any("pdf" in agent.name.lower() for agent in results)

        client.close()

    def test_search_with_category(self, tmp_path: Path) -> None:
        """カテゴリでエージェントをフィルターできることをテスト."""
        client = MarketplaceClient(install_dir=tmp_path / "agents")

        results = client.search(category="document")
        assert all(agent.category == "document" for agent in results)

        client.close()

    def test_search_with_protocols(self, tmp_path: Path) -> None:
        """プロトコルでエージェントをフィルターできることをテスト."""
        client = MarketplaceClient(install_dir=tmp_path / "agents")

        results = client.search(protocols=["mcp"])
        assert all("mcp" in agent.protocols for agent in results)

        client.close()

    def test_install_agent(self, tmp_path: Path) -> None:
        """エージェントをインストールできることをテスト."""
        install_dir = tmp_path / "agents"
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=install_dir, registry=registry)

        install_path = client.install("pdf-processor")

        assert install_path.exists()
        assert (install_path / "agent.yaml").exists()
        assert client.registry.is_installed("pdf-processor")

        client.close()

    def test_install_already_installed(self, tmp_path: Path) -> None:
        """既にインストール済みのエージェントのインストールが失敗することをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=tmp_path / "agents", registry=registry)

        client.install("pdf-processor")

        with pytest.raises(ValueError, match="already installed"):
            client.install("pdf-processor")

        client.close()

    def test_install_with_force(self, tmp_path: Path) -> None:
        """force フラグで既存エージェントを上書きできることをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=tmp_path / "agents", registry=registry)

        client.install("pdf-processor")
        install_path = client.install("pdf-processor", force=True)

        assert install_path.exists()

        client.close()

    def test_install_nonexistent_agent(self, tmp_path: Path) -> None:
        """存在しないエージェントのインストールが失敗することをテスト."""
        client = MarketplaceClient(install_dir=tmp_path / "agents")

        with pytest.raises(ValueError, match="not found"):
            client.install("nonexistent-agent")

        client.close()

    def test_uninstall_agent(self, tmp_path: Path) -> None:
        """エージェントをアンインストールできることをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=tmp_path / "agents", registry=registry)

        client.install("pdf-processor")
        assert client.registry.is_installed("pdf-processor")

        success = client.uninstall("pdf-processor")
        assert success
        assert not client.registry.is_installed("pdf-processor")

        client.close()

    def test_uninstall_nonexistent_agent(self, tmp_path: Path) -> None:
        """存在しないエージェントのアンインストールが失敗することをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=tmp_path / "agents", registry=registry)

        success = client.uninstall("nonexistent")
        assert not success

        client.close()

    def test_list_installed(self, tmp_path: Path) -> None:
        """インストール済みエージェントを一覧取得できることをテスト."""
        registry_path = tmp_path / "registry.yaml"
        registry = LocalRegistry(registry_path=registry_path)
        client = MarketplaceClient(install_dir=tmp_path / "agents", registry=registry)

        # エージェントをインストール
        client.install("pdf-processor")
        client.install("text-analyzer")

        agents = client.list_installed()
        assert len(agents) == 2
        assert any(agent.id == "pdf-processor" for agent in agents)
        assert any(agent.id == "text-analyzer" for agent in agents)

        client.close()

