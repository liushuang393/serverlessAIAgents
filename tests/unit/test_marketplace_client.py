"""MarketplaceClient のテスト.

このテストは MarketplaceClient クラスの機能をテストします。
"""

from pathlib import Path

import pytest

from agentflow.marketplace.client import MarketplaceAgent, MarketplaceClient
from agentflow.marketplace.registry import LocalRegistry


class TestMarketplaceAgent:
    """MarketplaceAgent のテスト."""

    def test_create_agent(self) -> None:
        """エージェントを作成できることをテスト."""
        agent = MarketplaceAgent(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            protocols=["mcp", "a2a"],
            download_url="https://example.com/test-agent.zip",
            dependencies=["dep1", "dep2"],
        )

        assert agent.id == "test-agent"
        assert agent.name == "Test Agent"
        assert agent.version == "1.0.0"
        assert agent.author == "Test Author"
        assert agent.category == "test"
        assert agent.description == "Test description"
        assert agent.protocols == ["mcp", "a2a"]
        assert agent.download_url == "https://example.com/test-agent.zip"
        assert agent.dependencies == ["dep1", "dep2"]


class TestMarketplaceClient:
    """MarketplaceClient のテスト."""

    @pytest.fixture
    def install_dir(self, tmp_path: Path) -> Path:
        """テスト用のインストールディレクトリを作成."""
        return tmp_path / "agents"

    @pytest.fixture
    def registry_path(self, tmp_path: Path) -> Path:
        """テスト用のレジストリパスを作成."""
        return tmp_path / "registry.yaml"

    @pytest.fixture
    def registry(self, registry_path: Path) -> LocalRegistry:
        """テスト用のレジストリを作成."""
        return LocalRegistry(registry_path=registry_path)

    @pytest.fixture
    def client(self, install_dir: Path, registry: LocalRegistry) -> MarketplaceClient:
        """テスト用のクライアントを作成."""
        return MarketplaceClient(
            marketplace_url="https://test.marketplace.dev",
            install_dir=install_dir,
            registry=registry,
        )

    def test_init_with_defaults(self) -> None:
        """デフォルト値で初期化できることをテスト."""
        client = MarketplaceClient()

        assert client.marketplace_url == "https://marketplace.agentflow.dev"
        assert client.install_dir == Path.home() / ".agentflow" / "agents"
        assert client.registry is not None

    def test_init_with_custom_values(self, install_dir: Path, registry: LocalRegistry) -> None:
        """カスタム値で初期化できることをテスト."""
        client = MarketplaceClient(
            marketplace_url="https://custom.marketplace.dev",
            install_dir=install_dir,
            registry=registry,
        )

        assert client.marketplace_url == "https://custom.marketplace.dev"
        assert client.install_dir == install_dir
        assert client.registry == registry

    def test_search_all(self, client: MarketplaceClient) -> None:
        """全エージェントを検索できることをテスト."""
        agents = client.search()

        assert len(agents) == 2
        assert agents[0].id == "pdf-processor"
        assert agents[1].id == "text-analyzer"

    def test_search_with_query(self, client: MarketplaceClient) -> None:
        """クエリでエージェントを検索できることをテスト."""
        agents = client.search(query="pdf")

        assert len(agents) == 1
        assert agents[0].id == "pdf-processor"

    def test_search_with_query_case_insensitive(self, client: MarketplaceClient) -> None:
        """クエリが大文字小文字を区別しないことをテスト."""
        agents = client.search(query="PDF")

        assert len(agents) == 1
        assert agents[0].id == "pdf-processor"

    def test_search_with_category(self, client: MarketplaceClient) -> None:
        """カテゴリでエージェントを検索できることをテスト."""
        agents = client.search(category="document")

        assert len(agents) == 1
        assert agents[0].id == "pdf-processor"

    def test_search_with_protocols(self, client: MarketplaceClient) -> None:
        """プロトコルでエージェントを検索できることをテスト."""
        agents = client.search(protocols=["agui"])

        assert len(agents) == 1
        assert agents[0].id == "text-analyzer"

    def test_search_with_multiple_protocols(self, client: MarketplaceClient) -> None:
        """複数のプロトコルでエージェントを検索できることをテスト."""
        agents = client.search(protocols=["mcp"])

        assert len(agents) == 2  # Both agents support MCP

    def test_search_with_limit(self, client: MarketplaceClient) -> None:
        """制限数でエージェントを検索できることをテスト."""
        agents = client.search(limit=1)

        assert len(agents) == 1

    def test_search_with_combined_filters(self, client: MarketplaceClient) -> None:
        """複数のフィルターを組み合わせて検索できることをテスト."""
        agents = client.search(query="text", category="text", protocols=["mcp"])

        assert len(agents) == 1
        assert agents[0].id == "text-analyzer"

    def test_install_agent(self, client: MarketplaceClient, install_dir: Path) -> None:
        """エージェントをインストールできることをテスト."""
        install_path = client.install("pdf-processor")

        assert install_path == install_dir / "pdf-processor"
        assert install_path.exists()
        assert (install_path / "agent.yaml").exists()

        # レジストリに追加されていることを確認
        assert client.registry.is_installed("pdf-processor")

    def test_install_already_installed(self, client: MarketplaceClient) -> None:
        """既にインストール済みのエージェントのインストールがエラーになることをテスト."""
        client.install("pdf-processor")

        with pytest.raises(ValueError, match="Agent already installed"):
            client.install("pdf-processor")

    def test_install_with_force(self, client: MarketplaceClient, install_dir: Path) -> None:
        """force フラグで既存エージェントを上書きできることをテスト."""
        client.install("pdf-processor")
        install_path = client.install("pdf-processor", force=True)

        assert install_path == install_dir / "pdf-processor"
        assert install_path.exists()

    def test_install_nonexistent_agent(self, client: MarketplaceClient) -> None:
        """存在しないエージェントのインストールがエラーになることをテスト."""
        with pytest.raises(ValueError, match="Agent not found"):
            client.install("nonexistent-agent")

    def test_uninstall_agent(self, client: MarketplaceClient) -> None:
        """エージェントをアンインストールできることをテスト."""
        client.install("pdf-processor")
        assert client.registry.is_installed("pdf-processor")

        result = client.uninstall("pdf-processor")

        assert result is True
        assert not client.registry.is_installed("pdf-processor")

    def test_uninstall_nonexistent_agent(self, client: MarketplaceClient) -> None:
        """存在しないエージェントのアンインストールが False を返すことをテスト."""
        result = client.uninstall("nonexistent-agent")

        assert result is False

    def test_list_installed_empty(self, client: MarketplaceClient) -> None:
        """インストール済みエージェントが空の場合をテスト."""
        agents = client.list_installed()

        assert agents == []

    def test_list_installed_multiple(self, client: MarketplaceClient) -> None:
        """複数のインストール済みエージェントを一覧取得できることをテスト."""
        client.install("pdf-processor")
        client.install("text-analyzer")

        agents = client.list_installed()

        assert len(agents) == 2
        assert agents[0].id == "pdf-processor"
        assert agents[1].id == "text-analyzer"

    def test_close(self, client: MarketplaceClient) -> None:
        """クライアントをクローズできることをテスト."""
        # クローズしてもエラーが発生しないことを確認
        client.close()

    def test_install_creates_agent_yaml_with_protocols(
        self, client: MarketplaceClient, install_dir: Path
    ) -> None:
        """インストール時に agent.yaml が正しく作成されることをテスト."""
        client.install("pdf-processor")

        agent_yaml = install_dir / "pdf-processor" / "agent.yaml"
        assert agent_yaml.exists()

        content = agent_yaml.read_text(encoding="utf-8")
        assert "id: pdf-processor" in content
        assert "name: PDF Processor" in content
        assert "version: 1.0.0" in content
        assert "mcp:" in content
        assert "a2a:" in content
        assert "agui:" in content
