"""LocalRegistry のテスト.

このテストは LocalRegistry クラスの機能をテストします。
"""

from datetime import UTC, datetime
from pathlib import Path

import pytest

from agentflow.marketplace.registry import AgentRegistryEntry, LocalRegistry


class TestAgentRegistryEntry:
    """AgentRegistryEntry のテスト."""

    def test_create_entry(self) -> None:
        """エントリを作成できることをテスト."""
        entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at="2025-01-01T00:00:00Z",
        )

        assert entry.id == "test-agent"
        assert entry.name == "Test Agent"
        assert entry.version == "1.0.0"
        assert entry.author == "Test Author"
        assert entry.category == "test"
        assert entry.description == "Test description"
        assert entry.install_path == "/path/to/agent"
        assert entry.installed_at == "2025-01-01T00:00:00Z"


class TestLocalRegistry:
    """LocalRegistry のテスト."""

    @pytest.fixture
    def registry_path(self, tmp_path: Path) -> Path:
        """テスト用のレジストリパスを作成."""
        return tmp_path / "registry.yaml"

    @pytest.fixture
    def registry(self, registry_path: Path) -> LocalRegistry:
        """テスト用のレジストリを作成."""
        return LocalRegistry(registry_path=registry_path)

    @pytest.fixture
    def sample_entry(self) -> AgentRegistryEntry:
        """サンプルエントリを作成."""
        return AgentRegistryEntry(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            category="test",
            description="Test description",
            install_path="/path/to/agent",
            installed_at=datetime.now(UTC).isoformat(),
        )

    def test_init_creates_registry_file(self, registry_path: Path) -> None:
        """初期化時にレジストリファイルが作成されることをテスト."""
        assert not registry_path.exists()

        LocalRegistry(registry_path=registry_path)

        assert registry_path.exists()
        content = registry_path.read_text(encoding="utf-8")
        assert "agents:" in content

    def test_init_with_default_path(self) -> None:
        """デフォルトパスで初期化できることをテスト."""
        registry = LocalRegistry()

        expected_path = Path.home() / ".agentflow" / "registry.yaml"
        assert registry.registry_path == expected_path

    def test_add_agent(self, registry: LocalRegistry, sample_entry: AgentRegistryEntry) -> None:
        """エージェントを追加できることをテスト."""
        registry.add_agent(sample_entry)

        agents = registry.list_agents()
        assert len(agents) == 1
        assert agents[0].id == "test-agent"
        assert agents[0].name == "Test Agent"

    def test_add_agent_updates_existing(self, registry: LocalRegistry, sample_entry: AgentRegistryEntry) -> None:
        """既存エージェントを更新できることをテスト."""
        # 最初のエントリを追加
        registry.add_agent(sample_entry)

        # 同じ ID で異なるバージョンを追加
        updated_entry = AgentRegistryEntry(
            id="test-agent",
            name="Test Agent Updated",
            version="2.0.0",
            author="Test Author",
            category="test",
            description="Updated description",
            install_path="/path/to/agent",
            installed_at=datetime.now(UTC).isoformat(),
        )
        registry.add_agent(updated_entry)

        # 1つのエントリのみ存在し、更新されていることを確認
        agents = registry.list_agents()
        assert len(agents) == 1
        assert agents[0].version == "2.0.0"
        assert agents[0].name == "Test Agent Updated"

    def test_remove_agent(self, registry: LocalRegistry, sample_entry: AgentRegistryEntry) -> None:
        """エージェントを削除できることをテスト."""
        registry.add_agent(sample_entry)
        assert len(registry.list_agents()) == 1

        result = registry.remove_agent("test-agent")

        assert result is True
        assert len(registry.list_agents()) == 0

    def test_remove_nonexistent_agent(self, registry: LocalRegistry) -> None:
        """存在しないエージェントの削除が False を返すことをテスト."""
        result = registry.remove_agent("nonexistent-agent")

        assert result is False

    def test_get_agent(self, registry: LocalRegistry, sample_entry: AgentRegistryEntry) -> None:
        """エージェント情報を取得できることをテスト."""
        registry.add_agent(sample_entry)

        agent = registry.get_agent("test-agent")

        assert agent is not None
        assert agent.id == "test-agent"
        assert agent.name == "Test Agent"

    def test_get_nonexistent_agent(self, registry: LocalRegistry) -> None:
        """存在しないエージェントの取得が None を返すことをテスト."""
        agent = registry.get_agent("nonexistent-agent")

        assert agent is None

    def test_list_agents_empty(self, registry: LocalRegistry) -> None:
        """空のレジストリで一覧取得できることをテスト."""
        agents = registry.list_agents()

        assert agents == []

    def test_list_agents_multiple(self, registry: LocalRegistry) -> None:
        """複数のエージェントを一覧取得できることをテスト."""
        # 3つのエージェントを追加
        for i in range(3):
            entry = AgentRegistryEntry(
                id=f"agent-{i}",
                name=f"Agent {i}",
                version="1.0.0",
                author="Test Author",
                category="test",
                description=f"Agent {i} description",
                install_path=f"/path/to/agent-{i}",
                installed_at=datetime.now(UTC).isoformat(),
            )
            registry.add_agent(entry)

        agents = registry.list_agents()

        assert len(agents) == 3
        assert agents[0].id == "agent-0"
        assert agents[1].id == "agent-1"
        assert agents[2].id == "agent-2"

    def test_is_installed(self, registry: LocalRegistry, sample_entry: AgentRegistryEntry) -> None:
        """エージェントがインストール済みかを確認できることをテスト."""
        assert registry.is_installed("test-agent") is False

        registry.add_agent(sample_entry)

        assert registry.is_installed("test-agent") is True

    def test_load_registry_with_empty_file(self, registry_path: Path) -> None:
        """空のレジストリファイルを読み込めることをテスト."""
        # 空のファイルを作成
        registry_path.parent.mkdir(parents=True, exist_ok=True)
        registry_path.write_text("", encoding="utf-8")

        registry = LocalRegistry(registry_path=registry_path)
        agents = registry.list_agents()

        assert agents == []

    def test_persistence(self, registry_path: Path, sample_entry: AgentRegistryEntry) -> None:
        """レジストリが永続化されることをテスト."""
        # 最初のレジストリでエージェントを追加
        registry1 = LocalRegistry(registry_path=registry_path)
        registry1.add_agent(sample_entry)

        # 新しいレジストリインスタンスで読み込み
        registry2 = LocalRegistry(registry_path=registry_path)
        agents = registry2.list_agents()

        assert len(agents) == 1
        assert agents[0].id == "test-agent"
