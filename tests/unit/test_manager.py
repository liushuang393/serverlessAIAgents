"""AgentBlockManager のテスト."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from agentflow.core.agent_block import AgentBlock
from agentflow.core.exceptions import AgentBlockNotFoundError, AgentBlockValidationError
from agentflow.core.manager import AgentBlockManager, AgentInfo
from agentflow.core.metadata import (
    AgentMetadata,
    DependencySpec,
    InputField,
    InterfaceDefinition,
    MCPConfig,
    MetaInfo,
    OutputField,
    PocketFlowConfig,
    ProtocolConfig,
    VisualConfig,
)
from agentflow.core.validator import ValidationResult


class TestAgentInfo:
    """AgentInfo のテスト."""

    def test_agent_info_creation(self) -> None:
        """AgentInfo の作成をテスト."""
        info = AgentInfo(
            agent_id="test-agent",
            name="Test Agent",
            version="1.0.0",
            description="A test agent",
            category="test",
            author="Test Author",
        )

        assert info.agent_id == "test-agent"
        assert info.name == "Test Agent"
        assert info.version == "1.0.0"
        assert info.description == "A test agent"
        assert info.category == "test"
        assert info.author == "Test Author"

    def test_agent_info_repr(self) -> None:
        """AgentInfo の文字列表現をテスト."""
        info = AgentInfo(
            agent_id="test-agent",
            name="Test Agent",
            version="1.0.0",
            description="A test agent",
            category="test",
            author="Test Author",
        )

        repr_str = repr(info)
        assert "test-agent" in repr_str
        assert "Test Agent" in repr_str
        assert "1.0.0" in repr_str


class TestAgentBlockManager:
    """AgentBlockManager のテスト."""

    @pytest.fixture
    def manager(self, tmp_path: Path) -> AgentBlockManager:
        """テスト用の AgentBlockManager を作成."""
        return AgentBlockManager(registry_path=tmp_path / "registry")

    @pytest.fixture
    def sample_metadata(self) -> AgentMetadata:
        """サンプルメタデータを作成."""
        return AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent for unit testing",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="text",
                        type="string",
                        required=True,
                        description="Input text",
                    )
                ],
                outputs=[
                    OutputField(
                        name="result",
                        type="string",
                    )
                ],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=[], resources=[]),
                a2a=None,
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:MySchema",
            ),
            visual=VisualConfig(color="#FF0000", size="medium", ports={}),
        )

    @pytest.fixture
    def mock_agent_block(self, sample_metadata: AgentMetadata) -> AgentBlock:
        """モック AgentBlock を作成."""
        agent = MagicMock(spec=AgentBlock)
        agent.metadata = sample_metadata
        return agent

    def test_manager_initialization(self, tmp_path: Path) -> None:
        """AgentBlockManager の初期化をテスト."""
        manager = AgentBlockManager(registry_path=tmp_path / "registry")

        assert manager.registry == {}
        assert manager.registry_path == tmp_path / "registry"
        assert manager.registry_path.exists()

    def test_manager_default_registry_path(self) -> None:
        """デフォルトのレジストリパスをテスト."""
        manager = AgentBlockManager()

        expected_path = Path.home() / ".agentflow" / "registry"
        assert manager.registry_path == expected_path

    def test_register_agent(self, manager: AgentBlockManager, mock_agent_block: AgentBlock) -> None:
        """Agent の登録をテスト."""
        manager.register_agent("test-agent", mock_agent_block)

        assert "test-agent" in manager.registry
        assert manager.registry["test-agent"] == mock_agent_block

    def test_get_agent(self, manager: AgentBlockManager, mock_agent_block: AgentBlock) -> None:
        """Agent の取得をテスト."""
        manager.register_agent("test-agent", mock_agent_block)

        retrieved = manager.get_agent("test-agent")
        assert retrieved == mock_agent_block

    def test_get_nonexistent_agent(self, manager: AgentBlockManager) -> None:
        """存在しない Agent の取得をテスト."""
        with pytest.raises(AgentBlockNotFoundError) as exc_info:
            manager.get_agent("nonexistent")

        assert "nonexistent" in str(exc_info.value)

    def test_unregister_agent(
        self, manager: AgentBlockManager, mock_agent_block: AgentBlock
    ) -> None:
        """Agent の登録解除をテスト."""
        manager.register_agent("test-agent", mock_agent_block)
        manager.unregister_agent("test-agent")

        assert "test-agent" not in manager.registry

    def test_unregister_nonexistent_agent(self, manager: AgentBlockManager) -> None:
        """存在しない Agent の登録解除をテスト."""
        with pytest.raises(AgentBlockNotFoundError) as exc_info:
            manager.unregister_agent("nonexistent")

        assert "nonexistent" in str(exc_info.value)

    def test_list_agents_empty(self, manager: AgentBlockManager) -> None:
        """空のレジストリで Agent 一覧を取得."""
        agents = manager.list_agents()
        assert agents == []

    def test_list_agents(self, manager: AgentBlockManager, mock_agent_block: AgentBlock) -> None:
        """Agent 一覧を取得."""
        manager.register_agent("test-agent", mock_agent_block)

        agents = manager.list_agents()
        assert len(agents) == 1
        assert agents[0].agent_id == "test-agent"
        assert agents[0].name == "Test Agent"
        assert agents[0].version == "1.0.0"

    def test_list_agents_with_category_filter(
        self, manager: AgentBlockManager, mock_agent_block: AgentBlock
    ) -> None:
        """カテゴリフィルターで Agent 一覧を取得."""
        manager.register_agent("test-agent", mock_agent_block)

        # マッチするフィルター
        agents = manager.list_agents(filters={"category": "test"})
        assert len(agents) == 1

        # マッチしないフィルター
        agents = manager.list_agents(filters={"category": "other"})
        assert len(agents) == 0

    def test_list_agents_with_author_filter(
        self, manager: AgentBlockManager, mock_agent_block: AgentBlock
    ) -> None:
        """作者フィルターで Agent 一覧を取得."""
        manager.register_agent("test-agent", mock_agent_block)

        # マッチするフィルター
        agents = manager.list_agents(filters={"author": "Test Author"})
        assert len(agents) == 1

        # マッチしないフィルター
        agents = manager.list_agents(filters={"author": "Other Author"})
        assert len(agents) == 0

    def test_validate_agent(
        self, manager: AgentBlockManager, sample_metadata: AgentMetadata
    ) -> None:
        """Agent メタデータの検証をテスト."""
        result = manager.validate_agent(sample_metadata)

        assert isinstance(result, ValidationResult)
        assert result.is_valid

    @patch("agentflow.core.manager.AgentLoader")
    def test_load_agent_success(
        self,
        mock_loader_class: MagicMock,
        manager: AgentBlockManager,
        mock_agent_block: AgentBlock,
        tmp_path: Path,
    ) -> None:
        """Agent の読み込み成功をテスト."""
        # Mock loader
        mock_loader = MagicMock()
        mock_loader.load_from_directory.return_value = mock_agent_block
        mock_loader_class.return_value = mock_loader

        # Create new manager to use mocked loader
        manager = AgentBlockManager(registry_path=tmp_path / "registry")

        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        loaded_agent = manager.load_agent(agent_path)

        assert loaded_agent == mock_agent_block
        mock_loader.load_from_directory.assert_called_once_with(agent_path)

    @patch("agentflow.core.manager.AgentLoader")
    @patch("agentflow.core.manager.AgentValidator")
    def test_load_agent_validation_failure(
        self,
        mock_validator_class: MagicMock,
        mock_loader_class: MagicMock,
        manager: AgentBlockManager,
        mock_agent_block: AgentBlock,
        tmp_path: Path,
    ) -> None:
        """Agent の読み込み時の検証失敗をテスト."""
        # Mock loader
        mock_loader = MagicMock()
        mock_loader.load_from_directory.return_value = mock_agent_block
        mock_loader_class.return_value = mock_loader

        # Mock validator to return invalid result
        mock_validator = MagicMock()
        invalid_result = ValidationResult(is_valid=False, errors=["Test error"])
        mock_validator.validate.return_value = invalid_result
        mock_validator_class.return_value = mock_validator

        # Create new manager to use mocked components
        manager = AgentBlockManager(registry_path=tmp_path / "registry")

        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        with pytest.raises(AgentBlockValidationError) as exc_info:
            manager.load_agent(agent_path)

        assert "Test error" in str(exc_info.value)

    def test_resolve_dependencies_no_deps(
        self, manager: AgentBlockManager, mock_agent_block: AgentBlock
    ) -> None:
        """依存関係なしの Agent の依存解決をテスト."""
        manager.register_agent("test-agent", mock_agent_block)

        deps = manager.resolve_dependencies("test-agent")

        assert deps == ["test-agent"]

    def test_resolve_dependencies_with_deps(
        self, manager: AgentBlockManager, sample_metadata: AgentMetadata
    ) -> None:
        """依存関係ありの Agent の依存解決をテスト."""
        # Create agent with dependencies
        metadata_with_deps = AgentMetadata(
            meta=sample_metadata.meta,
            interfaces=sample_metadata.interfaces,
            protocols=sample_metadata.protocols,
            dependencies=DependencySpec(agents=["dep-agent"], tools=[], packages=[]),
            pocketflow=sample_metadata.pocketflow,
            visual=sample_metadata.visual,
        )

        agent_with_deps = MagicMock(spec=AgentBlock)
        agent_with_deps.metadata = metadata_with_deps

        dep_agent = MagicMock(spec=AgentBlock)
        dep_agent.metadata = sample_metadata

        manager.register_agent("dep-agent", dep_agent)
        manager.register_agent("test-agent", agent_with_deps)

        deps = manager.resolve_dependencies("test-agent")

        assert "dep-agent" in deps
        assert "test-agent" in deps
        assert deps.index("dep-agent") < deps.index("test-agent")

    def test_resolve_dependencies_nonexistent_agent(self, manager: AgentBlockManager) -> None:
        """存在しない Agent の依存解決をテスト."""
        with pytest.raises(AgentBlockNotFoundError) as exc_info:
            manager.resolve_dependencies("nonexistent")

        assert "nonexistent" in str(exc_info.value)
