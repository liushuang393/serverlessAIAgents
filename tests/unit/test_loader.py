"""AgentLoader のテスト."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from kernel.core.exceptions import AgentBlockValidationError
from kernel.agents.loader import AgentLoader
from kernel.core.metadata import (
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


class TestAgentLoader:
    """AgentLoader のテスト."""

    @pytest.fixture
    def loader(self) -> AgentLoader:
        """テスト用の AgentLoader を作成."""
        return AgentLoader()

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

    def test_loader_initialization(self, loader: AgentLoader) -> None:
        """AgentLoader の初期化をテスト."""
        assert loader._schema_loader is not None

    def test_parse_entry_point_valid(self, loader: AgentLoader) -> None:
        """有効なエントリーポイントのパースをテスト."""
        module, func = loader._parse_entry_point("flow.py:create_flow")

        assert module == "flow.py"
        assert func == "create_flow"

    def test_parse_entry_point_with_path(self, loader: AgentLoader) -> None:
        """パス付きエントリーポイントのパースをテスト."""
        module, func = loader._parse_entry_point("src/flow.py:create_flow")

        assert module == "src/flow.py"
        assert func == "create_flow"

    def test_parse_entry_point_invalid(self, loader: AgentLoader) -> None:
        """無効なエントリーポイントのパースをテスト."""
        with pytest.raises(AgentBlockValidationError) as exc_info:
            loader._parse_entry_point("invalid_format")

        assert "Invalid entry point format" in str(exc_info.value)

    def test_parse_entry_point_empty_function(self, loader: AgentLoader) -> None:
        """関数名が空のエントリーポイントをテスト."""
        module, func = loader._parse_entry_point("flow.py:")

        assert module == "flow.py"
        assert func == ""

    def test_load_metadata_only_success(
        self, loader: AgentLoader, sample_metadata: AgentMetadata, tmp_path: Path
    ) -> None:
        """メタデータのみの読み込み成功をテスト."""
        # Create agent directory with agent.yaml
        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        yaml_path = agent_path / "agent.yaml"
        yaml_content = """
meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test Author
  icon: 🤖
  category: test
  description: A test agent for unit testing

interfaces:
  inputs:
    - name: text
      type: string
      required: true
      description: Input text
  outputs:
    - name: result
      type: string

protocols:
  mcp:
    tools: []
    resources: []

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: "flow.py:create_flow"
  shared_schema: "schemas.py:MySchema"

visual:
  color: "#FF0000"
  size: medium
  ports: {}
"""
        yaml_path.write_text(yaml_content, encoding="utf-8")

        metadata = loader.load_metadata_only(agent_path)

        assert metadata.meta.id == "test-agent"
        assert metadata.meta.name == "Test Agent"
        assert metadata.meta.version == "1.0.0"

    def test_load_metadata_only_file_not_found(self, loader: AgentLoader, tmp_path: Path) -> None:
        """agent.yaml が存在しない場合のテスト."""
        agent_path = tmp_path / "nonexistent"

        with pytest.raises(FileNotFoundError) as exc_info:
            loader.load_metadata_only(agent_path)

        assert "agent.yaml not found" in str(exc_info.value)

    @patch("kernel.agents.loader.SchemaLoader")
    def test_load_from_directory_yaml_not_found(
        self, mock_schema_loader: MagicMock, loader: AgentLoader, tmp_path: Path
    ) -> None:
        """agent.yaml が存在しない場合の load_from_directory をテスト."""
        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        with pytest.raises(FileNotFoundError) as exc_info:
            loader.load_from_directory(agent_path)

        assert "agent.yaml not found" in str(exc_info.value)

    @patch("kernel.agents.loader.SchemaLoader")
    def test_load_from_directory_flow_module_not_found(
        self,
        mock_schema_loader_class: MagicMock,
        loader: AgentLoader,
        sample_metadata: AgentMetadata,
        tmp_path: Path,
    ) -> None:
        """Flow モジュールが存在しない場合のテスト."""
        # Mock schema loader
        mock_schema_loader = MagicMock()
        mock_schema_loader.load_from_file.return_value = sample_metadata
        mock_schema_loader_class.return_value = mock_schema_loader

        # Create new loader with mocked schema loader
        loader = AgentLoader()

        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        # Create agent.yaml
        yaml_path = agent_path / "agent.yaml"
        yaml_path.write_text("dummy", encoding="utf-8")

        with pytest.raises(FileNotFoundError) as exc_info:
            loader.load_from_directory(agent_path)

        assert "Flow module not found" in str(exc_info.value)

    def test_load_flow_creator_function_not_found(self, loader: AgentLoader, tmp_path: Path) -> None:
        """関数が存在しない場合のテスト."""
        # Create a Python module without the expected function
        module_path = tmp_path / "flow.py"
        module_path.write_text(
            """
def other_function():
    pass
""",
            encoding="utf-8",
        )

        with pytest.raises(AgentBlockValidationError) as exc_info:
            loader._load_flow_creator(module_path, "create_flow")

        assert "Function 'create_flow' not found" in str(exc_info.value)

    def test_load_flow_creator_success(self, loader: AgentLoader, tmp_path: Path) -> None:
        """Flow 作成関数の読み込み成功をテスト."""
        # Create a Python module with the expected function
        module_path = tmp_path / "flow.py"
        module_path.write_text(
            """
def create_flow():
    return "test_flow"
""",
            encoding="utf-8",
        )

        flow_creator = loader._load_flow_creator(module_path, "create_flow")

        assert callable(flow_creator)
        assert flow_creator() == "test_flow"

    def test_load_flow_creator_invalid_module(self, loader: AgentLoader, tmp_path: Path) -> None:
        """無効な Python モジュールの読み込みをテスト."""
        # Create an invalid Python module
        module_path = tmp_path / "invalid.py"
        module_path.write_text("invalid python syntax !!!", encoding="utf-8")

        with pytest.raises(AgentBlockValidationError) as exc_info:
            loader._load_flow_creator(module_path, "create_flow")

        assert "Failed to load flow creator" in str(exc_info.value)

    def test_load_from_directory_success(self, loader: AgentLoader, tmp_path: Path) -> None:
        """load_from_directory の成功ケースをテスト."""
        # Create agent directory
        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        # Create agent.yaml
        yaml_path = agent_path / "agent.yaml"
        yaml_content = """
meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test Author
  icon: 🤖
  category: test
  description: A test agent for unit testing

interfaces:
  inputs:
    - name: text
      type: string
      required: true
      description: Input text
  outputs:
    - name: result
      type: string

protocols:
  mcp:
    tools: []
    resources: []

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: "flow.py:create_flow"
  shared_schema: "schemas.py:MySchema"

visual:
  color: "#FF0000"
  size: medium
  ports: {}
"""
        yaml_path.write_text(yaml_content, encoding="utf-8")

        # Create flow.py
        flow_path = agent_path / "flow.py"
        flow_path.write_text(
            """
def create_flow():
    return lambda x: {"result": x}
""",
            encoding="utf-8",
        )

        # Load agent
        agent_block = loader.load_from_directory(agent_path)

        assert agent_block is not None
        assert agent_block.metadata.meta.id == "test-agent"
        assert hasattr(agent_block, "run")

    def test_create_agent_block(
        self,
        loader: AgentLoader,
        sample_metadata: AgentMetadata,
        tmp_path: Path,
    ) -> None:
        """AgentBlock の作成をテスト."""

        def mock_flow_creator():
            return MagicMock()

        agent_path = tmp_path / "test-agent"
        agent_path.mkdir()

        agent_block = loader._create_agent_block(
            metadata=sample_metadata,
            flow_creator=mock_flow_creator,
            agent_path=agent_path,
        )

        assert agent_block is not None
        assert agent_block.metadata == sample_metadata
        assert hasattr(agent_block, "run")
        assert callable(agent_block.run)
