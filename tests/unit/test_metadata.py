"""エージェントメタデータスキーマのテスト."""

from pathlib import Path

import pytest
from pydantic import ValidationError

from kernel.core.metadata import (
    A2AConfig,
    AgentMetadata,
    AGUIConfig,
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
from kernel.core.schemas import SchemaLoader, SchemaValidationError


class TestMetaInfo:
    """MetaInfo モデルのテスト."""

    def test_valid_meta_info(self) -> None:
        """正常な MetaInfo を作成できることをテスト."""
        meta = MetaInfo(
            id="test-agent",
            name="Test Agent",
            version="1.0.0",
            author="Test Author",
            icon="🤖",
            category="test",
            description="A test agent",
        )
        assert meta.id == "test-agent"
        assert meta.version == "1.0.0"

    def test_invalid_id_format(self) -> None:
        """無効な ID 形式でエラーになることをテスト."""
        with pytest.raises(ValidationError):
            MetaInfo(
                id="Test_Agent",  # アンダースコアは不可
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent",
            )

    def test_invalid_version_format(self) -> None:
        """無効なバージョン形式でエラーになることをテスト."""
        with pytest.raises(ValidationError):
            MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0",  # セマンティックバージョンではない
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent",
            )


class TestInputField:
    """InputField モデルのテスト."""

    def test_required_field(self) -> None:
        """必須フィールドを作成できることをテスト."""
        field = InputField(
            name="text",
            type="string",
            required=True,
            description="Input text",
        )
        assert field.required is True
        assert field.default is None

    def test_optional_field_with_default(self) -> None:
        """デフォルト値付きオプションフィールドを作成できることをテスト."""
        field = InputField(
            name="count",
            type="number",
            required=False,
            description="Count",
            default=10,
        )
        assert field.required is False
        assert field.default == 10

    def test_enum_field_with_options(self) -> None:
        """選択肢付き enum フィールドを作成できることをテスト."""
        field = InputField(
            name="format",
            type="enum",
            required=False,
            description="Format",
            options=["json", "yaml", "xml"],
            default="json",
        )
        assert field.options == ["json", "yaml", "xml"]

    def test_file_field_with_accept(self) -> None:
        """許可拡張子付きファイルフィールドを作成できることをテスト."""
        field = InputField(
            name="file",
            type="file",
            required=True,
            description="Input file",
            accept=[".txt", ".md"],
        )
        assert field.accept == [".txt", ".md"]


class TestProtocolConfig:
    """ProtocolConfig モデルのテスト."""

    def test_all_protocols_enabled(self) -> None:
        """全プロトコルを有効にできることをテスト."""
        config = ProtocolConfig(
            mcp=MCPConfig(tools=["mcp://server/tool"], resources=[]),
            a2a=A2AConfig(enabled=True, skills=["skill1"]),
            agui=AGUIConfig(enabled=True, events=["flow.start"]),
        )
        assert config.mcp is not None
        assert config.a2a is not None
        assert config.agui is not None

    def test_partial_protocols(self) -> None:
        """一部のプロトコルのみ有効にできることをテスト."""
        config = ProtocolConfig(
            mcp=MCPConfig(tools=[], resources=[]),
            a2a=None,
            agui=None,
        )
        assert config.mcp is not None
        assert config.a2a is None
        assert config.agui is None


class TestAgentMetadata:
    """AgentMetadata モデルのテスト."""

    def test_complete_metadata(self) -> None:
        """完全なメタデータを作成できることをテスト."""
        metadata = AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent",
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
                outputs=[OutputField(name="result", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=[], resources=[]),
                a2a=A2AConfig(enabled=True, skills=[]),
                agui=AGUIConfig(enabled=True, events=[]),
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#FF0000", size="medium", ports={}),
        )
        assert metadata.meta.id == "test-agent"
        assert len(metadata.interfaces.inputs) == 1
        assert metadata.pocketflow.entry == "flow.py:create_flow"


class TestSchemaLoader:
    """SchemaLoader のテスト."""

    def test_load_from_file(self, tmp_path: Path) -> None:
        """ファイルからメタデータを読み込めることをテスト."""
        yaml_content = """
meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test Author
  icon: 🤖
  category: test
  description: A test agent

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
  entry: flow.py:create_flow
  shared_schema: schemas.py:Schema

visual:
  color: "#FF0000"
  size: medium
  ports: {}
"""
        yaml_file = tmp_path / "agent.yaml"
        yaml_file.write_text(yaml_content, encoding="utf-8")

        loader = SchemaLoader()
        metadata = loader.load_from_file(yaml_file)

        assert metadata.meta.id == "test-agent"
        assert metadata.meta.name == "Test Agent"

    def test_load_nonexistent_file(self) -> None:
        """存在しないファイルでエラーになることをテスト."""
        loader = SchemaLoader()
        with pytest.raises(FileNotFoundError):
            loader.load_from_file(Path("/nonexistent/agent.yaml"))

    def test_validate_invalid_data(self) -> None:
        """無効なデータで検証エラーになることをテスト."""
        loader = SchemaLoader()
        invalid_data = {"meta": {"id": "test"}}  # 必須フィールドが不足

        with pytest.raises(SchemaValidationError) as exc_info:
            loader.validate(invalid_data)

        assert len(exc_info.value.errors) > 0

    def test_save_and_load_roundtrip(self, tmp_path: Path) -> None:
        """保存と読み込みのラウンドトリップをテスト."""
        metadata = AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent",
            ),
            interfaces=InterfaceDefinition(inputs=[], outputs=[]),
            protocols=ProtocolConfig(),
            dependencies=DependencySpec(),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#FF0000", size="medium", ports={}),
        )

        yaml_file = tmp_path / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata, yaml_file)

        loaded_metadata = loader.load_from_file(yaml_file)
        assert loaded_metadata.meta.id == metadata.meta.id
        assert loaded_metadata.meta.version == metadata.meta.version
