"""プロトコルアダプターのテスト."""

import pytest

from agentflow.adapters.protocol_adapter import ProtocolAdapter
from agentflow.core.engine import AgentFlowEngine
from agentflow.core.metadata import (
    A2AConfig,
    AGUIConfig,
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
from agentflow.protocols.a2a_card import AgentCard
from agentflow.protocols.agui_emitter import AGUIEventEmitter


class TestProtocolAdapter:
    """ProtocolAdapter のテスト."""

    @pytest.fixture
    def sample_metadata(self) -> AgentMetadata:
        """テスト用のサンプルメタデータ."""
        return AgentMetadata(
            meta=MetaInfo(
                id="text-processor",
                name="Text Processor",
                version="1.0.0",
                author="Test Author",
                icon="📝",
                category="text",
                description="A text processing agent",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="text",
                        type="string",
                        required=True,
                        description="Input text to process",
                    ),
                    InputField(
                        name="max_length",
                        type="number",
                        required=False,
                        description="Maximum length",
                        default=100,
                    ),
                ],
                outputs=[
                    OutputField(
                        name="result",
                        type="string",
                    ),
                    OutputField(
                        name="metadata",
                        type="object",
                        schema={"properties": {"length": {"type": "number"}}},
                    ),
                ],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=[], resources=[]),
                a2a=A2AConfig(
                    enabled=True,
                    skills=["process_text", "format_text"],
                ),
                agui=AGUIConfig(
                    enabled=True,
                    events=["flow.start", "flow.complete"],
                ),
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#4A90E2", size="medium", ports={}),
        )

    def test_generate_mcp_tools(self, sample_metadata: AgentMetadata) -> None:
        """MCP ツール定義を生成できることをテスト."""
        tools = ProtocolAdapter.generate_mcp_tools(sample_metadata)

        assert len(tools) == 2
        assert tools[0]["name"] == "process_text"
        assert tools[1]["name"] == "format_text"

        # 入力スキーマを確認
        input_schema = tools[0]["inputSchema"]
        assert input_schema["type"] == "object"
        assert "text" in input_schema["properties"]
        assert "max_length" in input_schema["properties"]
        assert "text" in input_schema["required"]
        assert "max_length" not in input_schema["required"]

        # フィールドプロパティを確認
        assert input_schema["properties"]["text"]["type"] == "string"
        assert input_schema["properties"]["max_length"]["type"] == "number"
        assert input_schema["properties"]["max_length"]["default"] == 100

    def test_generate_mcp_tools_without_skills(self) -> None:
        """スキルがない場合のデフォルトツール生成をテスト."""
        metadata = AgentMetadata(
            meta=MetaInfo(
                id="simple-agent",
                name="Simple Agent",
                version="1.0.0",
                author="Test",
                icon="🤖",
                category="test",
                description="A simple agent",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="input",
                        type="string",
                        required=True,
                        description="Input",
                    )
                ],
                outputs=[OutputField(name="output", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(),
                a2a=A2AConfig(enabled=False, skills=[]),
                agui=AGUIConfig(enabled=False),
            ),
            dependencies=DependencySpec(),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        tools = ProtocolAdapter.generate_mcp_tools(metadata)

        assert len(tools) == 1
        assert tools[0]["name"] == "simple_agent"
        assert tools[0]["description"] == "A simple agent"

    def test_generate_a2a_card(self, sample_metadata: AgentMetadata) -> None:
        """A2A AgentCard を生成できることをテスト."""
        card = ProtocolAdapter.generate_a2a_card(sample_metadata)

        assert isinstance(card, AgentCard)
        assert card.name == "Text Processor"
        assert card.description == "A text processing agent"
        assert card.version == "1.0.0"
        assert card.author == "Test Author"

        # スキルを確認
        assert len(card.skills) == 2
        assert card.skills[0].name == "process_text"
        assert card.skills[1].name == "format_text"

        # 入力スキーマを確認
        input_schema = card.skills[0].input_schema
        assert input_schema["type"] == "object"
        assert "text" in input_schema["properties"]
        assert "max_length" in input_schema["properties"]

        # 出力スキーマを確認
        output_schema = card.skills[0].output_schema
        assert output_schema["type"] == "object"
        assert "result" in output_schema["properties"]
        assert "metadata" in output_schema["properties"]

        # メタデータを確認
        assert card.metadata["id"] == "text-processor"
        assert card.metadata["icon"] == "📝"
        assert card.metadata["category"] == "text"

    def test_generate_a2a_card_without_skills(self) -> None:
        """スキルがない場合のデフォルトカード生成をテスト."""
        metadata = AgentMetadata(
            meta=MetaInfo(
                id="simple-agent",
                name="Simple Agent",
                version="1.0.0",
                author="Test",
                icon="🤖",
                category="test",
                description="A simple agent",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="input",
                        type="string",
                        required=True,
                        description="Input",
                    )
                ],
                outputs=[OutputField(name="output", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(),
                a2a=A2AConfig(enabled=False, skills=[]),
                agui=AGUIConfig(enabled=False),
            ),
            dependencies=DependencySpec(),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        card = ProtocolAdapter.generate_a2a_card(metadata)

        assert len(card.skills) == 1
        assert card.skills[0].name == "simple_agent"
        assert card.skills[0].description == "A simple agent"

    def test_wrap_flow_with_agui(self, sample_metadata: AgentMetadata) -> None:
        """AG-UI イベントエミッターを作成できることをテスト."""
        engine = AgentFlowEngine()
        flow_id = "test-flow-123"

        emitter = ProtocolAdapter.wrap_flow_with_agui(engine, flow_id, sample_metadata)

        assert isinstance(emitter, AGUIEventEmitter)
        assert emitter._engine is engine

    def test_field_to_json_schema_with_enum(self) -> None:
        """enum フィールドの JSON Schema 変換をテスト."""
        field = InputField(
            name="format",
            type="string",
            required=True,
            description="Output format",
            options=["json", "xml", "yaml"],
        )

        schema = ProtocolAdapter._field_to_json_schema(field)

        assert schema["type"] == "string"
        assert schema["description"] == "Output format"
        assert schema["enum"] == ["json", "xml", "yaml"]

    def test_field_to_json_schema_with_file_accept(self) -> None:
        """file フィールドの accept プロパティをテスト."""
        field = InputField(
            name="document",
            type="file",
            required=True,
            description="Document file",
            accept=[".pdf", ".docx"],
        )

        schema = ProtocolAdapter._field_to_json_schema(field)

        assert schema["type"] == "file"
        assert schema["accept"] == [".pdf", ".docx"]

    def test_field_to_json_schema_output_with_schema(self) -> None:
        """OutputField の schema プロパティをテスト."""
        field = OutputField(
            name="metadata",
            type="object",
            schema={
                "properties": {
                    "length": {"type": "number"},
                    "words": {"type": "number"},
                }
            },
        )

        schema = ProtocolAdapter._field_to_json_schema(field)

        assert schema["type"] == "object"
        assert "properties" in schema
        assert "length" in schema["properties"]
        assert "words" in schema["properties"]

