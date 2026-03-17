"""AgentBlock 基底クラスのテスト."""

from pathlib import Path
from typing import Any

import pytest

from kernel.agents.agent_block import AgentBlock
from kernel.core.engine import AgentFlowEngine
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
from kernel.core.schemas import SchemaLoader
from kernel.protocols.a2a_card import AgentCard
from kernel.protocols.agui_emitter import AGUIEventEmitter


class TestAgentBlock:
    """AgentBlock のテスト."""

    @pytest.fixture
    def sample_metadata_file(self, tmp_path: Path) -> Path:
        """テスト用のメタデータファイルを作成."""
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
                a2a=A2AConfig(enabled=True, skills=["process"]),
                agui=AGUIConfig(enabled=True, events=[]),
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        metadata_file = tmp_path / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata, metadata_file)

        return metadata_file

    @pytest.fixture
    def concrete_agent_class(self) -> type[AgentBlock]:
        """テスト用の具象エージェントクラス."""

        class ConcreteAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                return {"result": "ok", "input": input_data}

        return ConcreteAgent

    def test_agent_block_is_abstract(self) -> None:
        """AgentBlock が抽象クラスであることをテスト."""
        with pytest.raises(TypeError):
            AgentBlock()  # type: ignore

    def test_concrete_agent_can_be_instantiated(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path
    ) -> None:
        """具象エージェントクラスがインスタンス化できることをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        assert isinstance(agent, AgentBlock)

    @pytest.mark.asyncio
    async def test_run_method_is_abstract(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path
    ) -> None:
        """run メソッドが抽象メソッドであることをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        result = await agent.run({"text": "hello"})
        assert result["result"] == "ok"

    def test_load_metadata(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """メタデータを読み込めることをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        metadata = agent.load_metadata()

        assert isinstance(metadata, AgentMetadata)
        assert metadata.meta.name == "Test Agent"

    def test_load_metadata_with_nonexistent_file(self, concrete_agent_class: type[AgentBlock], tmp_path: Path) -> None:
        """存在しないメタデータファイルを読み込もうとするとエラーになることをテスト."""
        nonexistent_file = tmp_path / "nonexistent.yaml"
        agent = concrete_agent_class(metadata_path=nonexistent_file)

        with pytest.raises(FileNotFoundError):
            agent.load_metadata()

    def test_metadata_property(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """metadata プロパティをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()

        assert agent.metadata is not None
        assert isinstance(agent.metadata, AgentMetadata)

    def test_engine_property(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """engine プロパティをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)

        assert isinstance(agent.engine, AgentFlowEngine)

    def test_custom_engine(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """カスタムエンジンを渡せることをテスト."""
        custom_engine = AgentFlowEngine()
        agent = concrete_agent_class(metadata_path=sample_metadata_file, engine=custom_engine)

        assert agent.engine is custom_engine

    @pytest.mark.asyncio
    async def test_initialize_and_cleanup(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path
    ) -> None:
        """initialize と cleanup メソッドをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)

        assert not agent.is_initialized

        await agent.initialize()
        assert agent.is_initialized

        await agent.cleanup()
        assert not agent.is_initialized

    @pytest.mark.asyncio
    async def test_context_manager(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """非同期コンテキストマネージャーをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)

        assert not agent.is_initialized

        async with agent:
            assert agent.is_initialized

        assert not agent.is_initialized

    def test_protocol_methods_are_injected(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path
    ) -> None:
        """プロトコルメソッドが注入されていることをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()

        # @auto_adapt デコレーターによって注入されたメソッドを確認
        assert hasattr(agent, "get_mcp_tools")
        assert hasattr(agent, "get_a2a_card")
        assert hasattr(agent, "create_agui_emitter")

    def test_get_mcp_tools(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """get_mcp_tools メソッドをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()

        tools = agent.get_mcp_tools()
        assert isinstance(tools, list)

    def test_get_a2a_card(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """get_a2a_card メソッドをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()

        card = agent.get_a2a_card()
        assert isinstance(card, AgentCard)
        assert card.name == "Test Agent"

    def test_create_agui_emitter(self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path) -> None:
        """create_agui_emitter メソッドをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()

        emitter = agent.create_agui_emitter(agent.engine)
        assert isinstance(emitter, AGUIEventEmitter)

    @pytest.mark.asyncio
    async def test_custom_initialize_and_cleanup(self, sample_metadata_file: Path) -> None:
        """カスタム initialize と cleanup をテスト."""
        initialized = False
        cleaned_up = False

        class CustomAgent(AgentBlock):
            async def initialize(self) -> None:
                nonlocal initialized
                await super().initialize()
                initialized = True

            async def cleanup(self) -> None:
                nonlocal cleaned_up
                await super().cleanup()
                cleaned_up = True

            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                return {"result": "ok"}

        agent = CustomAgent(metadata_path=sample_metadata_file)

        await agent.initialize()
        assert initialized

        await agent.cleanup()
        assert cleaned_up

    def test_load_metadata_with_different_path(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path, tmp_path: Path
    ) -> None:
        """異なるパスでメタデータを読み込めることをテスト."""
        # 別のメタデータファイルを作成
        metadata2 = AgentMetadata(
            meta=MetaInfo(
                id="test-agent-2",
                name="Test Agent 2",
                version="2.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A second test agent",
            ),
            interfaces=InterfaceDefinition(
                inputs=[InputField(name="data", type="string", required=True, description="Data")],
                outputs=[OutputField(name="output", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=None,
                a2a=None,
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        metadata_file2 = tmp_path / "agent2.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata2, metadata_file2)

        # 最初のメタデータで初期化
        agent = concrete_agent_class(metadata_path=sample_metadata_file)
        agent.load_metadata()
        assert agent.metadata.meta.name == "Test Agent"

        # 異なるパスでメタデータを再読み込み
        agent.load_metadata(metadata_path=metadata_file2)
        assert agent.metadata.meta.name == "Test Agent 2"

    def test_get_mcp_tools_with_mcp_protocol(self, concrete_agent_class: type[AgentBlock], tmp_path: Path) -> None:
        """MCP プロトコルが有効な場合の get_mcp_tools をテスト."""
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
                inputs=[InputField(name="text", type="string", required=True, description="Input")],
                outputs=[OutputField(name="result", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=["tool1"], resources=[]),
                a2a=None,
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        metadata_file = tmp_path / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata, metadata_file)

        agent = concrete_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # MCP ツールが生成されることを確認
        tools = agent.get_mcp_tools()
        assert isinstance(tools, list)

    def test_get_a2a_card_with_a2a_protocol(self, concrete_agent_class: type[AgentBlock], tmp_path: Path) -> None:
        """A2A プロトコルが有効な場合の get_a2a_card をテスト."""
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
                inputs=[InputField(name="text", type="string", required=True, description="Input")],
                outputs=[OutputField(name="result", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=None,
                a2a=A2AConfig(enabled=True, skills=["process"]),
                agui=None,
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#000000", size="medium", ports={}),
        )

        metadata_file = tmp_path / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata, metadata_file)

        agent = concrete_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # A2A カードが生成されることを確認
        card = agent.get_a2a_card()
        assert isinstance(card, AgentCard)
        assert card.name == "Test Agent"

    def test_protocol_methods_without_metadata(
        self, concrete_agent_class: type[AgentBlock], sample_metadata_file: Path
    ) -> None:
        """メタデータ読み込み前のプロトコルメソッドをテスト."""
        agent = concrete_agent_class(metadata_path=sample_metadata_file)

        # メタデータ読み込み前はデフォルト値を返す
        tools = agent.get_mcp_tools()
        assert tools == []

        card = agent.get_a2a_card()
        assert card is None

        # create_agui_emitter はメタデータが必要
        with pytest.raises(ValueError, match="Metadata not loaded"):
            agent.create_agui_emitter(agent.engine)
