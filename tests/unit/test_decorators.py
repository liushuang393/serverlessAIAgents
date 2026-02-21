"""デコレーターのテスト."""

from pathlib import Path

import pytest

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.metadata import (
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
from agentflow.core.schemas import SchemaLoader
from agentflow.decorators import auto_adapt
from agentflow.protocols.a2a_card import AgentCard
from agentflow.protocols.agui_emitter import AGUIEventEmitter


class TestAutoAdaptDecorator:
    """@auto_adapt デコレーターのテスト."""

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

    def test_decorator_with_mcp_protocol(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """MCP プロトコルを有効にしたデコレーターをテスト."""

        @auto_adapt(protocols=["mcp"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # MCP ツールが生成されていることを確認
        assert hasattr(agent, "get_mcp_tools")
        tools = agent.get_mcp_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0

    def test_decorator_with_a2a_protocol(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """A2A プロトコルを有効にしたデコレーターをテスト."""

        @auto_adapt(protocols=["a2a"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # A2A カードが生成されていることを確認
        assert hasattr(agent, "get_a2a_card")
        card = agent.get_a2a_card()
        assert isinstance(card, AgentCard)
        assert card.name == "Test Agent"

    def test_decorator_with_agui_protocol(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """AG-UI プロトコルを有効にしたデコレーターをテスト."""

        @auto_adapt(protocols=["agui"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # AG-UI エミッター作成メソッドが追加されていることを確認
        assert hasattr(agent, "create_agui_emitter")

        engine = AgentFlowEngine()
        emitter = agent.create_agui_emitter(engine)
        assert isinstance(emitter, AGUIEventEmitter)

    def test_decorator_with_all_protocols(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """全プロトコルを有効にしたデコレーターをテスト."""

        @auto_adapt(protocols=["mcp", "a2a", "agui"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # 全プロトコルメソッドが追加されていることを確認
        assert hasattr(agent, "get_mcp_tools")
        assert hasattr(agent, "get_a2a_card")
        assert hasattr(agent, "create_agui_emitter")

        tools = agent.get_mcp_tools()
        assert len(tools) > 0

        card = agent.get_a2a_card()
        assert card is not None

    def test_decorator_without_protocols(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """プロトコル指定なしのデコレーターをテスト (自動判定)."""

        @auto_adapt(metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # メタデータから自動判定されたプロトコルメソッドが追加されていることを確認
        assert hasattr(agent, "get_mcp_tools")
        assert hasattr(agent, "get_a2a_card")
        assert hasattr(agent, "create_agui_emitter")

    def test_decorator_preserves_original_init(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """デコレーターが元の __init__ を保持することをテスト."""

        @auto_adapt(protocols=["mcp"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self, value: int) -> None:
                self.value = value

        agent = TestAgent(42)

        # 元の __init__ が実行されていることを確認
        assert agent.value == 42

        # プロトコルメソッドも追加されていることを確認
        assert hasattr(agent, "get_mcp_tools")

    def test_decorator_with_inheritance(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """デコレーターが継承と互換性があることをテスト."""

        class BaseAgent:
            def __init__(self) -> None:
                self.base_value = "base"

        @auto_adapt(protocols=["mcp"], metadata_path=sample_metadata_file)
        class DerivedAgent(BaseAgent):
            def __init__(self) -> None:
                super().__init__()
                self.derived_value = "derived"

        agent = DerivedAgent()

        # 継承が正しく動作していることを確認
        assert agent.base_value == "base"
        assert agent.derived_value == "derived"

        # プロトコルメソッドも追加されていることを確認
        assert hasattr(agent, "get_mcp_tools")

    def test_decorator_with_nonexistent_metadata(self, tmp_path: Path) -> None:
        """メタデータファイルが存在しない場合のデコレーターをテスト."""
        nonexistent_file = tmp_path / "nonexistent.yaml"

        @auto_adapt(protocols=["mcp"], metadata_path=nonexistent_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # メタデータが None であることを確認
        assert agent.get_metadata() is None

        # プロトコルメソッドは追加されているが、空のリストを返す
        assert hasattr(agent, "get_mcp_tools")
        tools = agent.get_mcp_tools()
        assert tools == []

    def test_get_metadata_method(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """get_metadata メソッドをテスト."""

        @auto_adapt(protocols=["mcp"], metadata_path=sample_metadata_file)
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # メタデータが取得できることを確認
        assert hasattr(agent, "get_metadata")
        metadata = agent.get_metadata()
        assert isinstance(metadata, AgentMetadata)
        assert metadata.meta.name == "Test Agent"

    def test_decorator_can_be_stacked(self, sample_metadata_file: Path, tmp_path: Path) -> None:
        """デコレーターが他のデコレーターとスタックできることをテスト."""

        def another_decorator(cls: type) -> type:
            """別のデコレーター."""
            original_init = cls.__init__

            def new_init(self: object, *args: object, **kwargs: object) -> None:
                original_init(self, *args, **kwargs)
                self.decorated = True

            cls.__init__ = new_init  # type: ignore
            return cls

        @auto_adapt(protocols=["mcp"], metadata_path=sample_metadata_file)
        @another_decorator
        class TestAgent:
            def __init__(self) -> None:
                pass

        agent = TestAgent()

        # 両方のデコレーターが適用されていることを確認
        assert hasattr(agent, "get_mcp_tools")
        assert hasattr(agent, "decorated")
        assert agent.decorated is True
