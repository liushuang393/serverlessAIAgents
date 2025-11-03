"""プロトコル統合テスト.

このテストは3つのプロトコル (MCP/A2A/AG-UI) の統合をテストします。
"""

from pathlib import Path
from typing import TYPE_CHECKING, Any

import pytest

from agentflow.core.agent_block import AgentBlock
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
from agentflow.protocols.a2a_server import A2AServer


if TYPE_CHECKING:
    from agentflow.protocols.agui_events import AGUIEvent


class TestProtocolIntegration:
    """プロトコル統合テスト."""

    @pytest.fixture
    def sample_metadata(self, tmp_path: Path) -> tuple[AgentMetadata, Path]:
        """テスト用のメタデータとファイルパスを作成."""
        metadata = AgentMetadata(
            meta=MetaInfo(
                id="protocol-test-agent",
                name="Protocol Test Agent",
                version="1.0.0",
                author="Test",
                icon="🔌",
                category="test",
                description="Agent for protocol integration testing",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="input",
                        type="string",
                        required=True,
                        description="Input data",
                    )
                ],
                outputs=[OutputField(name="output", type="string")],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=[], resources=[]),
                a2a=A2AConfig(enabled=True, skills=["process", "transform"]),
                agui=AGUIConfig(
                    enabled=True,
                    events=["flow.start", "flow.complete", "node.start"],
                ),
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:flow",
                shared_schema="schemas.py:Schema",
            ),
            visual=VisualConfig(color="#FF5733", size="medium", ports={}),
        )

        metadata_file = tmp_path / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(metadata, metadata_file)

        return metadata, metadata_file

    @pytest.fixture
    def test_agent_class(self) -> type[AgentBlock]:
        """テスト用のエージェントクラス."""

        class TestAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                input_text = input_data.get("input", "")
                return {"output": f"processed: {input_text}"}

        return TestAgent

    def test_mcp_protocol_integration(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """MCP プロトコル統合をテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # MCP ツールを取得
        tools = agent.get_mcp_tools()

        assert isinstance(tools, list)
        assert len(tools) == 2  # process, transform

        # ツールの構造を検証
        tool = tools[0]
        assert "name" in tool
        assert "description" in tool
        assert "inputSchema" in tool

        # 入力スキーマを検証
        input_schema = tool["inputSchema"]
        assert input_schema["type"] == "object"
        assert "properties" in input_schema
        assert "input" in input_schema["properties"]

    def test_a2a_protocol_integration(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """A2A プロトコル統合をテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # A2A カードを取得
        card = agent.get_a2a_card()

        assert card is not None
        assert card.name == "Protocol Test Agent"
        assert card.version == "1.0.0"
        assert len(card.skills) == 2  # process, transform

        # スキルの構造を検証
        skill = card.skills[0]
        assert skill.name in ["process", "transform"]
        assert skill.description != ""
        assert "input" in skill.input_schema["properties"]
        assert "output" in skill.output_schema["properties"]

    @pytest.mark.asyncio
    async def test_a2a_server_integration(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """A2A サーバー統合をテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # A2A サーバーを作成
        server = A2AServer()

        # エージェントを登録
        card = agent.get_a2a_card()
        assert card is not None

        async def handler(input_data: dict[str, Any]) -> dict[str, Any]:
            return await agent.run(input_data)

        # スキルごとにハンドラーを登録
        handlers = {skill.name: handler for skill in card.skills}
        server.register_agent(card, handlers)

        # エージェントカードを取得 (agent.name をキーとして使用)
        retrieved_card = server.get_agent_card("Protocol Test Agent")
        assert retrieved_card is not None
        assert retrieved_card.name == "Protocol Test Agent"

        # タスクを実行 (agent.name をキーとして使用)
        response = await server.handle_task("Protocol Test Agent", "process", {"input": "test"})
        assert response["status"] == "success"
        assert response["result"]["output"] == "processed: test"

    @pytest.mark.asyncio
    async def test_agui_protocol_integration(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """AG-UI プロトコル統合をテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # AG-UI エミッターを作成
        emitter = agent.create_agui_emitter(agent.engine)

        assert emitter is not None

        # フローにアタッチ
        await emitter.attach_to_flow("test-flow")

        # イベントを発行
        await emitter.emit_log("info", "Test message", "test")

        # イベントストリームを取得
        events: list[AGUIEvent] = []
        async for event in emitter.stream_events():
            events.append(event)
            if len(events) >= 1:
                break

        assert len(events) > 0
        assert events[0].event_type.value == "log"

        # デタッチ
        await emitter.detach_from_flow()

    @pytest.mark.asyncio
    async def test_all_protocols_together(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """全プロトコルを同時に使用できることをテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # MCP ツールを取得
        mcp_tools = agent.get_mcp_tools()
        assert len(mcp_tools) > 0

        # A2A カードを取得
        a2a_card = agent.get_a2a_card()
        assert a2a_card is not None

        # AG-UI エミッターを作成
        agui_emitter = agent.create_agui_emitter(agent.engine)
        assert agui_emitter is not None

        # エージェントを実行
        result = await agent.run({"input": "test"})
        assert result["output"] == "processed: test"

    @pytest.mark.asyncio
    async def test_protocol_error_handling(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
    ) -> None:
        """プロトコルのエラーハンドリングをテスト."""

        class ErrorAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                msg = "Test error"
                raise ValueError(msg)

        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = ErrorAgent(metadata_path=metadata_file)
        agent.load_metadata()

        # A2A サーバーでエラーハンドリング
        server = A2AServer()
        card = agent.get_a2a_card()
        assert card is not None

        async def handler(input_data: dict[str, Any]) -> dict[str, Any]:
            return await agent.run(input_data)

        # スキルごとにハンドラーを登録
        handlers = {skill.name: handler for skill in card.skills}
        server.register_agent(card, handlers)

        # エラーが発生することを確認 (agent.name をキーとして使用)
        # A2AServer はエラーを例外として再スローせず、エラーレスポンスを返す
        response = await server.handle_task("Protocol Test Agent", "process", {"input": "test"})
        assert response["status"] == "error"
        assert response["error"] == "Task execution failed"

    def test_protocol_metadata_validation(
        self,
        sample_metadata: tuple[AgentMetadata, Path],
        test_agent_class: type[AgentBlock],
    ) -> None:
        """プロトコルメタデータの検証をテスト."""
        _metadata, metadata_file = sample_metadata

        # エージェントを作成
        agent = test_agent_class(metadata_path=metadata_file)
        agent.load_metadata()

        # メタデータを検証
        assert agent.metadata is not None
        assert agent.metadata.protocols.mcp is not None
        assert agent.metadata.protocols.a2a is not None
        assert agent.metadata.protocols.a2a.enabled is True
        assert agent.metadata.protocols.agui is not None
        assert agent.metadata.protocols.agui.enabled is True

        # プロトコル設定を検証
        assert len(agent.metadata.protocols.a2a.skills) == 2
        assert "process" in agent.metadata.protocols.a2a.skills
        assert "transform" in agent.metadata.protocols.a2a.skills

        assert len(agent.metadata.protocols.agui.events) == 3
        assert "flow.start" in agent.metadata.protocols.agui.events
