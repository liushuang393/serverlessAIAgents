"""フルワークフロー統合テスト.

このテストは BizCore フレームワークの全体的なワークフローをテストします:
1. プロジェクト初期化 (bizcore init)
2. エージェント作成 (bizcore create agent)
3. エージェント実行 (bizcore run)
"""

import json
from pathlib import Path
from typing import Any

import pytest

from kernel.agents.agent_block import AgentBlock
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


class TestFullWorkflow:
    """フルワークフロー統合テスト."""

    @pytest.fixture
    def test_project_dir(self, tmp_path: Path) -> Path:
        """テスト用のプロジェクトディレクトリを作成."""
        project_dir = tmp_path / "test-project"
        project_dir.mkdir()
        return project_dir

    @pytest.fixture
    def sample_agent_metadata(self) -> AgentMetadata:
        """テスト用のエージェントメタデータ."""
        return AgentMetadata(
            meta=MetaInfo(
                id="test-agent",
                name="Test Agent",
                version="1.0.0",
                author="Test Author",
                icon="🤖",
                category="test",
                description="A test agent for integration testing",
            ),
            interfaces=InterfaceDefinition(
                inputs=[
                    InputField(
                        name="text",
                        type="string",
                        required=True,
                        description="Input text",
                    ),
                    InputField(
                        name="operation",
                        type="string",
                        required=False,
                        description="Operation to perform",
                        options=["upper", "lower", "reverse"],
                        default="upper",
                    ),
                ],
                outputs=[
                    OutputField(name="result", type="string"),
                    OutputField(name="original", type="string"),
                ],
            ),
            protocols=ProtocolConfig(
                mcp=MCPConfig(tools=[], resources=[]),
                a2a=A2AConfig(enabled=True, skills=["process_text"]),
                agui=AGUIConfig(enabled=True, events=["flow.start", "flow.complete"]),
            ),
            dependencies=DependencySpec(agents=[], tools=[], packages=[]),
            pocketflow=PocketFlowConfig(
                entry="flow.py:create_flow",
                shared_schema="schemas.py:SharedData",
            ),
            visual=VisualConfig(color="#4A90E2", size="medium", ports={}),
        )

    def test_create_agent_metadata(self, test_project_dir: Path, sample_agent_metadata: AgentMetadata) -> None:
        """エージェントメタデータを作成できることをテスト."""
        agent_dir = test_project_dir / "test-agent"
        agent_dir.mkdir()

        metadata_file = agent_dir / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(sample_agent_metadata, metadata_file)

        assert metadata_file.exists()

        # メタデータを読み込んで検証
        loaded_metadata = loader.load_from_file(metadata_file)
        assert loaded_metadata.meta.name == "Test Agent"
        assert len(loaded_metadata.interfaces.inputs) == 2
        assert len(loaded_metadata.interfaces.outputs) == 2

    @pytest.mark.asyncio
    async def test_create_and_run_agent(self, test_project_dir: Path, sample_agent_metadata: AgentMetadata) -> None:
        """エージェントを作成して実行できることをテスト."""
        # エージェントディレクトリを作成
        agent_dir = test_project_dir / "test-agent"
        agent_dir.mkdir()

        # メタデータを保存
        metadata_file = agent_dir / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(sample_agent_metadata, metadata_file)

        # テスト用のエージェントクラスを定義
        class TestAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                text = input_data.get("text", "")
                operation = input_data.get("operation", "upper")

                if operation == "upper":
                    result = text.upper()
                elif operation == "lower":
                    result = text.lower()
                elif operation == "reverse":
                    result = text[::-1]
                else:
                    result = text

                return {"result": result, "original": text}

        # エージェントを作成
        agent = TestAgent(metadata_path=metadata_file)
        agent.load_metadata()

        # エージェントを実行
        result = await agent.run({"text": "Hello World", "operation": "upper"})

        assert result["result"] == "HELLO WORLD"
        assert result["original"] == "Hello World"

    @pytest.mark.asyncio
    async def test_agent_with_context_manager(
        self, test_project_dir: Path, sample_agent_metadata: AgentMetadata
    ) -> None:
        """コンテキストマネージャーを使用してエージェントを実行できることをテスト."""
        # エージェントディレクトリを作成
        agent_dir = test_project_dir / "test-agent"
        agent_dir.mkdir()

        # メタデータを保存
        metadata_file = agent_dir / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(sample_agent_metadata, metadata_file)

        # テスト用のエージェントクラスを定義
        class TestAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                return {"result": "ok"}

        # コンテキストマネージャーを使用
        async with TestAgent(metadata_path=metadata_file) as agent:
            assert agent.is_initialized
            result = await agent.run({"text": "test"})
            assert result["result"] == "ok"

    @pytest.mark.asyncio
    async def test_agent_with_protocols(self, test_project_dir: Path, sample_agent_metadata: AgentMetadata) -> None:
        """プロトコルアダプターが正しく動作することをテスト."""
        # エージェントディレクトリを作成
        agent_dir = test_project_dir / "test-agent"
        agent_dir.mkdir()

        # メタデータを保存
        metadata_file = agent_dir / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(sample_agent_metadata, metadata_file)

        # テスト用のエージェントクラスを定義
        class TestAgent(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                return {"result": "ok"}

        # エージェントを作成
        agent = TestAgent(metadata_path=metadata_file)
        agent.load_metadata()

        # MCP ツールを取得
        mcp_tools = agent.get_mcp_tools()
        assert isinstance(mcp_tools, list)
        assert len(mcp_tools) > 0

        # A2A カードを取得
        a2a_card = agent.get_a2a_card()
        assert a2a_card is not None
        assert a2a_card.name == "Test Agent"
        assert len(a2a_card.skills) > 0

        # AG-UI エミッターを作成
        emitter = agent.create_agui_emitter(agent.engine)
        assert emitter is not None

    @pytest.mark.asyncio
    async def test_multiple_agents_workflow(self, test_project_dir: Path, sample_agent_metadata: AgentMetadata) -> None:
        """複数のエージェントを連携させるワークフローをテスト."""
        # エージェント1を作成
        agent1_dir = test_project_dir / "agent1"
        agent1_dir.mkdir()
        metadata1_file = agent1_dir / "agent.yaml"
        loader = SchemaLoader()
        loader.save_to_file(sample_agent_metadata, metadata1_file)

        # エージェント2を作成
        agent2_dir = test_project_dir / "agent2"
        agent2_dir.mkdir()
        metadata2_file = agent2_dir / "agent.yaml"
        loader.save_to_file(sample_agent_metadata, metadata2_file)

        # テスト用のエージェントクラスを定義
        class Agent1(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                text = input_data.get("text", "")
                return {"result": text.upper(), "stage": "agent1"}

        class Agent2(AgentBlock):
            async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
                text = input_data.get("result", "")
                return {"result": text[::-1], "stage": "agent2"}

        # エージェントを作成
        agent1 = Agent1(metadata_path=metadata1_file)
        agent1.load_metadata()

        agent2 = Agent2(metadata_path=metadata2_file)
        agent2.load_metadata()

        # ワークフローを実行
        result1 = await agent1.run({"text": "hello"})
        assert result1["result"] == "HELLO"
        assert result1["stage"] == "agent1"

        result2 = await agent2.run(result1)
        assert result2["result"] == "OLLEH"
        assert result2["stage"] == "agent2"

    def test_save_and_load_results(self, test_project_dir: Path) -> None:
        """結果をファイルに保存して読み込めることをテスト."""
        results_file = test_project_dir / "results.json"

        # 結果を保存
        results = {
            "result": "test result",
            "metadata": {"agent": "test-agent", "version": "1.0.0"},
        }

        with open(results_file, "w", encoding="utf-8") as f:
            json.dump(results, f, ensure_ascii=False, indent=2)

        assert results_file.exists()

        # 結果を読み込み
        with open(results_file, encoding="utf-8") as f:
            loaded_results = json.load(f)

        assert loaded_results["result"] == "test result"
        assert loaded_results["metadata"]["agent"] == "test-agent"
