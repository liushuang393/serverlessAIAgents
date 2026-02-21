"""Quick API テスト.

create_flow, Flow, MemoryAccessor のテスト。
"""

import pytest

from agentflow import Flow, FlowWrapper, create_flow
from agentflow.core.agent_block import AgentBlock


class DummyAgent(AgentBlock):
    """テスト用ダミーAgent."""

    async def run(self, input_data: dict) -> dict:
        """入力を加工して返す."""
        return {"processed": True, "input": input_data.get("task", "")}


class CounterAgent(AgentBlock):
    """カウンターAgent."""

    def __init__(self) -> None:
        super().__init__()
        self.call_count = 0

    async def run(self, input_data: dict) -> dict:
        """呼び出し回数をカウント."""
        self.call_count += 1
        return {"count": self.call_count, **input_data}


class TestCreateFlow:
    """create_flow関数のテスト."""

    def test_create_flow_returns_flow(self) -> None:
        """create_flowがFlowインスタンスを返す."""
        flow = create_flow([DummyAgent()])
        assert isinstance(flow, Flow)

    def test_flow_has_name(self) -> None:
        """Flowに名前がある."""
        flow = create_flow([DummyAgent()], name="test-flow")
        assert flow.name == "test-flow"

    def test_flow_default_name(self) -> None:
        """デフォルト名が生成される."""
        flow = create_flow([DummyAgent()])
        assert "flow-" in flow.name

    def test_flowwrapper_is_alias(self) -> None:
        """FlowWrapperはFlowのエイリアス."""
        assert FlowWrapper is Flow


class TestFlowRun:
    """Flow.run()のテスト."""

    @pytest.mark.asyncio
    async def test_run_returns_final_result(self) -> None:
        """runは最終結果を直接返す（ラップしない）."""
        flow = create_flow([DummyAgent()])
        result = await flow.run({"task": "hello"})

        # final_resultをアンラップして返す
        assert result == {"processed": True, "input": "hello"}

    @pytest.mark.asyncio
    async def test_run_sequential_agents(self) -> None:
        """複数Agentを順次実行."""
        agent1 = CounterAgent()
        agent2 = CounterAgent()
        flow = create_flow([agent1, agent2])

        await flow.run({"task": "test"})

        assert agent1.call_count == 1
        assert agent2.call_count == 1


class TestFlowMemory:
    """Flow.memoryのテスト."""

    def test_memory_accessor_exists(self) -> None:
        """memoryアクセサがある."""
        flow = create_flow([DummyAgent()])
        assert hasattr(flow, "memory")

    def test_remember_and_recall(self) -> None:
        """remember/recallが動作."""
        flow = create_flow([DummyAgent()])
        flow.memory.remember("key1", "value1")
        assert flow.memory.recall("key1") == "value1"

    def test_recall_default(self) -> None:
        """recallのデフォルト値."""
        flow = create_flow([DummyAgent()])
        assert flow.memory.recall("nonexistent", "default") == "default"

    def test_forget(self) -> None:
        """forgetが動作."""
        flow = create_flow([DummyAgent()])
        flow.memory.remember("key", "value")
        flow.memory.forget("key")
        assert flow.memory.recall("key") is None


class TestFlowRunStream:
    """Flow.run_stream()のテスト."""

    @pytest.mark.asyncio
    async def test_run_stream_yields_events(self) -> None:
        """run_streamがイベントをyieldする."""
        flow = create_flow([DummyAgent()])
        events = []

        async for event in flow.run_stream({"task": "hello"}):
            events.append(event)

        # 少なくともresultイベントがある
        event_types = [e["type"] for e in events]
        assert "result" in event_types

    @pytest.mark.asyncio
    async def test_run_stream_node_events(self) -> None:
        """node_start/node_completeイベントが発火."""
        flow = create_flow([DummyAgent()])
        events = []

        async for event in flow.run_stream({"task": "hello"}):
            events.append(event)

        event_types = [e["type"] for e in events]
        assert "node_start" in event_types
        assert "node_complete" in event_types


class TestFlowCleanup:
    """Flow.cleanup()のテスト."""

    @pytest.mark.asyncio
    async def test_cleanup_resets_initialized(self) -> None:
        """cleanupが初期化状態をリセット."""
        flow = create_flow([DummyAgent()])
        await flow.run({"task": "test"})
        assert flow._initialized is True

        await flow.cleanup()
        assert flow._initialized is False
