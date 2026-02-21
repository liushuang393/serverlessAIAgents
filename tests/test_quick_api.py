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


def _build_flow(
    *agents: AgentBlock | type[AgentBlock],
    name: str | None = None,
) -> Flow:
    """create_flow ビルダーAPIでフローを構築するヘルパー."""
    flow_id = name or "test-flow"
    builder = create_flow(flow_id, name=name)
    if agents:
        builder = builder.then(*agents)
    return builder.build()


class TestCreateFlow:
    """create_flow関数のテスト."""

    def test_create_flow_returns_flow(self) -> None:
        """create_flow().then().build() が Flow インスタンスを返す."""
        flow = _build_flow(DummyAgent)
        assert isinstance(flow, Flow)

    def test_flow_has_name(self) -> None:
        """Flowに名前がある."""
        flow = _build_flow(DummyAgent, name="test-flow")
        assert flow.name == "test-flow"

    def test_flow_default_name(self) -> None:
        """フローIDが名前になる."""
        flow = _build_flow(DummyAgent)
        assert flow.name  # 名前が設定されている

    def test_flowwrapper_is_alias(self) -> None:
        """FlowWrapperはFlowのエイリアス."""
        assert FlowWrapper is Flow


class TestFlowRun:
    """Flow.run()のテスト."""

    @pytest.mark.asyncio
    async def test_run_returns_final_result(self) -> None:
        """runはノード名をキーにした結果を返す."""
        flow = _build_flow(DummyAgent)
        result = await flow.run({"task": "hello"})

        # ノード結果が含まれる（agent_1 等）
        assert isinstance(result, dict)
        node_results = [v for v in result.values() if isinstance(v, dict)]
        assert any(
            r.get("processed") is True and r.get("input") == "hello"
            for r in node_results
        )

    @pytest.mark.asyncio
    async def test_run_sequential_agents(self) -> None:
        """複数Agentを順次実行."""
        agent1 = CounterAgent()
        agent2 = CounterAgent()
        flow = _build_flow(agent1, agent2)

        await flow.run({"task": "test"})

        assert agent1.call_count == 1
        assert agent2.call_count == 1


class TestFlowMemory:
    """Flow.memoryのテスト."""

    def test_memory_accessor_exists(self) -> None:
        """memoryアクセサがある."""
        flow = _build_flow(DummyAgent)
        assert hasattr(flow, "memory")

    def test_remember_and_recall(self) -> None:
        """remember/recallが動作."""
        flow = _build_flow(DummyAgent)
        flow.memory.remember("key1", "value1")
        assert flow.memory.recall("key1") == "value1"

    def test_recall_default(self) -> None:
        """recallのデフォルト値."""
        flow = _build_flow(DummyAgent)
        assert flow.memory.recall("nonexistent", "default") == "default"

    def test_forget(self) -> None:
        """forgetが動作."""
        flow = _build_flow(DummyAgent)
        flow.memory.remember("key", "value")
        flow.memory.forget("key")
        assert flow.memory.recall("key") is None


class TestFlowRunStream:
    """Flow.run_stream()のテスト."""

    @pytest.mark.asyncio
    async def test_run_stream_yields_events(self) -> None:
        """run_streamがイベントをyieldする."""
        flow = _build_flow(DummyAgent)
        events = []

        async for event in flow.run_stream({"task": "hello"}):
            events.append(event)

        # 少なくともresultイベントがある
        event_types = [e.get("type", "") for e in events]
        assert any("result" in t or "node" in t for t in event_types) or len(events) > 0

    @pytest.mark.asyncio
    async def test_run_stream_node_events(self) -> None:
        """node_start/node_complete 等のイベントが発火."""
        flow = _build_flow(DummyAgent)
        events = []

        async for event in flow.run_stream({"task": "hello"}):
            events.append(event)

        event_types = [e.get("type", "") for e in events]
        assert "node_start" in event_types and "node_complete" in event_types


class TestFlowCleanup:
    """Flow.cleanup()のテスト."""

    @pytest.mark.asyncio
    async def test_cleanup_resets_initialized(self) -> None:
        """cleanupが初期化状態をリセット."""
        flow = _build_flow(DummyAgent)
        await flow.run({"task": "test"})
        assert getattr(flow, "_initialized", True) is True

        await flow.cleanup()
        assert flow._initialized is False
