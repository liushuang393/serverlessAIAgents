# -*- coding: utf-8 -*-
"""新しい create_flow チェーン API のテスト.

検証:
- then() の順次実行
- gate() の条件ブロック
- review() の審査 + REVISE ロールバック
- parallel() の並列実行
- 進捗イベントの送出
"""

import pytest
from typing import Any

from agentflow.flow import (
    create_flow,
    Flow,
    FlowBuilder,
    AgentProtocol,
    ReviewVerdict,
)


# ============================================================
# Mock Agents
# ============================================================


class MockAgent:
    """シンプルなモック Agent."""

    def __init__(self, name: str, output: dict[str, Any] | None = None):
        self.name = name
        self._output = output or {"result": f"{name}_output"}
        self.call_count = 0

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        self.call_count += 1
        return {**self._output, "inputs_received": inputs}


class MockGateAgent:
    """モックのゲート Agent."""

    def __init__(self, should_pass: bool = True):
        self.name = "gate"
        self._should_pass = should_pass

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        return {"proceed": self._should_pass, "is_acceptable": self._should_pass}


class MockReviewAgent:
    """モックのレビュー Agent."""

    def __init__(self, verdicts: list[str] | None = None):
        self.name = "review"
        self._verdicts = verdicts or ["PASS"]
        self._call_idx = 0

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        verdict = self._verdicts[min(self._call_idx, len(self._verdicts) - 1)]
        self._call_idx += 1
        return {"overall_verdict": verdict, "findings": []}


# ============================================================
# Tests
# ============================================================


class TestFlowBuilder:
    """FlowBuilder のチェーン構築テスト."""

    def test_create_flow_returns_builder(self):
        """create_flow は FlowBuilder を返す."""
        builder = create_flow("test-flow")
        assert isinstance(builder, FlowBuilder)
        assert builder.flow_id == "test-flow"

    def test_build_without_nodes_raises(self):
        """ノードがない場合、build は例外を送出する."""
        builder = create_flow("empty-flow")
        with pytest.raises(ValueError, match="フローにノードがありません"):
            builder.build()

    def test_build_returns_flow(self):
        """build は Flow インスタンスを返す."""
        flow = create_flow("test").then(MockAgent("a")).build()
        assert isinstance(flow, Flow)
        assert flow.flow_id == "test"
        assert flow.node_count == 1


class TestFlowExecution:
    """Flow 実行のテスト."""

    @pytest.mark.asyncio
    async def test_simple_then(self):
        """シンプルな順次実行のテスト."""
        agent1 = MockAgent("agent1")
        agent2 = MockAgent("agent2")

        flow = create_flow("test").then(agent1, agent2).build()
        result = await flow.run({"input": "test"})

        assert agent1.call_count == 1
        assert agent2.call_count == 1
        # 結果は全ノードの出力を含む（key は自動生成のノード ID）
        assert len(result) >= 2

    @pytest.mark.asyncio
    async def test_gate_pass(self):
        """ゲート通過のテスト."""
        gate = MockGateAgent(should_pass=True)
        agent = MockAgent("agent")

        flow = create_flow("test").gate(gate).then(agent).build()
        result = await flow.run({"input": "test"})

        assert agent.call_count == 1

    @pytest.mark.asyncio
    async def test_gate_block(self):
        """ゲート遮断のテスト."""
        gate = MockGateAgent(should_pass=False)
        agent = MockAgent("agent")

        flow = (
            create_flow("test")
            .gate(gate, on_fail=lambda ctx: {"status": "blocked"})
            .then(agent)
            .build()
        )
        result = await flow.run({"input": "test"})

        # Agent は呼び出されない
        assert agent.call_count == 0
        assert result.get("status") == "blocked"

    @pytest.mark.asyncio
    async def test_review_pass(self):
        """レビュー PASS のテスト."""
        agent = MockAgent("agent")
        review = MockReviewAgent(verdicts=["PASS"])

        flow = (
            create_flow("test")
            .then(agent, ids=["agent"])
            .review(review, retry_from="agent")
            .build()
        )
        result = await flow.run({"input": "test"})

        assert agent.call_count == 1

    @pytest.mark.asyncio
    async def test_review_revise(self):
        """レビュー REVISE のロールバックテスト."""
        agent = MockAgent("agent")
        # 1 回目は REVISE、2 回目は PASS
        review = MockReviewAgent(verdicts=["REVISE", "PASS"])

        flow = (
            create_flow("test")
            .then(agent, ids=["agent"])
            .review(review, retry_from="agent", max_revisions=2)
            .build()
        )
        result = await flow.run({"input": "test"})

        # Agent は 2 回呼ばれる（初回 + REVISE 1 回）
        assert agent.call_count == 2


class TestFlowStream:
    """ストリーミング実行のテスト."""

    @pytest.mark.asyncio
    async def test_stream_emits_events(self):
        """ストリーミング実行でイベントが送出される."""
        agent = MockAgent("agent")
        flow = create_flow("test").then(agent).build()

        events = []
        async for event in flow.run_stream({"input": "test"}):
            events.append(event)

        # flow_start, node_start, node_complete, flow_complete が含まれる
        event_types = [e["type"] for e in events]
        assert "flow_start" in event_types
        assert "node_start" in event_types
        assert "node_complete" in event_types
        assert "flow_complete" in event_types


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
