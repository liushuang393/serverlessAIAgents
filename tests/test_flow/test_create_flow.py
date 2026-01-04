# -*- coding: utf-8 -*-
"""测试新的 create_flow 链式API.

验证:
- 基本的 then() 顺序执行
- gate() 条件拦截
- review() 审查 + REVISE回退
- parallel() 并行执行
- 进度事件发射
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
    """简单的Mock Agent."""

    def __init__(self, name: str, output: dict[str, Any] | None = None):
        self.name = name
        self._output = output or {"result": f"{name}_output"}
        self.call_count = 0

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        self.call_count += 1
        return {**self._output, "inputs_received": inputs}


class MockGateAgent:
    """Mock闸门Agent."""

    def __init__(self, should_pass: bool = True):
        self.name = "gate"
        self._should_pass = should_pass

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        return {"proceed": self._should_pass, "is_acceptable": self._should_pass}


class MockReviewAgent:
    """Mock审查Agent."""

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
    """测试FlowBuilder链式构建."""

    def test_create_flow_returns_builder(self):
        """create_flow应该返回FlowBuilder."""
        builder = create_flow("test-flow")
        assert isinstance(builder, FlowBuilder)
        assert builder.flow_id == "test-flow"

    def test_build_without_nodes_raises(self):
        """没有节点时build应该抛出异常."""
        builder = create_flow("empty-flow")
        with pytest.raises(ValueError, match="没有节点"):
            builder.build()

    def test_build_returns_flow(self):
        """build应该返回Flow实例."""
        flow = create_flow("test").then(MockAgent("a")).build()
        assert isinstance(flow, Flow)
        assert flow.flow_id == "test"
        assert flow.node_count == 1


class TestFlowExecution:
    """测试Flow执行."""

    @pytest.mark.asyncio
    async def test_simple_then(self):
        """测试简单的顺序执行."""
        agent1 = MockAgent("agent1")
        agent2 = MockAgent("agent2")

        flow = create_flow("test").then(agent1, agent2).build()
        result = await flow.run({"input": "test"})

        assert agent1.call_count == 1
        assert agent2.call_count == 1
        # 结果包含所有节点的输出（key是自动生成的节点ID）
        assert len(result) >= 2

    @pytest.mark.asyncio
    async def test_gate_pass(self):
        """测试闸门通过."""
        gate = MockGateAgent(should_pass=True)
        agent = MockAgent("agent")

        flow = create_flow("test").gate(gate).then(agent).build()
        result = await flow.run({"input": "test"})

        assert agent.call_count == 1

    @pytest.mark.asyncio
    async def test_gate_block(self):
        """测试闸门拦截."""
        gate = MockGateAgent(should_pass=False)
        agent = MockAgent("agent")

        flow = (
            create_flow("test")
            .gate(gate, on_fail=lambda ctx: {"status": "blocked"})
            .then(agent)
            .build()
        )
        result = await flow.run({"input": "test"})

        # Agent不应该被调用
        assert agent.call_count == 0
        assert result.get("status") == "blocked"

    @pytest.mark.asyncio
    async def test_review_pass(self):
        """测试审查通过."""
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
        """测试审查REVISE回退."""
        agent = MockAgent("agent")
        # 第一次REVISE，第二次PASS
        review = MockReviewAgent(verdicts=["REVISE", "PASS"])

        flow = (
            create_flow("test")
            .then(agent, ids=["agent"])
            .review(review, retry_from="agent", max_revisions=2)
            .build()
        )
        result = await flow.run({"input": "test"})

        # Agent应该被调用2次（原始 + 1次REVISE）
        assert agent.call_count == 2


class TestFlowStream:
    """测试流式执行."""

    @pytest.mark.asyncio
    async def test_stream_emits_events(self):
        """测试流式执行发射事件."""
        agent = MockAgent("agent")
        flow = create_flow("test").then(agent).build()

        events = []
        async for event in flow.run_stream({"input": "test"}):
            events.append(event)

        # 应该有 flow_start, node_start, node_complete, flow_complete
        event_types = [e["type"] for e in events]
        assert "flow_start" in event_types
        assert "node_start" in event_types
        assert "node_complete" in event_types
        assert "flow_complete" in event_types


if __name__ == "__main__":
    pytest.main([__file__, "-v"])

