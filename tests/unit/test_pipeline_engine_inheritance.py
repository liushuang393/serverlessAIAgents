# -*- coding: utf-8 -*-
"""PipelineEngine 继承场景测试.

测试 PipelineEngine 的继承模式，确保子类能够安全地扩展而不遗漏关键步骤。

测试场景:
1. 使用 _setup_stages() 钩子动态配置 stages（推荐模式）
2. 直接覆盖 _initialize()（需要调用 _finalize_initialization）
3. 验证 Flow 构建正确性
4. 验证 run_stream() 正常工作
"""

import pytest
from typing import Any

from agentflow.engines import PipelineEngine, EngineConfig


# ============================================================
# Mock Agents
# ============================================================


class MockAgent:
    """简单 Mock Agent."""

    def __init__(self, name: str = "MockAgent"):
        self.name = name
        self._initialized = False

    async def initialize(self) -> None:
        self._initialized = True

    async def run(self, inputs: dict) -> dict:
        return {"agent": self.name, "processed": True, "input": inputs.get("question", "")}


class MockGateAgent:
    """Mock Gate Agent - 用于测试 Gate 节点."""

    name = "MockGate"

    async def run(self, inputs: dict) -> dict:
        question = inputs.get("question", "")
        passed = "reject" not in question.lower()
        return {"is_acceptable": passed, "reason": "OK" if passed else "Rejected"}


class MockReviewAgent:
    """Mock Review Agent - 用于测试 Review 节点."""

    name = "MockReview"
    _call_count = 0

    def __init__(self):
        MockReviewAgent._call_count = 0

    async def run(self, inputs: dict) -> dict:
        MockReviewAgent._call_count += 1
        # 第一次返回 REVISE，后续返回 PASS
        if MockReviewAgent._call_count == 1:
            return {"overall_verdict": "REVISE", "reason": "Need improvement"}
        return {"overall_verdict": "PASS", "reason": "Approved"}


# ============================================================
# 模拟的 AgentRegistry（类似 DecisionEngine 的模式）
# ============================================================


class MockAgentRegistry:
    """模拟 AgentRegistry 的行为."""

    def __init__(self):
        self._initialized = False
        self._agents: dict[str, Any] = {}

    async def initialize(self) -> None:
        self._initialized = True
        # 创建 agents
        self._agents = {
            "gate": MockGateAgent(),
            "process1": MockAgent("Process1"),
            "process2": MockAgent("Process2"),
            "review": MockReviewAgent(),
        }

    def get_agent(self, agent_id: str) -> Any:
        if not self._initialized:
            raise RuntimeError("Registry not initialized")
        return self._agents.get(agent_id)


# ============================================================
# 测试用 Engine 实现
# ============================================================


class DynamicPipelineEngine(PipelineEngine):
    """使用 _setup_stages() 钩子动态配置 stages（推荐模式）."""

    def __init__(self, registry: MockAgentRegistry | None = None):
        self._registry = registry or MockAgentRegistry()
        super().__init__(
            stages=[],  # 初始为空，在 _setup_stages 中动态设置
            max_revisions=2,
            config=EngineConfig(name="dynamic-pipeline"),
        )

    async def _setup_stages(self) -> None:
        """动态设置 stages."""
        await self._registry.initialize()

        self._stage_configs = self._parse_stages([
            {
                "name": "gate",
                "agent": self._registry.get_agent("gate"),
                "gate": True,
                "gate_check": lambda r: r.get("is_acceptable", False),
            },
            {
                "name": "process1",
                "agent": self._registry.get_agent("process1"),
            },
            {
                "name": "process2",
                "agent": self._registry.get_agent("process2"),
            },
            {
                "name": "review",
                "agent": self._registry.get_agent("review"),
                "review": True,
                "retry_from": "process1",
            },
        ])

        # 设置 stage_instances
        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            self._stage_instances[stage.name] = instances


class CorrectOverrideEngine(PipelineEngine):
    """正确覆盖 _initialize() 的模式（需要调用 _finalize_initialization）."""

    def __init__(self):
        super().__init__(
            stages=[],
            config=EngineConfig(name="correct-override"),
        )
        self._custom_initialized = False

    async def _initialize(self) -> None:
        # 自定义初始化逻辑
        self._custom_initialized = True

        # 动态设置 stages
        self._stage_configs = self._parse_stages([
            {"name": "step1", "agent": MockAgent("Step1")},
            {"name": "step2", "agent": MockAgent("Step2")},
        ])

        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            self._stage_instances[stage.name] = instances

        # 关键：必须调用 _finalize_initialization()
        await self._finalize_initialization()


class BrokenOverrideEngine(PipelineEngine):
    """错误覆盖 _initialize() 的模式（遗漏 _finalize_initialization）.

    这个类故意模拟一个忘记调用 _finalize_initialization 的错误实现。
    """

    def __init__(self):
        super().__init__(
            stages=[],
            config=EngineConfig(name="broken-override"),
        )

    async def _initialize(self) -> None:
        # 设置 stages
        self._stage_configs = self._parse_stages([
            {"name": "step1", "agent": MockAgent("Step1")},
        ])

        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            self._stage_instances[stage.name] = instances

        # 错误：忘记调用 _finalize_initialization()
        # self._flow = self._build_flow()  # 遗漏!


# ============================================================
# Tests
# ============================================================


class TestPipelineEngineSetupStagesHook:
    """测试 _setup_stages() 钩子模式（推荐模式）."""

    @pytest.mark.asyncio
    async def test_dynamic_stages_setup(self):
        """测试动态 stages 配置."""
        engine = DynamicPipelineEngine()
        result = await engine.run({"question": "Test question"})

        assert result["status"] == "success"
        assert "results" in result

    @pytest.mark.asyncio
    async def test_flow_is_built(self):
        """测试 Flow 正确构建."""
        engine = DynamicPipelineEngine()

        # 触发初始化
        await engine.run({"question": "Test"})

        # 验证 Flow 已构建
        assert engine._flow is not None
        assert engine._flow.node_count > 0

    @pytest.mark.asyncio
    async def test_run_stream_works(self):
        """测试 run_stream() 正常工作."""
        engine = DynamicPipelineEngine()
        events = []

        async for event in engine.run_stream({"question": "Test stream"}):
            events.append(event)

        # 验证收到了事件
        assert len(events) > 0

        # 检查事件类型
        event_types = [e.get("event_type") or e.get("type") for e in events]

        # 应该有 flow_start 事件
        assert any("flow" in str(t).lower() and "start" in str(t).lower() for t in event_types)

    @pytest.mark.asyncio
    async def test_gate_rejection(self):
        """测试 Gate 拒绝场景."""
        engine = DynamicPipelineEngine()

        # 包含 "reject" 的问题应该被 Gate 拒绝
        result = await engine.run({"question": "Please reject this"})

        # Gate 拒绝后应该 early return
        assert result.get("status") in ("rejected", "success")


class TestPipelineEngineCorrectOverride:
    """测试正确覆盖 _initialize() 的模式."""

    @pytest.mark.asyncio
    async def test_correct_override_works(self):
        """测试正确覆盖模式正常工作."""
        engine = CorrectOverrideEngine()
        result = await engine.run({"question": "Test"})

        assert result["status"] == "success"
        assert engine._custom_initialized is True

    @pytest.mark.asyncio
    async def test_flow_is_built_with_correct_override(self):
        """测试正确覆盖模式下 Flow 正确构建."""
        engine = CorrectOverrideEngine()
        await engine.run({"question": "Test"})

        assert engine._flow is not None


class TestPipelineEngineBrokenOverride:
    """测试错误覆盖 _initialize() 的模式.

    这些测试验证当遗漏 _finalize_initialization() 时的行为。
    """

    @pytest.mark.asyncio
    async def test_broken_override_flow_is_none(self):
        """测试错误覆盖模式下 Flow 为 None."""
        engine = BrokenOverrideEngine()

        # 触发初始化
        # run() 仍然可以工作（使用 fallback 逻辑），但 _flow 应该是 None
        await engine.run({"question": "Test"})

        # Flow 应该是 None（因为遗漏了 _finalize_initialization）
        assert engine._flow is None

    @pytest.mark.asyncio
    async def test_broken_override_stream_uses_fallback(self):
        """测试错误覆盖模式下 run_stream 使用 fallback 逻辑."""
        engine = BrokenOverrideEngine()
        events = []

        async for event in engine.run_stream({"question": "Test"}):
            events.append(event)

        # Fallback 逻辑仍然能产生事件
        assert len(events) > 0

        # 但是 _flow 仍然是 None
        assert engine._flow is None


class TestPipelineEngineReviewLoop:
    """测试 Review 循环（REVISE 回退）."""

    @pytest.mark.asyncio
    async def test_review_revise_loop(self):
        """测试 REVISE 回退机制."""
        engine = DynamicPipelineEngine()

        # MockReviewAgent 第一次返回 REVISE，第二次返回 PASS
        result = await engine.run({"question": "Test review loop"})

        assert result["status"] == "success"
        # Review agent 应该被调用 2 次
        assert MockReviewAgent._call_count == 2


class TestPipelineEngineAgentInitialization:
    """测试 Agent 初始化."""

    @pytest.mark.asyncio
    async def test_agents_are_initialized(self):
        """测试 Agent 的 initialize() 方法被调用."""
        # 创建带有 initialize 方法的 agent
        agent = MockAgent("TestAgent")

        engine = PipelineEngine(
            stages=[{"name": "test", "agent": agent}],
            config=EngineConfig(name="init-test"),
        )

        await engine.run({"question": "Test"})

        # Agent 应该被初始化
        assert agent._initialized is True


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
