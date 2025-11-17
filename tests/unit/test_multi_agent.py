"""Multi-Agent Collaboration Pattern のテスト."""

import pytest

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.multi_agent import (
    AgentCoordinator,
    AgentRouter,
    MultiAgentWorkflow,
    SharedContext,
)


class MockLLMClient:
    """モック LLM クライアント."""

    def __init__(self, responses: list[str] | None = None) -> None:
        """初期化."""
        self.responses = responses or []
        self.call_count = 0

    async def generate(self, prompt: str) -> str:
        """生成."""
        if self.call_count < len(self.responses):
            response = self.responses[self.call_count]
            self.call_count += 1
            return response
        return "デフォルトレスポンス"


class MockAgent(AgentBlock):
    """モック Agent."""

    def __init__(self, name: str, output: str = "結果") -> None:
        """初期化."""
        super().__init__()
        self.name = name
        self.output = output
        self.call_count = 0

    async def run(self, input_data: dict) -> dict:
        """実行."""
        self.call_count += 1
        return {
            "agent": self.name,
            "output": self.output,
            "input": input_data,
        }


class MockHandoffAgent(AgentBlock):
    """モック Handoff Agent."""

    def __init__(self, name: str, handoff_to: str | None = None) -> None:
        """初期化."""
        super().__init__()
        self.name = name
        self.handoff_to = handoff_to

    async def run(self, input_data: dict) -> dict:
        """実行."""
        result = {
            "agent": self.name,
            "output": f"{self.name} の結果",
        }
        if self.handoff_to:
            result["handoff_to"] = self.handoff_to
        return result


# ========================================
# SharedContext Tests
# ========================================


def test_shared_context_set_get():
    """SharedContext: 設定と取得."""
    context = SharedContext()

    context.set("key1", "value1")
    context.set("key2", {"data": "value2"})

    assert context.get("key1") == "value1"
    assert context.get("key2") == {"data": "value2"}
    assert context.get("key3") is None
    assert context.get("key3", "default") == "default"


def test_shared_context_get_all():
    """SharedContext: 全取得."""
    context = SharedContext()

    context.set("a", 1)
    context.set("b", 2)

    all_data = context.get_all()
    assert all_data == {"a": 1, "b": 2}


def test_shared_context_history():
    """SharedContext: 履歴."""
    context = SharedContext()

    context.set("key1", "value1")
    context.set("key2", "value2")

    history = context.get_history()
    assert len(history) == 2
    assert history[0]["key"] == "key1"
    assert history[1]["key"] == "key2"
    assert "timestamp" in history[0]


def test_shared_context_clear():
    """SharedContext: クリア."""
    context = SharedContext()

    context.set("key", "value")
    context.clear()

    assert context.get("key") is None
    assert len(context.get_history()) == 0


# ========================================
# AgentRouter Tests
# ========================================


@pytest.mark.asyncio
async def test_agent_router_with_llm():
    """AgentRouter: LLM 使用."""
    agent_a = MockAgent("AgentA")
    agent_b = MockAgent("AgentB")
    llm = MockLLMClient(responses=["AgentA を選択します"])

    router = AgentRouter(
        agents={"AgentA": agent_a, "AgentB": agent_b},
        llm_client=llm,
    )

    result = await router.run({"task": "タスク A"})

    assert result["selected_agent"] == "AgentA"
    assert result["result"]["agent"] == "AgentA"
    assert "reasoning" in result


@pytest.mark.asyncio
async def test_agent_router_without_llm():
    """AgentRouter: LLM なし、Fallback."""
    agent_a = MockAgent("AgentA")
    agent_b = MockAgent("AgentB")

    router = AgentRouter(
        agents={"AgentA": agent_a, "AgentB": agent_b},
        llm_client=None,
    )

    result = await router.run({"task": "タスク"})

    # Fallback: 最初の Agent を選択
    assert result["selected_agent"] == "AgentA"
    assert "LLM が設定されていない" in result["reasoning"]


@pytest.mark.asyncio
async def test_agent_router_no_agents():
    """AgentRouter: Agent なし."""
    router = AgentRouter(agents={}, llm_client=None)

    result = await router.run({"task": "タスク"})

    assert result["selected_agent"] is None
    assert "error" in result


# ========================================
# AgentCoordinator Tests
# ========================================


@pytest.mark.asyncio
async def test_coordinator_sequential():
    """AgentCoordinator: Sequential パターン."""
    agent_a = MockAgent("AgentA", "結果A")
    agent_b = MockAgent("AgentB", "結果B")
    agent_c = MockAgent("AgentC", "結果C")

    coordinator = AgentCoordinator(
        agents=[agent_a, agent_b, agent_c],
        pattern="sequential",
    )

    result = await coordinator.execute("タスク")

    assert result["pattern"] == "sequential"
    assert result["agents_executed"] == 3
    assert "final_result" in result

    # 全 Agent が実行された
    assert agent_a.call_count == 1
    assert agent_b.call_count == 1
    assert agent_c.call_count == 1


@pytest.mark.asyncio
async def test_coordinator_concurrent():
    """AgentCoordinator: Concurrent パターン."""
    agent_a = MockAgent("AgentA", "結果A")
    agent_b = MockAgent("AgentB", "結果B")
    agent_c = MockAgent("AgentC", "結果C")

    coordinator = AgentCoordinator(
        agents=[agent_a, agent_b, agent_c],
        pattern="concurrent",
    )

    result = await coordinator.execute("タスク")

    assert result["pattern"] == "concurrent"
    assert result["agents_executed"] == 3
    assert len(result["results"]) == 3
    assert len(result["errors"]) == 0

    # 全 Agent が実行された
    assert agent_a.call_count == 1
    assert agent_b.call_count == 1
    assert agent_c.call_count == 1


@pytest.mark.asyncio
async def test_coordinator_handoff():
    """AgentCoordinator: Handoff パターン."""
    agent_a = MockHandoffAgent("AgentA", handoff_to="AgentB")
    agent_b = MockHandoffAgent("AgentB", handoff_to="AgentC")
    agent_c = MockHandoffAgent("AgentC", handoff_to=None)  # 完了

    coordinator = AgentCoordinator(
        agents=[agent_a, agent_b, agent_c],
        pattern="handoff",
    )

    result = await coordinator.execute("タスク")

    assert result["pattern"] == "handoff"
    assert result["agents_executed"] == 3
    assert len(result["handoff_chain"]) == 3
    assert result["handoff_chain"][0]["agent"] == "AgentA"
    assert result["handoff_chain"][2]["agent"] == "AgentC"


@pytest.mark.asyncio
async def test_coordinator_handoff_early_stop():
    """AgentCoordinator: Handoff 早期終了."""
    agent_a = MockHandoffAgent("AgentA", handoff_to="AgentB")
    agent_b = MockHandoffAgent("AgentB", handoff_to=None)  # ここで完了
    agent_c = MockHandoffAgent("AgentC", handoff_to=None)

    coordinator = AgentCoordinator(
        agents=[agent_a, agent_b, agent_c],
        pattern="handoff",
    )

    result = await coordinator.execute("タスク")

    assert result["agents_executed"] == 2  # Agent C は実行されない
    assert len(result["handoff_chain"]) == 2


@pytest.mark.asyncio
async def test_coordinator_invalid_pattern():
    """AgentCoordinator: 無効なパターン."""
    agent = MockAgent("Agent")

    coordinator = AgentCoordinator(
        agents=[agent],
        pattern="invalid_pattern",
    )

    with pytest.raises(ValueError, match="Unknown pattern"):
        await coordinator.execute("タスク")


# ========================================
# MultiAgentWorkflow Tests
# ========================================


def test_multi_agent_workflow_create_sequential():
    """MultiAgentWorkflow: Sequential 作成."""
    agent_a = MockAgent("AgentA")
    agent_b = MockAgent("AgentB")

    workflow = MultiAgentWorkflow.create(
        workflow_id="test-sequential",
        agents=[agent_a, agent_b],
        pattern="sequential",
    )

    assert workflow.workflow_id == "test-sequential"
    assert "sequential" in workflow.name
    assert workflow.config["pattern"] == "sequential"
    assert workflow.config["num_agents"] == 2
    assert len(workflow.nodes) == 3  # 2 agents + 1 coordinator


def test_multi_agent_workflow_create_concurrent():
    """MultiAgentWorkflow: Concurrent 作成."""
    agent_a = MockAgent("AgentA")
    agent_b = MockAgent("AgentB")
    agent_c = MockAgent("AgentC")

    workflow = MultiAgentWorkflow.create(
        workflow_id="test-concurrent",
        agents=[agent_a, agent_b, agent_c],
        pattern="concurrent",
    )

    assert workflow.workflow_id == "test-concurrent"
    assert "concurrent" in workflow.name
    assert workflow.config["pattern"] == "concurrent"
    assert workflow.config["num_agents"] == 3


def test_multi_agent_workflow_create_with_router():
    """MultiAgentWorkflow: Router 付き作成."""
    agent_a = MockAgent("AgentA")
    agent_b = MockAgent("AgentB")
    llm = MockLLMClient()

    workflow = MultiAgentWorkflow.create_with_router(
        workflow_id="test-router",
        agents={"AgentA": agent_a, "AgentB": agent_b},
        llm_client=llm,
    )

    assert workflow.workflow_id == "test-router"
    assert "Router" in workflow.name
    assert workflow.config["pattern"] == "router"
    assert workflow.config["num_agents"] == 2
    assert len(workflow.nodes) == 1  # router only


# ========================================
# Integration Tests
# ========================================


@pytest.mark.asyncio
async def test_full_multi_agent_pipeline():
    """統合テスト: 完全な Multi-Agent パイプライン."""
    # 3つの専門 Agent を作成
    research_agent = MockAgent("Research", "調査結果")
    analysis_agent = MockAgent("Analysis", "分析結果")
    report_agent = MockAgent("Report", "レポート")

    # Sequential Coordinator を作成
    context = SharedContext()
    coordinator = AgentCoordinator(
        agents=[research_agent, analysis_agent, report_agent],
        pattern="sequential",
        shared_context=context,
    )

    # 実行
    result = await coordinator.execute("市場調査")

    # 検証
    assert result["pattern"] == "sequential"
    assert result["agents_executed"] == 3

    # 共有コンテキストに全 Agent の結果が保存されている
    all_data = context.get_all()
    assert len(all_data) == 3
    assert "agent_0_MockAgent" in all_data
    assert "agent_2_MockAgent" in all_data

