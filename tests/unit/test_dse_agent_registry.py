"""Design Skills Engine AgentRegistry のテスト."""


class TestDesignAgentRegistry:
    """DesignAgentRegistry テスト."""

    def _make_registry(self):
        """テスト用Registryインスタンスを生成."""
        from agentflow.skills.builtin.design_skills.services.agent_registry import (
            DesignAgentRegistry,
        )
        return DesignAgentRegistry(llm_client=None)

    async def test_initialize_loads_agents(self) -> None:
        """初期化でAgent定義が読み込まれること."""
        registry = self._make_registry()
        await registry.initialize()
        assert registry.total_agents == 3

    async def test_flow_id(self) -> None:
        """Flow IDが正しいこと."""
        registry = self._make_registry()
        await registry.initialize()
        assert registry.flow_id == "design-skills-engine"

    async def test_get_intent_analyzer(self) -> None:
        """IntentAnalyzerAgentが取得できること."""
        registry = self._make_registry()
        await registry.initialize()

        agent = registry.get_agent("intent_analyzer")
        assert agent is not None
        assert agent.name == "IntentAnalyzerAgent"

    async def test_get_prompt_planner(self) -> None:
        """PromptPlannerAgentが取得できること."""
        registry = self._make_registry()
        await registry.initialize()

        agent = registry.get_agent("prompt_planner")
        assert agent is not None
        assert agent.name == "PromptPlannerAgent"

    async def test_get_workflow_executor(self) -> None:
        """WorkflowExecutorAgentが取得できること."""
        registry = self._make_registry()
        await registry.initialize()

        agent = registry.get_agent("workflow_executor")
        assert agent is not None
        assert agent.name == "WorkflowExecutorAgent"

    async def test_get_ordered_agents(self) -> None:
        """定義順でAgentリストが取得できること."""
        registry = self._make_registry()
        await registry.initialize()

        agents = registry.get_ordered_agents()
        assert len(agents) == 3
        assert agents[0].name == "IntentAnalyzerAgent"
        assert agents[1].name == "PromptPlannerAgent"
        assert agents[2].name == "WorkflowExecutorAgent"

    async def test_double_initialize_is_safe(self) -> None:
        """二重初期化が安全であること."""
        registry = self._make_registry()
        await registry.initialize()
        await registry.initialize()  # 二回目は何も起きない
        assert registry.total_agents == 3

    async def test_agent_caching(self) -> None:
        """Agentインスタンスがキャッシュされること."""
        registry = self._make_registry()
        await registry.initialize()

        agent1 = registry.get_agent("intent_analyzer")
        agent2 = registry.get_agent("intent_analyzer")
        assert agent1 is agent2  # 同じインスタンス
