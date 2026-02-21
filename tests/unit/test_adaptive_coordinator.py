"""適応型コーディネーターのユニットテスト.

AdaptiveCoordinator, AgentProfile, TaskRequirementの機能をテストする。
"""

import pytest

from agentflow.patterns.adaptive_coordinator import (
    AdaptiveCoordinator,
    AgentCapability,
    AgentProfile,
    MatchScore,
    TaskRequirement,
)


class TestAgentCapability:
    """AgentCapabilityのテスト."""

    def test_create_capability(self) -> None:
        """能力作成のテスト."""
        cap = AgentCapability(
            name="analysis",
            proficiency=0.9,
        )

        assert cap.name == "analysis"
        assert cap.proficiency >= 0.89  # 浮動小数点比較

    def test_capability_default_proficiency(self) -> None:
        """デフォルト熟練度のテスト."""
        cap = AgentCapability(name="coding")

        assert cap.proficiency >= 0.49  # デフォルト値0.5


class TestAgentProfile:
    """AgentProfileのテスト."""

    def test_create_profile(self) -> None:
        """プロファイル作成のテスト."""
        profile = AgentProfile(
            id="agent-001",
            name="Expert Agent",
            capabilities={
                "analysis": AgentCapability(name="analysis", level=0.9),
                "reasoning": AgentCapability(name="reasoning", level=0.8),
            },
            performance_level=0.85,
        )

        assert profile.id == "agent-001"
        assert profile.name == "Expert Agent"
        assert len(profile.capabilities) == 2

    def test_profile_has_capability(self) -> None:
        """能力保有チェックのテスト."""
        profile = AgentProfile(
            id="agent-001",
            capabilities={
                "analysis": AgentCapability(name="analysis", level=0.9),
            },
        )

        assert "analysis" in profile.capabilities
        assert "coding" not in profile.capabilities


class TestTaskRequirement:
    """TaskRequirementのテスト."""

    def test_create_requirement(self) -> None:
        """要件作成のテスト."""
        req = TaskRequirement(
            required_capabilities=["analysis", "reasoning"],
            min_proficiency=0.7,
            priority=2,
        )

        assert "analysis" in req.required_capabilities
        assert req.min_proficiency >= 0.69  # 浮動小数点比較

    def test_requirement_defaults(self) -> None:
        """デフォルト値のテスト."""
        req = TaskRequirement(required_capabilities=["coding"])

        assert req.min_proficiency >= 0.29  # デフォルト0.3
        assert req.priority == 1  # デフォルト1


class TestAdaptiveCoordinator:
    """AdaptiveCoordinatorのテスト."""

    def test_register_agent(self) -> None:
        """Agent登録のテスト."""
        coordinator = AdaptiveCoordinator()

        profile = coordinator.register_agent(
            agent_id="agent-001",
            agent=lambda x: {"result": "done"},
            capabilities=["analysis", "reasoning"],
            performance_level=0.9,
        )

        assert profile.id == "agent-001"
        assert "analysis" in profile.capabilities

    def test_register_multiple_agents(self) -> None:
        """複数Agent登録のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="expert",
            agent=lambda x: {"result": "expert"},
            capabilities=["analysis", "reasoning"],
            performance_level=0.9,
        )
        coordinator.register_agent(
            agent_id="basic",
            agent=lambda x: {"result": "basic"},
            capabilities=["coding"],
            performance_level=0.6,
        )

        assert len(coordinator.get_all_profiles()) == 2

    def test_find_best_agent_exact_match(self) -> None:
        """最適Agent検索（完全一致）のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="expert",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        requirements = TaskRequirement(required_capabilities=["analysis"])
        match = coordinator.find_best_agent(requirements)

        assert match is not None
        assert match.agent_id == "expert"

    def test_find_best_agent_no_match(self) -> None:
        """最適Agent検索（マッチなし）のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="expert",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        requirements = TaskRequirement(required_capabilities=["coding"])
        match = coordinator.find_best_agent(requirements)

        assert match is None

    def test_find_best_agent_performance_filter(self) -> None:
        """最適Agent検索（パフォーマンスフィルター）のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="low_perf",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.4,
        )
        coordinator.register_agent(
            agent_id="high_perf",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        requirements = TaskRequirement(
            required_capabilities=["analysis"],
            minimum_performance=0.7,
        )
        match = coordinator.find_best_agent(requirements)

        assert match is not None
        assert match.agent_id == "high_perf"

    @pytest.mark.asyncio
    async def test_delegate_task(self) -> None:
        """タスク委譲のテスト."""
        coordinator = AdaptiveCoordinator()

        async def expert_agent(inputs: dict) -> dict:
            return {"result": "analyzed", "data": inputs.get("data", "")}

        coordinator.register_agent(
            agent_id="expert",
            agent=expert_agent,
            capabilities=["analysis"],
            performance_level=0.9,
        )

        result = await coordinator.delegate_task(
            task={"data": "test_data"},
            requirements=TaskRequirement(required_capabilities=["analysis"]),
        )

        assert result is not None
        assert result.success is True

    @pytest.mark.asyncio
    async def test_delegate_task_no_agent(self) -> None:
        """タスク委譲（Agentなし）のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="expert",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        result = await coordinator.delegate_task(
            task={"data": "test_data"},
            requirements=TaskRequirement(required_capabilities=["unknown_skill"]),
        )

        assert result is not None
        assert result.success is False

    def test_get_profile(self) -> None:
        """Agentプロファイル取得のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="expert",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        profile = coordinator.get_profile("expert")

        assert profile is not None
        assert profile.id == "expert"

    def test_get_all_profiles(self) -> None:
        """全Agentプロファイル取得のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="agent1",
            agent=lambda x: {},
            capabilities=["analysis"],
        )
        coordinator.register_agent(
            agent_id="agent2",
            agent=lambda x: {},
            capabilities=["coding"],
        )

        profiles = coordinator.get_all_profiles()

        assert len(profiles) == 2
        profile_ids = [p.id for p in profiles]
        assert "agent1" in profile_ids
        assert "agent2" in profile_ids

    def test_unregister_agent(self) -> None:
        """Agent登録解除のテスト."""
        coordinator = AdaptiveCoordinator()

        coordinator.register_agent(
            agent_id="agent1",
            agent=lambda x: {},
            capabilities=["analysis"],
        )

        success = coordinator.unregister_agent("agent1")

        assert success is True
        assert len(coordinator.get_all_profiles()) == 0

    def test_unregister_nonexistent_agent(self) -> None:
        """存在しないAgent登録解除のテスト."""
        coordinator = AdaptiveCoordinator()

        success = coordinator.unregister_agent("nonexistent")

        assert success is False

    def test_get_profile_nonexistent(self) -> None:
        """存在しないAgentプロファイル取得のテスト."""
        coordinator = AdaptiveCoordinator()

        profile = coordinator.get_profile("nonexistent")

        assert profile is None

    def test_find_best_agent_with_load(self) -> None:
        """負荷を考慮したAgent検索のテスト."""
        coordinator = AdaptiveCoordinator(load_balance_threshold=0.8)

        coordinator.register_agent(
            agent_id="agent1",
            agent=lambda x: {},
            capabilities=["analysis"],
            performance_level=0.9,
        )

        # 最初は見つかる
        match = coordinator.find_best_agent(TaskRequirement(required_capabilities=["analysis"]))
        assert match is not None
        assert match.agent_id == "agent1"

    @pytest.mark.asyncio
    async def test_delegate_task_with_callable_agent(self) -> None:
        """callable Agentへのタスク委譲テスト."""
        coordinator = AdaptiveCoordinator()

        async def callable_agent(task: dict) -> dict:
            return {"processed": True, "input": task}

        coordinator.register_agent(
            agent_id="callable",
            agent=callable_agent,
            capabilities=["processing"],
            performance_level=0.8,
        )

        result = await coordinator.delegate_task(
            task={"data": "test"},
            requirements=TaskRequirement(required_capabilities=["processing"]),
        )

        assert result.success is True
        assert result.result is not None

    @pytest.mark.asyncio
    async def test_delegate_task_agent_error(self) -> None:
        """Agent実行エラー時のテスト."""
        coordinator = AdaptiveCoordinator()

        async def error_agent(task: dict) -> dict:
            msg = "Agent error"
            raise ValueError(msg)

        coordinator.register_agent(
            agent_id="error_agent",
            agent=error_agent,
            capabilities=["error_prone"],
            performance_level=0.5,
        )

        result = await coordinator.delegate_task(
            task={"data": "test"},
            requirements=TaskRequirement(required_capabilities=["error_prone"]),
        )

        assert result.success is False
        assert result.error is not None
        assert "Agent error" in result.error


class TestAgentProfileAdvanced:
    """AgentProfileの追加テスト."""

    def test_profile_get_capability_score(self) -> None:
        """能力スコア取得のテスト."""
        profile = AgentProfile(
            id="test",
            name="Test Agent",
            capabilities={
                "coding": AgentCapability(name="coding", proficiency=0.9),
                "analysis": AgentCapability(name="analysis", proficiency=0.7),
            },
        )

        assert profile.get_capability_score("coding") >= 0.89
        assert profile.get_capability_score("analysis") >= 0.69
        # 存在しない能力は0
        assert profile.get_capability_score("unknown") <= 0.01

    def test_profile_current_load(self) -> None:
        """現在負荷のテスト."""
        profile = AgentProfile(
            id="test",
            name="Test Agent",
            max_concurrent=5,
        )

        assert profile.current_load <= 0.01  # 初期は0

        # 負荷を増やす
        profile.current_load = 0.5
        assert profile.current_load >= 0.49

    def test_profile_is_available(self) -> None:
        """利用可能フラグのテスト."""
        profile = AgentProfile(
            id="test",
            name="Test Agent",
        )

        assert profile.is_available is True

        profile.is_available = False
        assert profile.is_available is False


class TestTaskRequirementAdvanced:
    """TaskRequirementの追加テスト."""

    def test_requirement_with_preferred_capabilities(self) -> None:
        """推奨能力付き要件のテスト."""
        req = TaskRequirement(
            required_capabilities=["coding"],
            preferred_capabilities=["testing", "documentation"],
            min_proficiency=0.6,
        )

        assert len(req.required_capabilities) == 1
        assert len(req.preferred_capabilities) == 2
        assert req.min_proficiency >= 0.59

    def test_requirement_priority(self) -> None:
        """優先度のテスト."""
        req_high = TaskRequirement(
            required_capabilities=["urgent"],
            priority=10,
        )
        req_low = TaskRequirement(
            required_capabilities=["normal"],
            priority=1,
        )

        assert req_high.priority > req_low.priority


class TestMatchScore:
    """MatchScoreのテスト."""

    def test_match_score_creation(self) -> None:
        """MatchScore作成のテスト."""
        match = MatchScore(
            agent_id="agent1",
            score=0.85,
            capability_match=0.9,
            load_factor=0.8,
        )

        assert match.agent_id == "agent1"
        assert match.score >= 0.84
        assert match.capability_match >= 0.89
        assert match.load_factor >= 0.79

    def test_match_score_with_details(self) -> None:
        """詳細付きMatchScoreのテスト."""
        match = MatchScore(
            agent_id="agent1",
            score=0.75,
            capability_match=0.8,
            load_factor=0.7,
            details={"reason": "best match"},
        )

        assert match.details is not None
        assert match.details.get("reason") == "best match"
