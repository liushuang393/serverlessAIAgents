"""Task harness unit tests."""

from __future__ import annotations

from typing import Any

import pytest

from apps.messaging_hub.execution_substrate import ExecutionProfile
from apps.messaging_hub.task_harness import (
    ProviderDiscoveryMode,
    TaskHarnessPlanner,
    TaskProviderDiscoveryService,
)
from kernel.skills.gateway import RiskLevel, SkillCategory, SkillDefinition, SkillResult


class _DummyGateway:
    """最小 SkillGateway 代替."""

    def __init__(self) -> None:
        self._skills = [
            SkillDefinition(
                name="web_search",
                description="web search",
                category=SkillCategory.NETWORK,
                risk_level=RiskLevel.LOW,
                handler=self._noop,
            ),
            SkillDefinition(
                name="browser_navigate",
                description="browser nav",
                category=SkillCategory.BROWSER,
                risk_level=RiskLevel.LOW,
                handler=self._noop,
            ),
            SkillDefinition(
                name="browser_get_text",
                description="browser text",
                category=SkillCategory.BROWSER,
                risk_level=RiskLevel.LOW,
                handler=self._noop,
            ),
            SkillDefinition(
                name="read_file",
                description="read file",
                category=SkillCategory.OS_READ,
                risk_level=RiskLevel.LOW,
                handler=self._noop,
            ),
        ]

    async def _noop(self, *_args: Any, **_kwargs: Any) -> dict[str, Any]:
        return {}

    def list_available_skills(self) -> list[SkillDefinition]:
        return list(self._skills)

    async def call(self, skill_name: str, params: dict[str, Any]) -> SkillResult:
        assert skill_name == "web_search"
        assert isinstance(params.get("query"), str)
        return SkillResult(
            success=True,
            skill_name=skill_name,
            result={
                "results": [
                    {
                        "title": "Best flight comparison site",
                        "snippet": "Compare flight prices and airfare trends",
                        "url": "https://www.example.com/flights",
                    },
                    {
                        "title": "Example flight deals",
                        "snippet": "Another page on the same domain",
                        "url": "https://www.example.com/deals",
                    },
                    {
                        "title": "Travel aggregator for cheap tickets",
                        "snippet": "Find cheap ticket prices fast",
                        "url": "https://travel.test/search",
                    },
                ]
            },
        )


class _DummyMCPManager:
    """最小 MCP manager 代替."""

    @staticmethod
    def list_servers() -> list[dict[str, Any]]:
        return [{"name": "browser"}, {"name": "serpapi"}]


@pytest.mark.asyncio
async def test_task_harness_builds_generic_flight_plan() -> None:
    """flight watch が generic harness plan に落ちること."""
    planner = TaskHarnessPlanner(
        skill_gateway=_DummyGateway(),
        mcp_manager=_DummyMCPManager(),
    )

    plan = await planner.build_plan(
        message="Monitor flight prices from HND to LAX",
        required_capability="flight_watch",
        input_data={
            "request": {
                "origin": "HND",
                "depart_window": {"start_date": "2026-05-01", "end_date": "2026-05-03"},
                "create_watch": True,
            }
        },
        partial_request={
            "origin": "HND",
            "depart_window": {"start_date": "2026-05-01", "end_date": "2026-05-03"},
        },
    )

    assert plan.blueprint_id == "structured_monitoring.flight_watch"
    assert plan.execution_profile == ExecutionProfile.MONITORED_READ_THEN_WRITE
    assert plan.provider_strategy.mode == ProviderDiscoveryMode.DISCOVER_FIRST
    assert plan.checkpoint_policy.requires_pre_mutation is True
    assert plan.gate_policy.verification_profile == "constraint"
    assert plan.verification.verification_profile == "constraint"
    assert "destination" in plan.missing_inputs
    assert "return_window" in plan.missing_inputs
    assert any(worker.role.value == "main_agent" for worker in plan.workers)
    assert any(worker.role.value == "specialist" for worker in plan.workers)
    assert any(worker.role.value == "monitor" for worker in plan.workers)
    assert "user_preferences" in plan.memory_plan.main_agent_keys
    assert "provider_candidates" in plan.memory_plan.shared_task_keys
    assert "browser" in plan.tool_context.available_mcp_servers
    assert "create_watch" in plan.context_hierarchy.open_actions


@pytest.mark.asyncio
async def test_task_harness_builds_lightweight_general_plan() -> None:
    """general task は lightweight default を選ぶこと."""
    planner = TaskHarnessPlanner(
        skill_gateway=_DummyGateway(),
        mcp_manager=_DummyMCPManager(),
    )

    plan = await planner.build_plan(
        message="Summarize my unread messages",
        required_capability=None,
        input_data={},
    )

    assert plan.execution_profile == ExecutionProfile.LIGHTWEIGHT_DEFAULT
    assert plan.gate_policy.mode == "lightweight_default"
    assert plan.gate_policy.verification_profile == "basic"
    assert plan.verification.verification_profile == "basic"
    assert plan.checkpoint_policy.requires_pre_mutation is False
    assert plan.verification.constraints[0].field == "result_payload"


@pytest.mark.asyncio
async def test_task_harness_capability_route_can_upgrade_to_gap_fill_profile() -> None:
    """explicit capability route と gap fill overlay が切り替わること."""
    planner = TaskHarnessPlanner(
        skill_gateway=_DummyGateway(),
        mcp_manager=_DummyMCPManager(),
    )

    plan = await planner.build_plan(
        message="Review travel reimbursement exceptions",
        required_capability="travel_policy_review",
        input_data={},
    )

    assert plan.execution_profile == ExecutionProfile.CAPABILITY_ROUTE
    assert plan.gate_policy.mode == "capability_route"
    updated = planner.apply_execution_profile(plan, ExecutionProfile.GAP_FILL_GOVERNED)
    assert updated.execution_profile == ExecutionProfile.GAP_FILL_GOVERNED
    assert updated.gate_policy.artifact_governance_required is True
    assert updated.gate_policy.allow_runtime_artifact is True
    assert updated.verification.verification_profile == "artifact_governance"
    assert updated.verification.constraints[0].field == "generated_artifact"
    assert "approve_artifact" in updated.context_hierarchy.open_actions


@pytest.mark.asyncio
async def test_provider_discovery_ranks_and_deduplicates_domains() -> None:
    """provider discovery が domain 単位で正規化・順位付けすること."""
    discovery = TaskProviderDiscoveryService(skill_gateway=_DummyGateway())

    candidates = await discovery.discover(
        query="best websites to compare flight prices HND LAX",
        task_kind="structured_monitoring",
        keywords=["flight", "price"],
        candidate_limit=5,
    )

    assert len(candidates) == 2
    assert candidates[0].score >= candidates[1].score
    assert candidates[0].domain == "www.example.com"
    assert candidates[1].domain == "travel.test"
