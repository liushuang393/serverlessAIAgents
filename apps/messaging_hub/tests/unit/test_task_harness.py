"""Task harness unit tests."""

from __future__ import annotations

from typing import Any

import pytest

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
    assert plan.provider_strategy.mode == ProviderDiscoveryMode.DISCOVER_FIRST
    assert "destination" in plan.missing_inputs
    assert "return_window" in plan.missing_inputs
    assert any(worker.role.value == "main_agent" for worker in plan.workers)
    assert any(worker.role.value == "specialist" for worker in plan.workers)
    assert any(worker.role.value == "monitor" for worker in plan.workers)
    assert "user_preferences" in plan.memory_plan.main_agent_keys
    assert "provider_candidates" in plan.memory_plan.shared_task_keys
    assert "browser" in plan.tool_context.available_mcp_servers


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
