"""Coordinator control plane tests."""

from __future__ import annotations

import pytest

from apps.messaging_hub.control_plane import ControlPlaneDecision, CoordinatorControlPlane
from apps.messaging_hub.coordinator import AssistantConfig, PersonalAssistantCoordinator
from apps.messaging_hub.focused_subagents import SemanticToolContractRegistry, TaskTriageSubagent
from kernel.router import Intent, IntentCategory


def test_control_plane_denies_write_contract_in_read_only_mode() -> None:
    """read_only では write 系 semantic action を deny すること."""
    triage = TaskTriageSubagent(SemanticToolContractRegistry())
    plan = triage.plan(
        template_name="report",
        parameters={"title": "Q2", "format": "markdown"},
        original_text="create report",
        security_mode="read_only",
    )

    evaluation = CoordinatorControlPlane().evaluate(plan=plan, security_mode="read_only")

    assert evaluation.decision == ControlPlaneDecision.DENY
    assert "persist_report_draft" in evaluation.blocked_operations


@pytest.mark.asyncio
async def test_coordinator_process_blocks_mutation_task_in_read_only_mode() -> None:
    """read_only モードで report 保存系タスクを事前停止すること."""
    coordinator = PersonalAssistantCoordinator(
        config=AssistantConfig(
            enable_os_skills=True,
            enable_browser_skills=True,
            security_mode="read_only",
        ),
    )

    async def _fake_route(_message: str, _context: dict[str, object]) -> Intent:
        return Intent(
            category=IntentCategory.TASK_EXECUTION,
            template_name="report",
            confidence=0.9,
            parameters={"title": "Q2", "format": "markdown"},
            original_text="Q2 report",
            rewritten_query="Q2 report",
        )

    coordinator._intent_router.route = _fake_route  # type: ignore[method-assign]

    result = await coordinator.process("Q2 report", user_id="u1")

    assert result["raw_results"]["blocked"] is True
    assert result["raw_results"]["control_plane"]["decision"] == "deny"
    assert "persist_report_draft" in result["raw_results"]["control_plane"]["blocked_operations"]
