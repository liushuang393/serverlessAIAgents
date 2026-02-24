"""Tests for CLI proposal routing in messaging hub coordinator."""

from __future__ import annotations

import pytest

from apps.messaging_hub.coordinator import PersonalAssistantCoordinator


def test_is_troubleshooting_message_detects_error_keywords() -> None:
    """Troubleshooting keyword detector should match startup error phrases."""
    assert PersonalAssistantCoordinator._is_troubleshooting_message("启动失败，出现报错，帮我排查") is True
    assert PersonalAssistantCoordinator._is_troubleshooting_message("normal chat question") is False


@pytest.mark.asyncio
async def test_process_returns_cli_proposal_when_escalation_triggered() -> None:
    """When escalation condition is true, process should return confirmation proposal."""
    coordinator = PersonalAssistantCoordinator()

    def _always_true(*, message, intent):  # noqa: ANN001, D401
        return True

    coordinator._should_propose_cli = _always_true  # type: ignore[method-assign]

    result = await coordinator.process("please debug startup error")
    assert result.get("needs_cli_confirmation") is True
    proposal = result.get("cli_proposal")
    assert isinstance(proposal, dict)
    assert isinstance(proposal.get("proposal_id"), str)
    assert proposal.get("mode") == "read_only"
