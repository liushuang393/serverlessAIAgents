# -*- coding: utf-8 -*-
"""Messaging Hub security mode tests."""

from __future__ import annotations

import pytest

from apps.messaging_hub.coordinator import AssistantConfig, PersonalAssistantCoordinator


@pytest.mark.asyncio
async def test_file_organize_blocked_when_os_skills_disabled() -> None:
    """approval_required/read_only 相当では OS 操作をブロックする."""
    coordinator = PersonalAssistantCoordinator(
        config=AssistantConfig(
            enable_os_skills=False,
            enable_browser_skills=False,
            security_mode="approval_required",
        ),
    )
    result = await coordinator._execute_file_organize({}, {})  # noqa: SLF001
    assert result["blocked"] is True
    assert result["security_mode"] == "approval_required"


@pytest.mark.asyncio
async def test_competitor_analysis_runs_when_browser_enabled() -> None:
    """autonomous 相当では browser 操作を許可する."""
    coordinator = PersonalAssistantCoordinator(
        config=AssistantConfig(
            enable_os_skills=True,
            enable_browser_skills=True,
            security_mode="autonomous",
        ),
    )
    result = await coordinator._execute_competitor_analysis({"competitor": "A社"}, {})  # noqa: SLF001
    assert result.get("blocked") is not True
    assert result["competitor"] == "A社"

