"""Messaging Hub が Agent を A2AHub に登録することを検証."""
from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from agentflow.protocols.a2a_hub import get_hub, reset_hub


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


class TestMessagingHubRegistration:
    def test_hub_registration_pattern(self):
        """Hub 登録パターンが動作することを検証."""
        hub = get_hub()

        mock_file_org = MagicMock()
        mock_file_org.name = "FileOrganizer"
        mock_file_org.run = MagicMock()

        mock_assistant = MagicMock()
        mock_assistant.name = "PersonalAssistantCoordinator"
        mock_assistant.run = MagicMock()

        hub.register(mock_file_org)
        hub.register(mock_assistant)

        assert hub.discover("FileOrganizer") is not None
        assert hub.discover("PersonalAssistantCoordinator") is not None
        assert hub.agent_count == 2

    def test_duplicate_registration_skipped(self):
        """重複登録がスキップされることを検証."""
        hub = get_hub()

        mock_agent = MagicMock()
        mock_agent.name = "FileOrganizer"
        mock_agent.run = MagicMock()

        hub.register(mock_agent)
        # discover で存在チェック → 登録スキップ
        if hub.discover("FileOrganizer") is None:
            hub.register(mock_agent)

        assert hub.agent_count == 1

    def test_list_agents_returns_cards(self):
        """list_agents が登録済み AgentCard を返すことを検証."""
        hub = get_hub()

        mock_agent = MagicMock()
        mock_agent.name = "TestAgent"
        mock_agent.run = MagicMock()

        hub.register(mock_agent)
        cards = hub.list_agents()

        assert len(cards) == 1
        assert cards[0].name == "TestAgent"

    def test_to_a2a_format_on_cards(self):
        """AgentCard.to_a2a_format() が正しい形式を返すことを検証."""
        hub = get_hub()

        mock_agent = MagicMock()
        mock_agent.name = "TestAgent"
        mock_agent.run = MagicMock()

        hub.register(mock_agent)
        cards = hub.list_agents()
        formatted = cards[0].to_a2a_format()

        assert formatted["name"] == "TestAgent"
        assert "capabilities" in formatted
        assert "skills" in formatted
