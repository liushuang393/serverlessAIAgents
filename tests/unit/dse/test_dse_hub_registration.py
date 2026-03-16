"""DSE DesignAgentRegistry が Agent を A2AHub に登録することを検証."""
from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from agentflow.protocols.a2a_hub import get_hub, reset_hub


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


class TestDSEHubRegistration:
    def test_hub_registration_pattern(self):
        """Hub 登録パターンが動作することを検証."""
        hub = get_hub()
        mock_agent = MagicMock()
        mock_agent.name = "TestDSEAgent"
        mock_agent.run = MagicMock()
        hub.register(mock_agent)
        assert hub.discover("TestDSEAgent") is not None

    def test_discover_returns_none_for_unknown(self):
        """未登録 Agent は None を返すことを検証."""
        hub = get_hub()
        assert hub.discover("NonExistent") is None
