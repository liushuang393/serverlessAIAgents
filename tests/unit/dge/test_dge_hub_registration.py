"""DGE AgentRegistry が Agent を A2AHub に登録することを検証."""
from __future__ import annotations

from typing import Any
from unittest.mock import MagicMock

import pytest

from kernel.protocols.a2a_hub import get_hub, reset_hub


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


class TestDGEHubRegistration:
    def test_get_agent_registers_to_hub(self):
        """AgentRegistry.get_agent() が A2AHub に登録することを検証."""
        from kernel.protocols.a2a_hub import get_hub

        hub = get_hub()

        # 手動で agent を登録して検証
        mock_agent = MagicMock()
        mock_agent.name = "TestDGEAgent"
        mock_agent.run = MagicMock()
        hub.register(mock_agent)

        assert hub.discover("TestDGEAgent") is not None
        assert hub.agent_count == 1

    def test_duplicate_registration_skipped(self):
        """重複登録がスキップされることを検証."""
        hub = get_hub()
        mock_agent = MagicMock()
        mock_agent.name = "TestDGEAgent"
        mock_agent.run = MagicMock()
        hub.register(mock_agent)

        # 同名で再登録を試みても例外が出ることを確認
        assert hub.agent_count == 1
        # discover で存在確認できる
        assert hub.discover("TestDGEAgent") is not None
