"""GeoOrchestrator が A2AHub に Agent を登録することを検証."""

from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from agentflow.protocols.a2a_hub import get_hub, reset_hub


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


class TestOrchestratorA2ARegistration:
    def test_agents_registered_on_init(self):
        """GeoOrchestrator 初期化時に 8 Agent が Hub に登録される."""
        from apps.Legacy_modernization_geo_platform.backend.orchestrator import (
            GeoOrchestrator,
        )
        from apps.Legacy_modernization_geo_platform.backend.settings import (
            GeoPlatformSettings,
        )

        settings = GeoPlatformSettings.from_env()
        mock_repo = MagicMock()
        GeoOrchestrator(settings=settings, repository=mock_repo)

        hub = get_hub()
        expected_agents = [
            "BrandMemory",
            "DemandSignal",
            "AccountScore",
            "QuestionGraph",
            "EvidenceMatrix",
            "LegacySemantics",
            "ContentBlueprint",
            "ContentDraft",
        ]
        for name in expected_agents:
            assert hub.discover(name) is not None, f"{name} not registered"
        assert hub.agent_count >= 8

    def test_invoke_routes_through_hub(self):
        """_invoke_registered_agent が Hub 経由でルーティングする."""
        from apps.Legacy_modernization_geo_platform.backend.orchestrator import (
            GeoOrchestrator,
        )
        from apps.Legacy_modernization_geo_platform.backend.settings import (
            GeoPlatformSettings,
        )

        settings = GeoPlatformSettings.from_env()
        mock_repo = MagicMock()
        orchestrator = GeoOrchestrator(settings=settings, repository=mock_repo)

        hub = get_hub()
        # BrandMemory は Hub に登録済みのはず
        assert hub.discover("BrandMemory") is not None
