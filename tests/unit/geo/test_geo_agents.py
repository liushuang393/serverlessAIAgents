"""GEO Platform ResilientAgent サブクラスの単体テスト."""

from __future__ import annotations

import pytest

from kernel.protocols.a2a_hub import get_hub, reset_hub
from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountScoreEntry,
    AccountSignalArtifact,
    ArtifactMeta,
    BrandMemoryArtifact,
    ContentBlueprintArtifact,
    ContentBlueprintPage,
    EvidenceMatrixArtifact,
    EvidenceMatrixEntry,
    ExecuteInputs,
    ExecuteTargets,
    FunnelCluster,
    GeoExecuteRequest,
    LegacySemanticsArtifact,
    PersonaQuestionSet,
    QuestionGraphArtifact,
    SignalEntry,
)


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


@pytest.fixture()
def geo_request():
    return GeoExecuteRequest(
        campaign_name="test-campaign",
        targets=ExecuteTargets(
            industries=["manufacturing"],
            legacy_stacks=["COBOL"],
            regions=["Japan"],
        ),
        inputs=ExecuteInputs(
            target_accounts=["TestCorp"],
        ),
    )


class TestBrandMemoryAgent:
    async def test_process_returns_artifact(self, geo_request):
        from apps.legacy_modernization_geo_platform.agents.brand_memory_agent import (
            BrandMemoryAgent,
        )
        from apps.legacy_modernization_geo_platform.agents._models import (
            BrandMemoryInput,
        )

        agent = BrandMemoryAgent()
        inp = BrandMemoryInput(task_id="t1", request=geo_request)
        result = await agent.process(inp)
        assert result.artifact.positioning
        assert result.artifact.meta.task_id == "t1"

    async def test_hub_registration_and_call(self, geo_request):
        from apps.legacy_modernization_geo_platform.agents.brand_memory_agent import (
            BrandMemoryAgent,
        )

        agent = BrandMemoryAgent()
        hub = get_hub()
        hub.register(agent)
        result = await hub.call(
            "BrandMemory", {"task_id": "t1", "request": geo_request}
        )
        assert "artifact" in result


class TestAccountScoreAgent:
    async def test_process_returns_artifact(self, geo_request):
        from apps.legacy_modernization_geo_platform.agents.account_score_agent import (
            AccountScoreAgent,
        )
        from apps.legacy_modernization_geo_platform.agents._models import (
            AccountScoreInput,
        )

        signal = AccountSignalArtifact(
            meta=ArtifactMeta(task_id="t1", trace_id="t", stage="s"),
            company="TestCorp",
            signals=[],
            urgency_hypothesis="保守要員不足",
            modernization_fit_score=85,
        )
        agent = AccountScoreAgent()
        inp = AccountScoreInput(
            task_id="t1", request=geo_request, signal_artifact=signal
        )
        result = await agent.process(inp)
        assert result.artifact.account_scores
        assert result.artifact.meta.task_id == "t1"


class TestLegacySemanticsAgent:
    async def test_process_returns_artifact(self, geo_request):
        from apps.legacy_modernization_geo_platform.agents.legacy_semantics_agent import (
            LegacySemanticsAgent,
        )
        from apps.legacy_modernization_geo_platform.agents._models import (
            LegacySemanticsInput,
        )

        brand = BrandMemoryArtifact(
            meta=ArtifactMeta(task_id="t1", trace_id="t", stage="s"),
            positioning="test",
            differentiators=[],
            supported_stacks=["COBOL"],
            target_regions=["Japan"],
        )
        agent = LegacySemanticsAgent()
        inp = LegacySemanticsInput(
            task_id="t1", request=geo_request, brand_memory=brand
        )
        result = await agent.process(inp)
        assert result.artifact.business_processes
        assert result.artifact.meta.task_id == "t1"
