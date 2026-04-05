"""12 エージェント全ての単体テスト.

各 Agent の process() を直接呼び出し、出力の型・構造を検証する。
外部サービス（QualityGate, Publisher）はテスト用インスタンスを注入する。
"""

from __future__ import annotations

from datetime import UTC, datetime
from typing import TYPE_CHECKING

import pytest
from apps.legacy_modernization_geo_platform.agents._models import (
    AccountScoreInput,
    BrandMemoryInput,
    ContentBlueprintInput,
    ContentDraftInput,
    DemandSignalInput,
    EvidenceMatrixInput,
    GeoQAInput,
    LegacySemanticsInput,
    PublishingInput,
    QuestionGraphInput,
    ReportAssemblerInput,
    SupervisorInput,
)
from apps.legacy_modernization_geo_platform.agents.account_score_agent import AccountScoreAgent
from apps.legacy_modernization_geo_platform.agents.brand_memory_agent import BrandMemoryAgent
from apps.legacy_modernization_geo_platform.agents.content_blueprint_agent import ContentBlueprintAgent
from apps.legacy_modernization_geo_platform.agents.content_draft_agent import ContentDraftAgent
from apps.legacy_modernization_geo_platform.agents.demand_signal_agent import DemandSignalAgent
from apps.legacy_modernization_geo_platform.agents.evidence_matrix_agent import EvidenceMatrixAgent
from apps.legacy_modernization_geo_platform.agents.geo_qa_agent import GeoQAAgent
from apps.legacy_modernization_geo_platform.agents.legacy_semantics_agent import LegacySemanticsAgent
from apps.legacy_modernization_geo_platform.agents.publishing_agent import PublishingAgent
from apps.legacy_modernization_geo_platform.agents.question_graph_agent import QuestionGraphAgent
from apps.legacy_modernization_geo_platform.agents.report_assembler_agent import ReportAssemblerAgent
from apps.legacy_modernization_geo_platform.agents.supervisor_agent import SupervisorAgent
from apps.legacy_modernization_geo_platform.backend.intelligence import (
    IntelligenceSnapshot,
    NormalizedEvidenceSource,
)
from apps.legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate
from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountSignalArtifact,
    BrandMemoryArtifact,
    ContentBlueprintArtifact,
    ContentDraftArtifact,
    EvidenceMatrixArtifact,
    GeoExecuteRequest,
    GeoQAReport,
    LegacySemanticsArtifact,
    PublishManifest,
    QuestionGraphArtifact,
)
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings


if TYPE_CHECKING:
    from pathlib import Path



# ---------------------------------------------------------------------------
# 共通フィクスチャ
# ---------------------------------------------------------------------------

TASK_ID = "geo-test-001"


@pytest.fixture
def request_fixture() -> GeoExecuteRequest:
    return GeoExecuteRequest(
        campaign_name="unit-test-campaign",
        package="assessment",
        targets={
            "industries": ["manufacturing"],
            "legacy_stacks": ["COBOL", "Struts"],
            "regions": ["Japan"],
        },
    )


@pytest.fixture
def intelligence_fixture() -> IntelligenceSnapshot:
    now = datetime.now(UTC)
    sources = [
        NormalizedEvidenceSource(
            url=f"https://example.com/article-{i}",
            title=f"Legacy Modernization Article {i}",
            publisher="Tech Publisher",
            summary=f"記事 {i}: COBOL から Java への段階移行事例",
            snippet=f"段階移行のアプローチで年間コストを {20 + i * 5}% 削減",
            reliability="HIGH" if i < 3 else "MEDIUM",
            published_at=now,
            retrieved_at=now,
            provider="sample",
            tags=["modernization", "cobol"],
        )
        for i in range(5)
    ]
    return IntelligenceSnapshot(sources=sources, warnings=[], primary_provider="sample")


@pytest.fixture
async def brand_memory_artifact(request_fixture: GeoExecuteRequest) -> BrandMemoryArtifact:
    agent = BrandMemoryAgent()
    result = await agent.process(BrandMemoryInput(task_id=TASK_ID, request=request_fixture))
    return result.artifact


@pytest.fixture
async def signal_artifact(
    request_fixture: GeoExecuteRequest,
    intelligence_fixture: IntelligenceSnapshot,
) -> AccountSignalArtifact:
    agent = DemandSignalAgent()
    result = await agent.process(
        DemandSignalInput(task_id=TASK_ID, request=request_fixture, intelligence_snapshot=intelligence_fixture),
    )
    return result.artifact


@pytest.fixture
async def score_artifact(
    request_fixture: GeoExecuteRequest,
    signal_artifact: AccountSignalArtifact,
) -> AccountScoreArtifact:
    agent = AccountScoreAgent()
    result = await agent.process(
        AccountScoreInput(task_id=TASK_ID, request=request_fixture, signal_artifact=signal_artifact),
    )
    return result.artifact


@pytest.fixture
async def question_graph_artifact(
    request_fixture: GeoExecuteRequest,
    score_artifact: AccountScoreArtifact,
) -> QuestionGraphArtifact:
    agent = QuestionGraphAgent()
    result = await agent.process(
        QuestionGraphInput(task_id=TASK_ID, request=request_fixture, score_artifact=score_artifact),
    )
    return result.artifact


@pytest.fixture
async def evidence_matrix_artifact(
    question_graph_artifact: QuestionGraphArtifact,
    intelligence_fixture: IntelligenceSnapshot,
) -> EvidenceMatrixArtifact:
    agent = EvidenceMatrixAgent()
    result = await agent.process(
        EvidenceMatrixInput(
            task_id=TASK_ID,
            question_graph=question_graph_artifact,
            intelligence_snapshot=intelligence_fixture,
        ),
    )
    return result.artifact


@pytest.fixture
async def legacy_semantics_artifact(
    request_fixture: GeoExecuteRequest,
    brand_memory_artifact: BrandMemoryArtifact,
) -> LegacySemanticsArtifact:
    agent = LegacySemanticsAgent()
    result = await agent.process(
        LegacySemanticsInput(task_id=TASK_ID, request=request_fixture, brand_memory=brand_memory_artifact),
    )
    return result.artifact


@pytest.fixture
async def blueprint_artifact(
    request_fixture: GeoExecuteRequest,
    question_graph_artifact: QuestionGraphArtifact,
) -> ContentBlueprintArtifact:
    agent = ContentBlueprintAgent()
    result = await agent.process(
        ContentBlueprintInput(task_id=TASK_ID, request=request_fixture, question_graph=question_graph_artifact),
    )
    return result.artifact


@pytest.fixture
async def content_draft_artifact(
    request_fixture: GeoExecuteRequest,
    blueprint_artifact: ContentBlueprintArtifact,
    evidence_matrix_artifact: EvidenceMatrixArtifact,
    legacy_semantics_artifact: LegacySemanticsArtifact,
) -> ContentDraftArtifact:
    agent = ContentDraftAgent()
    result = await agent.process(
        ContentDraftInput(
            task_id=TASK_ID,
            request=request_fixture,
            blueprint=blueprint_artifact,
            evidence_matrix=evidence_matrix_artifact,
            legacy_semantics=legacy_semantics_artifact,
        ),
    )
    return result.artifact


@pytest.fixture
def geo_settings(tmp_path: Path) -> GeoPlatformSettings:
    from kernel.runtime import resolve_app_runtime as _resolve

    runtime = _resolve("apps/legacy_modernization_geo_platform/app_config.json")
    frontend_dist_dir = tmp_path / "frontend-dist"
    frontend_dist_dir.mkdir(parents=True, exist_ok=True)
    (frontend_dist_dir / "index.html").write_text("<html></html>", encoding="utf-8")
    return GeoPlatformSettings(
        app_root=tmp_path,
        host="127.0.0.1",
        port=runtime.ports.api or 8093,
        api_key="",
        cors_origins=["http://localhost:3093"],
        db_path=tmp_path / "data" / "geo.db",
        artifacts_dir=tmp_path / "data" / "artifacts",
        reports_dir=tmp_path / "data" / "reports",
        published_dir=tmp_path / "data" / "published",
        public_base_url=runtime.urls.backend or "http://localhost:8093",
        frontend_dist_dir=frontend_dist_dir,
    )


# ---------------------------------------------------------------------------
# 1. SupervisorAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_supervisor_returns_plan(request_fixture: GeoExecuteRequest) -> None:
    agent = SupervisorAgent()
    result = await agent.process(SupervisorInput(task_id=TASK_ID, request=request_fixture))
    assert isinstance(result.plan, list)
    assert len(result.plan) > 0
    assert isinstance(result.approval_required, bool)


# ---------------------------------------------------------------------------
# 2. BrandMemoryAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_brand_memory_produces_artifact(request_fixture: GeoExecuteRequest) -> None:
    agent = BrandMemoryAgent()
    result = await agent.process(BrandMemoryInput(task_id=TASK_ID, request=request_fixture))
    artifact = result.artifact
    assert isinstance(artifact, BrandMemoryArtifact)
    assert artifact.meta.task_id == TASK_ID
    assert len(artifact.supported_stacks) >= 1
    assert len(artifact.differentiators) >= 1


@pytest.mark.asyncio
async def test_brand_memory_parse_input() -> None:
    agent = BrandMemoryAgent()
    parsed = agent._parse_input(
        {"task_id": "t1", "request": {"campaign_name": "demo"}},
    )
    assert isinstance(parsed, BrandMemoryInput)
    assert parsed.task_id == "t1"


# ---------------------------------------------------------------------------
# 3. DemandSignalAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_demand_signal_produces_signals(
    request_fixture: GeoExecuteRequest,
    intelligence_fixture: IntelligenceSnapshot,
) -> None:
    agent = DemandSignalAgent()
    result = await agent.process(
        DemandSignalInput(task_id=TASK_ID, request=request_fixture, intelligence_snapshot=intelligence_fixture),
    )
    artifact = result.artifact
    assert isinstance(artifact, AccountSignalArtifact)
    assert len(artifact.signals) > 0
    assert artifact.modernization_fit_score > 0


# ---------------------------------------------------------------------------
# 4. AccountScoreAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_account_score_produces_scores(
    request_fixture: GeoExecuteRequest,
    signal_artifact: AccountSignalArtifact,
) -> None:
    agent = AccountScoreAgent()
    result = await agent.process(
        AccountScoreInput(task_id=TASK_ID, request=request_fixture, signal_artifact=signal_artifact),
    )
    artifact = result.artifact
    assert isinstance(artifact, AccountScoreArtifact)
    assert len(artifact.account_scores) >= 1
    score = artifact.account_scores[0]
    assert 0 <= score.fit_score <= 100
    assert score.priority in {"high", "medium", "low"}


# ---------------------------------------------------------------------------
# 5. QuestionGraphAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_question_graph_produces_personas(
    request_fixture: GeoExecuteRequest,
    score_artifact: AccountScoreArtifact,
) -> None:
    agent = QuestionGraphAgent()
    result = await agent.process(
        QuestionGraphInput(task_id=TASK_ID, request=request_fixture, score_artifact=score_artifact),
    )
    artifact = result.artifact
    assert isinstance(artifact, QuestionGraphArtifact)
    assert len(artifact.personas) >= 1
    assert len(artifact.funnel_clusters) >= 1


# ---------------------------------------------------------------------------
# 6. EvidenceMatrixAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_evidence_matrix_produces_entries(
    question_graph_artifact: QuestionGraphArtifact,
    intelligence_fixture: IntelligenceSnapshot,
) -> None:
    agent = EvidenceMatrixAgent()
    result = await agent.process(
        EvidenceMatrixInput(
            task_id=TASK_ID,
            question_graph=question_graph_artifact,
            intelligence_snapshot=intelligence_fixture,
        ),
    )
    artifact = result.artifact
    assert isinstance(artifact, EvidenceMatrixArtifact)
    assert len(artifact.entries) > 0


# ---------------------------------------------------------------------------
# 7. LegacySemanticsAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_legacy_semantics_produces_business_model(
    request_fixture: GeoExecuteRequest,
    brand_memory_artifact: BrandMemoryArtifact,
) -> None:
    agent = LegacySemanticsAgent()
    result = await agent.process(
        LegacySemanticsInput(task_id=TASK_ID, request=request_fixture, brand_memory=brand_memory_artifact),
    )
    artifact = result.artifact
    assert isinstance(artifact, LegacySemanticsArtifact)
    assert len(artifact.business_processes) >= 1
    assert artifact.state_model is not None


# ---------------------------------------------------------------------------
# 8. ContentBlueprintAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_content_blueprint_produces_pages(
    request_fixture: GeoExecuteRequest,
    question_graph_artifact: QuestionGraphArtifact,
) -> None:
    agent = ContentBlueprintAgent()
    result = await agent.process(
        ContentBlueprintInput(task_id=TASK_ID, request=request_fixture, question_graph=question_graph_artifact),
    )
    artifact = result.artifact
    assert isinstance(artifact, ContentBlueprintArtifact)
    assert len(artifact.pages) >= 1
    assert artifact.target_language in {"ja", "en", "zh"}


# ---------------------------------------------------------------------------
# 9. ContentDraftAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_content_draft_produces_publishable_pages(
    request_fixture: GeoExecuteRequest,
    blueprint_artifact: ContentBlueprintArtifact,
    evidence_matrix_artifact: EvidenceMatrixArtifact,
    legacy_semantics_artifact: LegacySemanticsArtifact,
) -> None:
    agent = ContentDraftAgent()
    result = await agent.process(
        ContentDraftInput(
            task_id=TASK_ID,
            request=request_fixture,
            blueprint=blueprint_artifact,
            evidence_matrix=evidence_matrix_artifact,
            legacy_semantics=legacy_semantics_artifact,
        ),
    )
    artifact = result.artifact
    assert isinstance(artifact, ContentDraftArtifact)
    assert len(artifact.pages) >= 1
    page = artifact.pages[0]
    assert page.slug
    assert page.body_markdown


# ---------------------------------------------------------------------------
# 10. GeoQAAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_geo_qa_evaluates_draft(
    content_draft_artifact: ContentDraftArtifact,
    evidence_matrix_artifact: EvidenceMatrixArtifact,
) -> None:
    gate = GeoQualityGate()
    agent = GeoQAAgent(quality_gate=gate)
    result = await agent.process(
        GeoQAInput(task_id=TASK_ID, draft=content_draft_artifact, evidence_matrix=evidence_matrix_artifact),
    )
    assert isinstance(result.artifact, GeoQAReport)
    assert result.artifact.risk_level in {"LOW", "MEDIUM", "HIGH"}


# ---------------------------------------------------------------------------
# 11. PublishingAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_publishing_generates_manifest(
    content_draft_artifact: ContentDraftArtifact,
    geo_settings: GeoPlatformSettings,
) -> None:
    publisher = GeoPublisher(geo_settings)
    agent = PublishingAgent(publisher=publisher)
    result = await agent.process(PublishingInput(task_id=TASK_ID, draft=content_draft_artifact))
    assert isinstance(result.artifact, PublishManifest)
    assert len(result.artifact.pages) >= 1


# ---------------------------------------------------------------------------
# 12. ReportAssemblerAgent
# ---------------------------------------------------------------------------


@pytest.mark.asyncio
async def test_report_assembler_builds_report(
    request_fixture: GeoExecuteRequest,
    signal_artifact: AccountSignalArtifact,
    content_draft_artifact: ContentDraftArtifact,
    evidence_matrix_artifact: EvidenceMatrixArtifact,
    geo_settings: GeoPlatformSettings,
) -> None:
    gate = GeoQualityGate()
    qa_report = gate.evaluate(
        task_id=TASK_ID,
        draft=content_draft_artifact,
        evidence_matrix=evidence_matrix_artifact,
    )
    publisher = GeoPublisher(geo_settings)
    manifest = publisher.publish(TASK_ID, content_draft_artifact)

    agent = ReportAssemblerAgent()
    result = await agent.process(
        ReportAssemblerInput(
            task_id=TASK_ID,
            request=request_fixture,
            signal_artifact=signal_artifact,
            qa_report=qa_report,
            publish_manifest=manifest,
            provider_status={"primary_provider": "sample"},
        ),
    )
    assert result.markdown
    assert result.summary
    assert result.artifact is not None
