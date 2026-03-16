"""GEO Platform agent Input/Output Pydantic models."""

from __future__ import annotations

from apps.Legacy_modernization_geo_platform.backend.intelligence import (
    IntelligenceSnapshot,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountSignalArtifact,
    BrandMemoryArtifact,
    CampaignReport,
    ContentBlueprintArtifact,
    ContentDraftArtifact,
    EvidenceMatrixArtifact,
    GeoExecuteRequest,
    GeoQAReport,
    LegacySemanticsArtifact,
    PublishManifest,
    QuestionGraphArtifact,
)
from pydantic import BaseModel, ConfigDict


# -- Supervisor -----------------------------------------------------------


class SupervisorInput(BaseModel):
    task_id: str
    request: GeoExecuteRequest


class SupervisorOutput(BaseModel):
    plan: list[str]
    approval_required: bool


# -- BrandMemory ----------------------------------------------------------


class BrandMemoryInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest


class BrandMemoryOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: BrandMemoryArtifact


# -- DemandSignal ----------------------------------------------------------


class DemandSignalInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    intelligence_snapshot: IntelligenceSnapshot


class DemandSignalOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: AccountSignalArtifact


# -- AccountScore ----------------------------------------------------------


class AccountScoreInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    signal_artifact: AccountSignalArtifact


class AccountScoreOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: AccountScoreArtifact


# -- QuestionGraph ---------------------------------------------------------


class QuestionGraphInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    score_artifact: AccountScoreArtifact


class QuestionGraphOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: QuestionGraphArtifact


# -- EvidenceMatrix --------------------------------------------------------


class EvidenceMatrixInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    question_graph: QuestionGraphArtifact
    intelligence_snapshot: IntelligenceSnapshot


class EvidenceMatrixOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: EvidenceMatrixArtifact


# -- LegacySemantics -------------------------------------------------------


class LegacySemanticsInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    brand_memory: BrandMemoryArtifact


class LegacySemanticsOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: LegacySemanticsArtifact


# -- ContentBlueprint ------------------------------------------------------


class ContentBlueprintInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    question_graph: QuestionGraphArtifact


class ContentBlueprintOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: ContentBlueprintArtifact


# -- ContentDraft ----------------------------------------------------------


class ContentDraftInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    blueprint: ContentBlueprintArtifact
    evidence_matrix: EvidenceMatrixArtifact
    legacy_semantics: LegacySemanticsArtifact


class ContentDraftOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: ContentDraftArtifact


# -- GeoQA ----------------------------------------------------------------


class GeoQAInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    draft: ContentDraftArtifact
    evidence_matrix: EvidenceMatrixArtifact


class GeoQAOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: GeoQAReport


# -- Publishing -----------------------------------------------------------


class PublishingInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    draft: ContentDraftArtifact


class PublishingOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: PublishManifest


# -- ReportAssembler ------------------------------------------------------


class ReportAssemblerInput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    task_id: str
    request: GeoExecuteRequest
    signal_artifact: AccountSignalArtifact
    qa_report: GeoQAReport
    publish_manifest: PublishManifest
    provider_status: dict[str, str]


class ReportAssemblerOutput(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)
    artifact: CampaignReport
    markdown: str
    summary: dict[str, object]
