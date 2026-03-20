"""Schemas for the Legacy Modernization GEO Platform backend."""

from __future__ import annotations

from datetime import datetime, timezone
from enum import Enum
from typing import Any

from contracts.policy import ApprovalRequest as ContractApprovalRequest
from pydantic import BaseModel, Field
from pydantic import field_validator
from pydantic import model_validator

from kernel.skills.gateway import RiskLevel


SUPPORTED_CONTENT_LANGUAGES: tuple[str, ...] = ("ja", "en", "zh")
_SCHEMA_LANGUAGE_BY_CONTENT_LANGUAGE: dict[str, str] = {
    "ja": "ja-JP",
    "en": "en-US",
    "zh": "zh-CN",
}
_HTML_LANGUAGE_BY_CONTENT_LANGUAGE: dict[str, str] = {
    "ja": "ja",
    "en": "en",
    "zh": "zh-CN",
}


def normalize_content_language(language: str | None) -> str:
    """Normalize arbitrary locale inputs to a supported content language code."""
    if language is None:
        return "ja"
    normalized = language.strip().lower()
    if normalized.startswith("ja"):
        return "ja"
    if normalized.startswith("en"):
        return "en"
    if normalized.startswith("zh"):
        return "zh"
    return "ja"


def schema_language_code(language: str | None) -> str:
    """Resolve JSON-LD language tag from a normalized content language code."""
    normalized = normalize_content_language(language)
    return _SCHEMA_LANGUAGE_BY_CONTENT_LANGUAGE[normalized]


def html_language_code(language: str | None) -> str:
    """Resolve `<html lang>` code from a normalized content language code."""
    normalized = normalize_content_language(language)
    return _HTML_LANGUAGE_BY_CONTENT_LANGUAGE[normalized]


def _utcnow() -> datetime:
    """Return a timezone-aware UTC timestamp."""
    return datetime.now(timezone.utc)


class TaskStatus(str, Enum):
    """High-level task lifecycle states."""

    QUEUED = "queued"
    RUNNING = "running"
    WAITING_APPROVAL = "waiting_approval"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class ApprovalStatus(str, Enum):
    """Supported approval outcomes."""

    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    REWRITE = "rewrite"


class ExecuteTargets(BaseModel):
    """Target market segmentation."""

    industries: list[str] = Field(default_factory=list)
    legacy_stacks: list[str] = Field(default_factory=list)
    regions: list[str] = Field(default_factory=list)


class ExecuteInputs(BaseModel):
    """Optional richer execution inputs."""

    target_accounts: list[str] = Field(default_factory=list)
    target_services: list[str] = Field(default_factory=list)
    regions: list[str] = Field(default_factory=list)
    content_languages: list[str] = Field(default_factory=lambda: ["ja"])
    channels: list[str] = Field(default_factory=lambda: ["website", "ai_search"])
    conversion_goal: str = "diagnostic_request"

    @field_validator("content_languages", mode="before")
    @classmethod
    def _normalize_content_languages(cls, value: object) -> list[str]:
        """Normalize locale values and ensure at least one supported language."""
        candidates: list[str]
        if value is None:
            candidates = []
        elif isinstance(value, str):
            candidates = [value]
        elif isinstance(value, list):
            candidates = [str(item) for item in value]
        else:
            candidates = [str(value)]

        normalized: list[str] = []
        for candidate in candidates:
            language = normalize_content_language(candidate)
            if language not in normalized:
                normalized.append(language)

        return normalized or ["ja"]


class ExecuteOptions(BaseModel):
    """Execution behavior switches."""

    skill_mode: str = "skill_first"
    human_policy: str = "risk_based"
    acceptance_threshold: float = 85.0
    max_auto_iterations: int = 3


class ExecuteConstraints(BaseModel):
    """Execution safety constraints."""

    must_cite_sources: bool = True
    human_approval_before_publish: bool = True
    avoid_scaled_low_value_content: bool = True


class GeoExecuteRequest(BaseModel):
    """Root execution request."""

    campaign_name: str
    package: str = "assessment"
    targets: ExecuteTargets = Field(default_factory=ExecuteTargets)
    inputs: ExecuteInputs = Field(default_factory=ExecuteInputs)
    options: ExecuteOptions = Field(default_factory=ExecuteOptions)
    constraints: ExecuteConstraints = Field(default_factory=ExecuteConstraints)


class TaskCommandRequest(BaseModel):
    """Command payload from the operator UI."""

    command: str
    actor: str = "operator"
    comment: str | None = None
    payload: dict[str, Any] = Field(default_factory=dict)


class ApprovalRequest(ContractApprovalRequest):
    """Approval request payload.

    Keeps the legacy GEO surface while reusing the canonical approval contract.
    """

    approved: bool = True
    reviewer_name: str = "operator"
    comment: str | None = None
    action: ApprovalStatus | None = None

    @model_validator(mode="before")
    @classmethod
    def _normalize_legacy_payload(cls, value: object) -> object:
        """Map legacy GEO fields to the canonical approval contract."""
        if not isinstance(value, dict):
            return value

        payload = dict(value)
        approved = bool(payload.get("approved", True))
        reviewer_name = str(payload.get("reviewer_name", "operator")).strip() or "operator"
        comment = payload.get("comment")
        comment_text = str(comment).strip() if comment is not None else ""

        action_raw = payload.get("action")
        if action_raw is None:
            action = ApprovalStatus.APPROVED if approved else ApprovalStatus.REJECTED
        elif isinstance(action_raw, ApprovalStatus):
            action = action_raw
        else:
            action = ApprovalStatus(str(action_raw))

        payload["approved"] = approved
        payload["reviewer_name"] = reviewer_name
        payload["comment"] = comment_text or None
        payload["action"] = action
        payload["requester"] = str(payload.get("requester") or reviewer_name).strip() or reviewer_name
        payload["priority"] = str(payload.get("priority") or _approval_priority(action)).strip() or _approval_priority(
            action
        )
        payload["reason"] = str(payload.get("reason") or comment_text or _approval_reason(action, reviewer_name)).strip()
        if not payload["reason"]:
            payload["reason"] = _approval_reason(action, reviewer_name)
        existing_context = payload.get("context")
        context = existing_context if isinstance(existing_context, dict) else {}
        payload["context"] = {
            **context,
            "approved": approved,
            "reviewer_name": reviewer_name,
            "comment": comment_text or None,
            "action": action.value,
            "risk_level": _approval_risk_level(action).value,
        }
        return payload

    def to_dict(self) -> dict[str, Any]:
        """Serialize to the legacy GEO approval shape."""
        return {
            **self.model_dump(mode="json"),
            "approved": self.approved,
            "reviewer_name": self.reviewer_name,
            "comment": self.comment,
            "action": self.action.value if self.action is not None else None,
        }


class ArtifactMeta(BaseModel):
    """Shared artifact metadata envelope."""

    task_id: str
    trace_id: str
    stage: str
    generated_at: datetime = Field(default_factory=_utcnow)


class ArtifactBaseModel(BaseModel):
    """Base artifact shape used across the pipeline."""

    meta: ArtifactMeta
    evidence: list[dict[str, Any]] = Field(default_factory=list)
    unknowns: list[str] = Field(default_factory=list)
    extensions: dict[str, Any] = Field(default_factory=dict)


class BrandMemoryArtifact(ArtifactBaseModel):
    """Codified positioning and internal brand constraints."""

    positioning: str
    differentiators: list[str] = Field(default_factory=list)
    supported_stacks: list[str] = Field(default_factory=list)
    target_regions: list[str] = Field(default_factory=list)


class SignalEntry(BaseModel):
    """Normalized account signal entry."""

    type: str
    description: str
    source: str
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)


class AccountSignalArtifact(ArtifactBaseModel):
    """Demand signal artifact."""

    company: str
    signals: list[SignalEntry] = Field(default_factory=list)
    urgency_hypothesis: str = ""
    modernization_fit_score: int = 0


class AccountScoreEntry(BaseModel):
    """Scored account row."""

    company: str
    fit_score: int
    urgency_score: int
    priority: str
    rationale: list[str] = Field(default_factory=list)


class AccountScoreArtifact(ArtifactBaseModel):
    """Scoring output for accounts and themes."""

    account_scores: list[AccountScoreEntry] = Field(default_factory=list)
    recommended_focus: list[str] = Field(default_factory=list)


class PersonaQuestionSet(BaseModel):
    """Persona-based question cluster."""

    role: str
    questions: list[str] = Field(default_factory=list)
    high_intent_questions: list[str] = Field(default_factory=list)


class FunnelCluster(BaseModel):
    """Funnel-stage grouping."""

    stage: str
    questions: list[str] = Field(default_factory=list)
    recommended_page_type: str


class QuestionGraphArtifact(ArtifactBaseModel):
    """Buyer question map."""

    personas: list[PersonaQuestionSet] = Field(default_factory=list)
    funnel_clusters: list[FunnelCluster] = Field(default_factory=list)
    content_clusters: list[dict[str, Any]] = Field(default_factory=list)


class LegacySemanticsArtifact(ArtifactBaseModel):
    """Business semantics artifact."""

    business_processes: list[str] = Field(default_factory=list)
    business_events: list[str] = Field(default_factory=list)
    state_model: dict[str, Any] = Field(default_factory=dict)
    business_rules: list[str] = Field(default_factory=list)


class EvidenceMatrixEntry(BaseModel):
    """Evidence row used for GEO QA and content authoring."""

    claim: str
    question_ref: str
    source_url: str
    title: str
    publisher: str
    summary: str
    snippet: str
    reliability: str
    citation_ready: bool
    fresh: bool


class EvidenceMatrixArtifact(ArtifactBaseModel):
    """Evidence matrix artifact."""

    entries: list[EvidenceMatrixEntry] = Field(default_factory=list)
    provider_status: dict[str, Any] = Field(default_factory=dict)


class ContentBlueprintPage(BaseModel):
    """Planned page blueprint."""

    slug: str
    title: str
    persona: str
    primary_question: str
    page_type: str
    cta: str


class ContentBlueprintArtifact(ArtifactBaseModel):
    """Content strategy blueprint."""

    pages: list[ContentBlueprintPage] = Field(default_factory=list)
    target_language: str = "ja"


class FAQEntry(BaseModel):
    """FAQ pair."""

    question: str
    answer: str


class ContentDraftPage(BaseModel):
    """Generated content page draft."""

    slug: str
    title: str
    summary: str
    body_markdown: str
    cta: str
    faq_entries: list[FAQEntry] = Field(default_factory=list)
    json_ld: dict[str, Any] = Field(default_factory=dict)


class ContentDraftArtifact(ArtifactBaseModel):
    """Draft content bundle."""

    pages: list[ContentDraftPage] = Field(default_factory=list)
    target_language: str = "ja"


class GeoQAReport(ArtifactBaseModel):
    """GEO QA output."""

    pass_or_fail: str = "PASS"
    issues: list[str] = Field(default_factory=list)
    fix_instructions: list[str] = Field(default_factory=list)
    publish_ready: bool = True
    risk_level: str = "LOW"
    metrics: dict[str, float] = Field(default_factory=dict)


class PublishedPageRecord(BaseModel):
    """Published static page record."""

    slug: str
    title: str
    page_url: str
    html_path: str


class PublishManifest(ArtifactBaseModel):
    """Publish output manifest."""

    pages: list[PublishedPageRecord] = Field(default_factory=list)
    sitemap_url: str = ""
    ai_feed_url: str = ""
    published_at: datetime = Field(default_factory=_utcnow)


class CampaignReport(ArtifactBaseModel):
    """Final execution report artifact."""

    summary: str
    highlights: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    published_urls: list[str] = Field(default_factory=list)


class TaskEvent(BaseModel):
    """WebSocket and audit event payload."""

    event_type: str
    timestamp: datetime = Field(default_factory=_utcnow)
    task_id: str
    stage: str | None = None
    agent: str | None = None
    message: str | None = None
    payload: dict[str, Any] = Field(default_factory=dict)


class ArtifactRecord(BaseModel):
    """Artifact index row returned by the API."""

    artifact_name: str
    stage: str
    path: str
    summary: str
    created_at: datetime


class ApprovalRecord(BaseModel):
    """Approval index row returned by the API."""

    request_id: str
    task_id: str
    stage: str
    object_id: str
    risk_level: str
    reason: str
    status: ApprovalStatus
    actions: list[str] = Field(default_factory=list)
    comment: str | None = None
    reviewer_name: str | None = None
    created_at: datetime
    updated_at: datetime


class ReportPayload(BaseModel):
    """Structured report response."""

    task_id: str
    markdown: str
    summary: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime
    updated_at: datetime


class TaskStateResponse(BaseModel):
    """Combined task state payload."""

    task_id: str
    status: TaskStatus
    current_stage: str | None = None
    campaign_name: str
    package: str
    request: GeoExecuteRequest
    events: list[TaskEvent] = Field(default_factory=list)
    artifacts: list[ArtifactRecord] = Field(default_factory=list)
    approvals: list[ApprovalRecord] = Field(default_factory=list)
    report: ReportPayload | None = None
    published_pages: list[PublishedPageRecord] = Field(default_factory=list)
    error_message: str | None = None


def _approval_priority(action: ApprovalStatus) -> str:
    """Map approval outcome to a canonical priority string."""
    if action == ApprovalStatus.REJECTED:
        return "high"
    if action == ApprovalStatus.REWRITE:
        return "normal"
    return "normal"


def _approval_reason(action: ApprovalStatus, reviewer_name: str) -> str:
    """Build a stable default reason for legacy approval requests."""
    return f"Approval decision {action.value} by {reviewer_name}"


def _approval_risk_level(action: ApprovalStatus) -> RiskLevel:
    """Map approval outcome to a risk level used by legacy GEO reporting."""
    if action == ApprovalStatus.REJECTED:
        return RiskLevel.HIGH
    if action == ApprovalStatus.REWRITE:
        return RiskLevel.MEDIUM
    return RiskLevel.LOW
