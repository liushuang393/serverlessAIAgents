"""Core types for Evolution V2."""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field, field_validator


class ScopeLevel(str, Enum):
    """Scope levels for strategy inheritance."""

    TENANT_APP = "tenant_app"
    TENANT_PRODUCT_LINE = "tenant_product_line"
    GLOBAL_VERIFIED = "global_verified"


class StrategyStatus(str, Enum):
    """Lifecycle states for a strategy."""

    CANDIDATE = "candidate"
    VERIFIED = "verified"
    SUSPECT = "suspect"
    DEPRECATED = "deprecated"


class RetrievalMode(str, Enum):
    """Retrieval modes used by gate and router."""

    SKIP = "skip"
    LIGHT = "light_retrieval"
    DEEP = "deep_retrieval"


class StalenessRisk(str, Enum):
    """Staleness risk label."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"


class StrategyValidity(BaseModel):
    """Validity window and age policy."""

    valid_from: datetime | None = None
    valid_to: datetime | None = None
    max_age_days: int = Field(default=30, ge=1, le=3650)


class StrategyScope(BaseModel):
    """Scope key used by registry."""

    scope_level: ScopeLevel = ScopeLevel.TENANT_APP
    tenant_id: str | None = None
    app_id: str | None = None
    product_line: str | None = None


class StrategyCapsule(BaseModel):
    """Strategy capsule extracted from successful executions."""

    intent: str = Field(min_length=1)
    environment_fingerprint: dict[str, Any] = Field(default_factory=dict)
    tool_sequence: list[str] = Field(default_factory=list)
    guard_conditions: list[str] = Field(default_factory=list)
    expected_output: str = ""
    validation_method: str = "replay+tests+policy_checks"
    keywords: list[str] = Field(default_factory=list)
    scope: StrategyScope = Field(default_factory=StrategyScope)
    validity: StrategyValidity = Field(default_factory=StrategyValidity)


class StrategyScoreSnapshot(BaseModel):
    """Score snapshot used for ranking and health checks."""

    success_rate_7d: float = Field(default=0.0, ge=0.0, le=1.0)
    success_rate_30d: float = Field(default=0.0, ge=0.0, le=1.0)
    reuse_count_30d: int = Field(default=0, ge=0)
    avg_latency_ms: float = Field(default=0.0, ge=0.0)
    freshness_decay: float = Field(default=1.0, ge=0.0, le=1.0)
    suspicion_level: float = Field(default=0.0, ge=0.0, le=1.0)
    condition_match: float = Field(default=1.0, ge=0.0, le=1.0)
    cost_efficiency: float = Field(default=0.5, ge=0.0, le=1.0)
    final_score: float = Field(default=0.0)
    sample_count: int = Field(default=0, ge=0)
    updated_at: datetime = Field(default_factory=lambda: datetime.now(UTC))


class StrategyRecord(BaseModel):
    """Stored strategy metadata."""

    id: str
    tenant_id: str | None = None
    app_id: str | None = None
    product_line: str | None = None
    scope_level: ScopeLevel
    intent_hash: str
    capsule: StrategyCapsule
    status: StrategyStatus = StrategyStatus.CANDIDATE
    version: int = Field(default=1, ge=1)
    parent_strategy_id: str | None = None
    valid_from: datetime | None = None
    valid_to: datetime | None = None
    last_verified_at: datetime | None = None
    created_at: datetime = Field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime = Field(default_factory=lambda: datetime.now(UTC))
    score: StrategyScoreSnapshot = Field(default_factory=StrategyScoreSnapshot)


class StrategySearchRequest(BaseModel):
    """Search request sent to registry/service."""

    tenant_id: str | None = None
    app_id: str | None = None
    product_line: str | None = None
    intent: str = Field(min_length=1)
    task_signature: str | None = None
    decision_mode: RetrievalMode = RetrievalMode.LIGHT
    top_k: int = Field(default=5, ge=1, le=20)


class StrategySearchResultItem(BaseModel):
    """Search candidate item."""

    strategy_id: str
    scope_level: ScopeLevel
    status: StrategyStatus
    score: float
    reasons: list[str] = Field(default_factory=list)
    capsule: StrategyCapsule
    last_verified_at: datetime | None = None


class StrategySearchResponse(BaseModel):
    """Search response payload."""

    decision: str
    reasons: list[str] = Field(default_factory=list)
    strategies: list[StrategySearchResultItem] = Field(default_factory=list)
    score_breakdown: dict[str, Any] = Field(default_factory=dict)


class RetrievalDecisionInput(BaseModel):
    """Input for RetrievalGate V2."""

    query: str = Field(min_length=1)
    context: dict[str, Any] = Field(default_factory=dict)
    complexity: float = Field(default=0.5, ge=0.0, le=1.0)
    self_confidence: float = Field(default=0.5, ge=0.0, le=1.0)
    novelty: float = Field(default=0.5, ge=0.0, le=1.0)
    recent_failures: int = Field(default=0, ge=0)
    explicit_request: bool = False
    staleness_risk: StalenessRisk = StalenessRisk.MEDIUM
    tenant_id: str | None = None
    app_id: str | None = None
    product_line: str | None = None
    task_signature: str | None = None


class RetrievalDecision(BaseModel):
    """Decision from RetrievalGate V2."""

    should_retrieve: bool
    mode: RetrievalMode
    confidence: float = Field(ge=0.0, le=1.0)
    reason_codes: list[str] = Field(default_factory=list)
    suggested_query: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)


class StrategyDecision(BaseModel):
    """Router decision returned to orchestrator."""

    use_strategy: bool
    mode: RetrievalMode
    reasons: list[str] = Field(default_factory=list)
    selected_strategy_id: str | None = None
    selected_capsule: StrategyCapsule | None = None
    search_response: StrategySearchResponse | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)


class ExecutionEvent(BaseModel):
    """Execution event emitted by recorder hooks."""

    run_id: str
    step_id: str | None = None
    event_type: str
    payload: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime = Field(default_factory=lambda: datetime.now(UTC))


class OutcomeRecord(BaseModel):
    """Outcome facts used for extractor/scoring."""

    run_id: str
    strategy_id: str | None = None
    task_signature_hash: str | None = None
    difficulty: float = Field(default=0.5, ge=0.0, le=1.0)
    self_confidence: float = Field(default=0.5, ge=0.0, le=1.0)
    used_retrieval: bool = False
    success: bool = True
    failure_reason: str | None = None
    latency_ms: float = Field(default=0.0, ge=0.0)
    token_in: int = Field(default=0, ge=0)
    token_out: int = Field(default=0, ge=0)
    cost_usd: float = Field(default=0.0, ge=0.0)
    created_at: datetime = Field(default_factory=lambda: datetime.now(UTC))


class ValidationEvent(BaseModel):
    """Validator result payload."""

    strategy_id: str
    job_id: str
    checks: dict[str, Any] = Field(default_factory=dict)
    status: str
    score_delta: float = 0.0
    created_at: datetime = Field(default_factory=lambda: datetime.now(UTC))


class ScoreUpdateRequest(BaseModel):
    """Score update event."""

    strategy_id: str
    success: bool
    latency_ms: float = Field(default=0.0, ge=0.0)
    cost_efficiency: float = Field(default=0.5, ge=0.0, le=1.0)
    condition_match: float = Field(default=1.0, ge=0.0, le=1.0)
    failure_streak: int = Field(default=0, ge=0)
    environment_mismatch: bool = False


class RetrievalDecisionContract(BaseModel):
    """Contract-aligned retrieval decision payload."""

    mode: RetrievalMode
    confidence: float
    complexity: float
    novelty: float
    staleness_risk: StalenessRisk
    reason_codes: list[str] = Field(default_factory=list)

    @field_validator("confidence")
    @classmethod
    def _validate_confidence(cls, value: float) -> float:
        return max(0.0, min(1.0, value))
