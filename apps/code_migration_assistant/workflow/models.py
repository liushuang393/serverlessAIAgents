"""移行ワークフローの契約モデル.

工程間の受け渡しを JSON Schema 相当の Pydantic モデルで定義する。
全成果物は `meta / unknowns / extensions` を必須とする。
"""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ArtifactMeta(BaseModel):
    """成果物メタ情報."""

    task_id: str
    trace_id: str
    stage: str
    generated_at: str
    source_language: str | None = None
    target_language: str | None = None
    module: str | None = None


class UnknownItem(BaseModel):
    """不明点の明示情報."""

    field: str
    reason: str


class ArtifactBase(BaseModel):
    """全成果物の共通基底."""

    meta: ArtifactMeta
    unknowns: list[UnknownItem] = Field(default_factory=list)
    extensions: dict[str, Any] = Field(default_factory=dict)


class LegacyAnalysisArtifact(ArtifactBase):
    """分析工程の成果物."""

    programs: list[dict[str, Any]] = Field(default_factory=list)
    entry_points: list[dict[str, Any]] = Field(default_factory=list)
    io_contracts: list[dict[str, Any]] = Field(default_factory=list)
    data_structures: list[dict[str, Any]] = Field(default_factory=list)
    control_flow: list[dict[str, Any]] = Field(default_factory=list)
    db_access: list[dict[str, Any]] = Field(default_factory=list)
    external_calls: list[dict[str, Any]] = Field(default_factory=list)


class MigrationDesignArtifact(ArtifactBase):
    """設計工程の成果物."""

    package_mapping: dict[str, str] = Field(default_factory=dict)
    class_mapping: dict[str, str] = Field(default_factory=dict)
    transaction_policy: dict[str, Any] = Field(default_factory=dict)
    state_model: dict[str, Any] = Field(default_factory=dict)
    framework_mapping: dict[str, Any] = Field(default_factory=dict)
    rationale: dict[str, str] = Field(default_factory=dict)


class BusinessSemanticsArtifact(ArtifactBase):
    """業務語義工程の成果物."""

    business_processes: list[dict[str, Any]] = Field(default_factory=list)
    business_events: list[dict[str, Any]] = Field(default_factory=list)
    state_model: dict[str, Any] = Field(default_factory=dict)
    business_rules: list[dict[str, Any]] = Field(default_factory=list)


class GeneratedFile(BaseModel):
    """生成ファイル情報."""

    path: str
    content: str


class TransformationArtifact(ArtifactBase):
    """変換工程の成果物."""

    target_code: str
    generated_files: list[GeneratedFile] = Field(default_factory=list)
    rule_hits: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)


class TestSynthesisArtifact(ArtifactBase):
    """テスト生成工程の成果物."""

    test_cases: list[dict[str, Any]] = Field(default_factory=list)
    golden_master: dict[str, Any] = Field(default_factory=dict)
    evidence: dict[str, Any] = Field(default_factory=dict)


class DifferentialVerificationArtifact(ArtifactBase):
    """差分検証工程の成果物."""

    equivalence: bool
    diffs: list[dict[str, Any]] = Field(default_factory=list)
    classification: str
    confidence: float = 0.0
    evidence: dict[str, Any] = Field(default_factory=dict)


class QualityDecision(str, Enum):
    """品質裁定結果."""

    PASSED = "PASSED"
    DESIGN_ISSUE = "DESIGN_ISSUE"
    TRANSFORM_ISSUE = "TRANSFORM_ISSUE"
    TEST_ISSUE = "TEST_ISSUE"
    ENV_ISSUE = "ENV_ISSUE"
    KNOWN_LEGACY = "KNOWN_LEGACY"


class QualityGateArtifact(ArtifactBase):
    """品質裁定工程の成果物."""

    decision: QualityDecision
    target_agent: str
    reason: str
    severity: str
    evidence: dict[str, Any] = Field(default_factory=dict)


class LimitedFixArtifact(ArtifactBase):
    """限定修正工程の成果物."""

    applied: bool
    target_code: str
    patch_summary: list[str] = Field(default_factory=list)
    retest_required: bool = False


class HumanFeedbackArtifact(ArtifactBase):
    """人間コマンド成果物."""

    command: str
    actor: str
    payload: dict[str, Any] = Field(default_factory=dict)


class TransformationIterationRecord(BaseModel):
    """変換反復の1回分記録."""

    iteration: int
    score: float | None = None
    accepted: bool = False
    feedback: list[str] = Field(default_factory=list)
    suggestions: list[str] = Field(default_factory=list)


class TransformationIterationArtifact(ArtifactBase):
    """変換反復工程の成果物."""

    iterations: list[TransformationIterationRecord] = Field(default_factory=list)
    accepted: bool = False
    final_score: float | None = None


class TaskSpec(BaseModel):
    """タスク仕様."""

    task_id: str
    trace_id: str
    migration_type: str
    source_language: str
    target_language: str
    module: str
    expected_outputs: dict[str, Any] = Field(default_factory=dict)
    options: dict[str, Any] = Field(default_factory=dict)


def now_iso() -> str:
    """UTC時刻を ISO8601 文字列で返す."""
    return datetime.now(tz=UTC).isoformat()


def build_meta(
    *,
    task_id: str,
    trace_id: str,
    stage: str,
    source_language: str | None = None,
    target_language: str | None = None,
    module: str | None = None,
) -> ArtifactMeta:
    """成果物メタ情報を生成."""
    return ArtifactMeta(
        task_id=task_id,
        trace_id=trace_id,
        stage=stage,
        generated_at=now_iso(),
        source_language=source_language,
        target_language=target_language,
        module=module,
    )
