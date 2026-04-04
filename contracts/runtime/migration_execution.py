"""Migration execution contracts shared by framework and apps."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class MigrationTaskProfile(ContractModel):
    """Normalized migration task profile."""

    migration_type: str = Field(..., description="移行タイプ識別子")
    migration_family: str = Field(..., description="移行ファミリー")
    source_profile: dict[str, Any] = Field(default_factory=dict, description="ソース側プロファイル")
    target_profile: dict[str, Any] = Field(default_factory=dict, description="ターゲット側プロファイル")
    preparation_profile: dict[str, Any] = Field(default_factory=dict, description="事前準備プロファイル")
    analysis_capability_set: list[str] = Field(default_factory=list, description="解析能力セット")
    transformation_strategy: str = Field(default="", description="変換戦略")
    verification_strategy: str = Field(default="", description="検証戦略")
    delivery_strategy: str = Field(default="", description="成果物提供戦略")


class ExecutorRoutePolicy(ContractModel):
    """Executor routing policy for a stage."""

    stage: str = Field(..., description="対象ステージ")
    default_executor: str = Field(..., description="第一実行器")
    fallback_executor: str = Field(default="native", description="フォールバック実行器")
    max_retries: int = Field(default=1, ge=0, description="最大リトライ回数")
    fallback_conditions: list[str] = Field(default_factory=list, description="フォールバック条件")


class StageExecutionPlan(ContractModel):
    """Concrete stage execution plan."""

    stage: str = Field(..., description="対象ステージ")
    capability_id: str = Field(default="", description="ステージ能力ID")
    default_executor: str = Field(..., description="第一実行器")
    fallback_executor: str = Field(default="native", description="フォールバック実行器")
    max_retries: int = Field(default=1, ge=0, description="最大リトライ回数")
    fallback_conditions: list[str] = Field(default_factory=list, description="フォールバック条件")
    required_skills: list[str] = Field(default_factory=list, description="事前実行するスキル")
    evidence_kinds: list[str] = Field(default_factory=list, description="収集する証跡種別")


class StageTimelineEvent(ContractModel):
    """Projection-friendly timeline event."""

    event_type: str = Field(..., description="イベント種別")
    stage: str = Field(default="", description="対象ステージ")
    title: str = Field(default="", description="表示用タイトル")
    detail: str = Field(default="", description="表示用詳細")
    status: str = Field(default="", description="状態")
    executor: str | None = Field(default=None, description="実行器")
    reason: str | None = Field(default=None, description="理由")
    timestamp: float | None = Field(default=None, description="UNIX timestamp")


class EvidencePacket(ContractModel):
    """Evidence summary projected to UI/state."""

    stage: str = Field(..., description="対象ステージ")
    summary: str = Field(default="", description="要約")
    artifact_paths: dict[str, str] = Field(default_factory=dict, description="成果物パス")
    metadata: dict[str, Any] = Field(default_factory=dict, description="補助メタデータ")


class RetryDecision(ContractModel):
    """Retry/fallback decision trace."""

    stage: str = Field(..., description="対象ステージ")
    decision: str = Field(..., description="判定")
    executor: str | None = Field(default=None, description="適用実行器")
    reason: str = Field(default="", description="理由")
    attempt: int = Field(default=0, ge=0, description="試行回数")
