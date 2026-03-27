"""Flow 系契約."""

from __future__ import annotations

from enum import StrEnum
from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class FlowStatus(StrEnum):
    """フロー実行状態."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class AgentRoleSpec(ContractModel):
    """Capability ベースの Agent 役割定義."""

    role_id: str = Field(..., description="役割識別子")
    name: str = Field(..., description="表示名")
    description: str = Field(default="", description="説明")
    capability_tags: list[str] = Field(default_factory=list, description="能力タグ")
    max_parallelism: int = Field(default=1, ge=1, description="同時実行上限")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class FlowDefinition(ContractModel):
    """層横断で参照するフロー定義."""

    flow_id: str = Field(..., description="フロー識別子")
    name: str = Field(..., description="フロー名")
    version: str = Field(default="1.0.0", description="フローバージョン")
    description: str = Field(default="", description="説明")
    roles: list[AgentRoleSpec] = Field(default_factory=list, description="役割定義")
    steps: list[str] = Field(default_factory=list, description="標準ステップ一覧")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class FlowExecutionState(ContractModel):
    """実行中フローの状態契約."""

    flow_id: str = Field(..., description="フロー識別子")
    run_id: str = Field(..., description="実行識別子")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    status: FlowStatus = Field(default=FlowStatus.PENDING, description="現在状態")
    current_step: str | None = Field(default=None, description="現在ステップ")
    completed_steps: list[str] = Field(default_factory=list, description="完了済みステップ")
    artifact_ids: list[str] = Field(default_factory=list, description="関連成果物")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")
