"""Flow 系契約."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import StrEnum
from typing import Any, Protocol, runtime_checkable

from pydantic import Field

from contracts.base import ContractModel


# ============================================================
# FlowMiddleware: ガバナンス・監査・安全チェックの差し込み契約
# ============================================================


class MiddlewareDecision(StrEnum):
    """ミドルウェア判定結果."""

    ALLOW = "allow"  # 実行を許可
    DENY = "deny"  # 実行を拒否（ノードをスキップ）
    APPROVAL_REQUIRED = "approval_required"  # 人間承認が必要


@dataclass(frozen=True)
class MiddlewareResult:
    """ミドルウェア実行結果.

    Attributes:
        decision: 判定結果
        reason: 判定理由（deny / approval_required 時に必須）
        modified_result: after_node で結果を変換した場合の新しい結果
        metadata: 監査・トレース用の追加情報
    """

    decision: MiddlewareDecision = MiddlewareDecision.ALLOW
    reason: str = ""
    modified_result: dict[str, Any] | None = None
    metadata: dict[str, Any] = field(default_factory=dict)


@runtime_checkable
class FlowMiddleware(Protocol):
    """FlowExecutor に差し込むミドルウェア契約.

    GovernanceEngine / RiskAssessor / ReplayRecorder / ExecutionScorer 等を
    このインターフェースで統一し、FlowExecutor のノード実行前後に介入する。

    設計原則:
    - 各ミドルウェアは独立・合成可能
    - before_node で DENY を返すとノード実行をスキップ
    - after_node で結果の検証・変換・記録が可能
    - ミドルウェアは登録順に実行される
    """

    @property
    def name(self) -> str:
        """ミドルウェア名（ログ・トレース用）."""
        ...

    async def before_node(
        self,
        node_id: str,
        node_name: str,
        inputs: dict[str, Any],
    ) -> MiddlewareResult:
        """ノード実行前に呼ばれる.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            inputs: ノードへの入力データ

        Returns:
            判定結果。DENY の場合、ノード実行をスキップする。
        """
        ...

    async def after_node(
        self,
        node_id: str,
        node_name: str,
        result: dict[str, Any],
        success: bool,
    ) -> MiddlewareResult:
        """ノード実行後に呼ばれる.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            result: ノード実行結果
            success: ノード実行成功したか

        Returns:
            判定結果。modified_result があれば結果を差し替える。
        """
        ...


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
