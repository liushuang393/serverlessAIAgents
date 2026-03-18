"""ポリシーと承認の契約."""

from __future__ import annotations

import uuid
from datetime import UTC, datetime
from typing import Any, Protocol, runtime_checkable

from pydantic import Field

from contracts.base import ContractModel


class PolicyDecision(ContractModel):
    """実行判断の標準契約."""

    decision_id: str = Field(default_factory=lambda: str(uuid.uuid4()), description="判断識別子")
    policy_name: str = Field(..., description="ポリシー名")
    decision: str = Field(..., description="allow/deny/approval_required 等")
    reason: str = Field(default="", description="理由")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    subject: dict[str, Any] = Field(default_factory=dict, description="主体情報")
    resource: dict[str, Any] = Field(default_factory=dict, description="対象情報")
    action: str = Field(default="", description="操作")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class ApprovalRequest(ContractModel):
    """承認要求の標準契約."""

    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    action: str = Field(..., description="承認対象アクション")
    resource_id: str | None = Field(default=None, description="対象リソース識別子")
    resource_type: str | None = Field(default=None, description="対象リソース種別")
    reason: str = Field(..., description="承認理由")
    context: dict[str, Any] = Field(default_factory=dict, description="追加文脈")
    requester: str | None = Field(default=None, description="要求元")
    priority: str = Field(default="normal", description="優先度")
    timeout_seconds: int | None = Field(default=None, description="タイムアウト")
    expires_at: datetime | None = Field(default=None, description="期限")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")
    created_at: datetime = Field(default_factory=lambda: datetime.now(UTC))


class EvalResult(ContractModel):
    """評価結果契約."""

    evaluation_id: str = Field(default_factory=lambda: str(uuid.uuid4()), description="評価識別子")
    evaluator: str = Field(..., description="評価器名")
    passed: bool = Field(..., description="合否")
    score: float | None = Field(default=None, description="スコア")
    reason: str = Field(default="", description="理由")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    artifact_id: str | None = Field(default=None, description="対象成果物")
    metrics: dict[str, float] = Field(default_factory=dict, description="評価指標")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")



# ---------------------------------------------------------------------------
# Governance Protocol — harness 層が legacy governance layer に静的依存しないための抽象
# ---------------------------------------------------------------------------


class GovernanceResultProtocol(Protocol):
    """GovernanceResult の duck-type 契約.

    harness 層は具象 GovernanceResult をインポートせず、
    このプロトコルを介してフィールドにアクセスする。
    """

    @property
    def decision(self) -> Any:
        """判定値（GovernanceDecision enum または文字列）."""
        ...

    @property
    def reason(self) -> str:
        """判定理由."""
        ...

    @property
    def requires_approval(self) -> bool:
        """承認要否."""
        ...

    @property
    def warnings(self) -> list[str]:
        """警告リスト."""
        ...

    @property
    def plugin_id(self) -> str | None:
        """プラグインID."""
        ...

    @property
    def plugin_version(self) -> str | None:
        """プラグインバージョン."""
        ...


@runtime_checkable
class GovernanceEngineProtocol(Protocol):
    """GovernanceEngine の duck-type 契約.

    harness/gating 層が具象エンジンをインポートせずに DI で利用する。
    """

    async def evaluate_tool(
        self,
        tool: Any,
        tool_call_id: str | None,
        arguments: dict[str, object],
        context: Any | None = None,
    ) -> GovernanceResultProtocol:
        """ツール実行のガバナンス判定を実行."""
        ...