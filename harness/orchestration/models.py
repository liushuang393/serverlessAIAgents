"""自律編排パイプライン - データモデル定義.

ExecutionPlan / PlanStep / PlannerInput / PlannerOutput など、
パイプライン全体で共有するモデルを集約する。
"""

from __future__ import annotations

import uuid
from datetime import UTC, datetime
from enum import StrEnum
from typing import Any

from pydantic import BaseModel, Field

from harness.risk.service import RiskLevel


class StepStatus(StrEnum):
    """ステップ実行状態."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"
    AWAITING_APPROVAL = "awaiting_approval"


class PlanStep(BaseModel):
    """実行計画の1ステップ.

    Attributes:
        step_id: 一意識別子
        agent_id: AgentRegistry 内のエージェントID
        description: ステップの説明
        input_spec: Agent への入力テンプレート（前ステップの結果をマージ可能）
        risk_level: ステップ単位のリスクレベル
        dependencies: 前提ステップの step_id リスト
        timeout_seconds: タイムアウト秒数
        max_retries: 最大リトライ回数
        status: 現在の実行状態
        metadata: 拡張メタデータ
    """

    step_id: str = Field(..., description="一意識別子")
    agent_id: str = Field(..., description="AgentRegistry 内のエージェントID")
    description: str = Field(..., description="ステップの説明")
    input_spec: dict[str, Any] = Field(default_factory=dict, description="Agent への入力テンプレート")
    risk_level: RiskLevel = Field(default=RiskLevel.LOW, description="リスクレベル")
    dependencies: list[str] = Field(default_factory=list, description="前提ステップの step_id リスト")
    timeout_seconds: int = Field(default=300, ge=1, description="タイムアウト秒数")
    max_retries: int = Field(default=2, ge=0, description="最大リトライ回数")
    status: StepStatus = Field(default=StepStatus.PENDING, description="実行状態")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class ExecutionPlan(BaseModel):
    """タスク分解結果としての実行計画.

    Planner Agent が生成し、DynamicFlowGenerator がフローに変換する。
    """

    plan_id: str = Field(
        default_factory=lambda: f"plan-{uuid.uuid4().hex[:8]}",
        description="計画ID",
    )
    goal: str = Field(..., description="元のユーザー要求")
    steps: list[PlanStep] = Field(default_factory=list, description="実行ステップ一覧")
    overall_risk: RiskLevel = Field(default=RiskLevel.LOW, description="全体リスクレベル")
    created_at: datetime = Field(
        default_factory=lambda: datetime.now(UTC),
        description="作成日時",
    )
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")

    def get_step(self, step_id: str) -> PlanStep | None:
        """step_id でステップを検索."""
        for step in self.steps:
            if step.step_id == step_id:
                return step
        return None

    def compute_overall_risk(self) -> RiskLevel:
        """全ステップの最高リスクを全体リスクとして算出."""
        if not self.steps:
            return RiskLevel.LOW
        level_order = [RiskLevel.LOW, RiskLevel.MEDIUM, RiskLevel.HIGH, RiskLevel.CRITICAL]
        max_level = max(
            (s.risk_level for s in self.steps),
            key=lambda lv: level_order.index(lv),
        )
        return max_level


class PlannerInput(BaseModel):
    """PlannerAgent への入力."""

    user_request: str = Field(..., description="ユーザーの要求テキスト")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")
    constraints: list[str] = Field(default_factory=list, description="制約条件リスト")
    available_agents: list[str] = Field(default_factory=list, description="利用可能エージェントIDリスト")


class PlannerOutput(BaseModel):
    """PlannerAgent の出力."""

    plan: ExecutionPlan = Field(..., description="生成された実行計画")
    reasoning: str = Field(default="", description="LLM の思考過程")
    warnings: list[str] = Field(default_factory=list, description="警告事項リスト")


class ReplanRequest(BaseModel):
    """再計画リクエスト.

    ステップ検証失敗時に PlannerAgent に送信し、
    失敗ステップ以降の計画を再生成する。
    """

    original_plan: ExecutionPlan = Field(..., description="元の実行計画")
    failed_step_id: str = Field(..., description="失敗したステップID")
    failure_reason: str = Field(..., description="失敗理由")
    completed_results: dict[str, Any] = Field(
        default_factory=dict,
        description="完了済みステップの結果マップ",
    )


__all__ = [
    "ExecutionPlan",
    "PlanStep",
    "PlannerInput",
    "PlannerOutput",
    "ReplanRequest",
    "RiskLevel",
    "StepStatus",
]
