"""状態モデル定義.

L2層：標準状態フィールド（Goal/Constraints/Facts/Decisions）を定義。
状態の一次資産化を実現し、再現可能な実行を担保。

設計原則:
- 全状態フィールドを型付けで定義
- Goal（目標）とConstraints（制約）でエージェントの行動範囲を明確化
- Facts（事実）で収集した情報を追跡
- Decisions（判断）で意思決定履歴を記録

使用例:
    >>> from agentflow.state.models import Goal, Fact, Decision
    >>>
    >>> goal = Goal(
    ...     objective="顧客への見積書作成",
    ...     constraints=["予算上限100万円", "納期1週間以内"],
    ...     success_criteria=["顧客承認", "上長承認"],
    ... )
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ConstraintType(str, Enum):
    """制約タイプ."""

    BUDGET = "budget"  # 予算制約
    TIME = "time"  # 時間制約
    RESOURCE = "resource"  # リソース制約
    QUALITY = "quality"  # 品質制約
    POLICY = "policy"  # ポリシー制約
    SECURITY = "security"  # セキュリティ制約
    CUSTOM = "custom"  # カスタム制約


class Constraint(BaseModel):
    """制約条件.

    Attributes:
        type: 制約タイプ
        description: 制約の説明
        value: 制約値（数値や条件）
        unit: 単位（円、秒、件数等）
        priority: 優先度（1=最高）
        is_hard: 絶対条件か（Falseの場合は推奨）
    """

    type: ConstraintType = Field(..., description="制約タイプ")
    description: str = Field(..., description="制約の説明")
    value: Any = Field(None, description="制約値")
    unit: str | None = Field(None, description="単位")
    priority: int = Field(default=1, description="優先度（1=最高）")
    is_hard: bool = Field(default=True, description="絶対条件か")


class Goal(BaseModel):
    """目標定義.

    エージェントが達成すべき目標と成功基準を定義。

    Attributes:
        objective: 目標の説明
        constraints: 制約条件リスト
        success_criteria: 成功基準リスト
        priority: 優先度
        deadline: 期限
        context: 追加コンテキスト
    """

    objective: str = Field(..., description="目標の説明")
    constraints: list[Constraint] = Field(
        default_factory=list,
        description="制約条件リスト",
    )
    success_criteria: list[str] = Field(
        default_factory=list,
        description="成功基準リスト",
    )
    priority: int = Field(default=1, description="優先度（1=最高）")
    deadline: datetime | None = Field(None, description="期限")
    context: dict[str, Any] = Field(
        default_factory=dict,
        description="追加コンテキスト",
    )

    def add_constraint(
        self,
        constraint_type: ConstraintType,
        description: str,
        value: Any = None,
        **kwargs: Any,
    ) -> Goal:
        """制約を追加."""
        constraint = Constraint(
            type=constraint_type,
            description=description,
            value=value,
            **kwargs,
        )
        self.constraints.append(constraint)
        return self


class FactSource(str, Enum):
    """事実のソース."""

    TOOL = "tool"  # ツール実行結果
    RAG = "rag"  # RAG検索結果
    USER = "user"  # ユーザー入力
    SYSTEM = "system"  # システム情報
    EXTERNAL = "external"  # 外部API


class Fact(BaseModel):
    """事実（収集した情報）.

    エージェントが収集した情報を記録。
    根拠追跡と再現性のために使用。

    Attributes:
        id: 事実ID
        source: ソースタイプ
        source_name: ソース名（ツール名、API名等）
        data: データ本体
        timestamp: 収集日時
        confidence: 信頼度（0.0-1.0）
        metadata: メタデータ
    """

    id: str = Field(
        default_factory=lambda: f"fact-{datetime.now().strftime('%Y%m%d%H%M%S%f')}",
        description="事実ID",
    )
    source: FactSource = Field(..., description="ソースタイプ")
    source_name: str = Field(..., description="ソース名")
    data: dict[str, Any] = Field(..., description="データ本体")
    timestamp: datetime = Field(default_factory=datetime.now, description="収集日時")
    confidence: float = Field(default=1.0, ge=0.0, le=1.0, description="信頼度")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    def is_reliable(self, threshold: float = 0.8) -> bool:
        """信頼できる事実か."""
        return self.confidence >= threshold


class DecisionType(str, Enum):
    """判断タイプ."""

    ACTION = "action"  # アクション選択
    PARAMETER = "parameter"  # パラメータ決定
    BRANCH = "branch"  # 分岐判断
    APPROVAL = "approval"  # 承認判断
    FALLBACK = "fallback"  # フォールバック判断
    TERMINATION = "termination"  # 終了判断


class Decision(BaseModel):
    """判断記録.

    エージェントが行った意思決定を記録。
    説明可能性と監査のために使用。

    Attributes:
        id: 判断ID
        step: ステップ名/ID
        decision_type: 判断タイプ
        choice: 選択した選択肢
        alternatives: 他の選択肢
        reason: 選択理由
        evidence_facts: 根拠となる事実ID
        timestamp: 判断日時
        confidence: 確信度（0.0-1.0）
        metadata: メタデータ
    """

    id: str = Field(
        default_factory=lambda: f"dec-{datetime.now().strftime('%Y%m%d%H%M%S%f')}",
        description="判断ID",
    )
    step: str = Field(..., description="ステップ名/ID")
    decision_type: DecisionType = Field(..., description="判断タイプ")
    choice: str = Field(..., description="選択した選択肢")
    alternatives: list[str] = Field(
        default_factory=list,
        description="他の選択肢",
    )
    reason: str = Field(..., description="選択理由")
    evidence_facts: list[str] = Field(
        default_factory=list,
        description="根拠となる事実ID",
    )
    timestamp: datetime = Field(default_factory=datetime.now, description="判断日時")
    confidence: float = Field(default=1.0, ge=0.0, le=1.0, description="確信度")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class StandardState(BaseModel):
    """標準状態モデル.

    L2層の標準状態フィールドを包含。
    Goal/Constraints/Facts/Decisionsを統合管理。

    Attributes:
        goal: 現在の目標
        facts: 収集した事実リスト
        decisions: 判断履歴
    """

    goal: Goal | None = Field(None, description="現在の目標")
    facts: list[Fact] = Field(default_factory=list, description="収集した事実")
    decisions: list[Decision] = Field(default_factory=list, description="判断履歴")

    def add_fact(self, fact: Fact) -> StandardState:
        """事実を追加."""
        self.facts.append(fact)
        return self

    def add_decision(self, decision: Decision) -> StandardState:
        """判断を追加."""
        self.decisions.append(decision)
        return self

    def get_facts_by_source(self, source: FactSource) -> list[Fact]:
        """ソース別に事実を取得."""
        return [f for f in self.facts if f.source == source]

    def get_reliable_facts(self, threshold: float = 0.8) -> list[Fact]:
        """信頼できる事実のみ取得."""
        return [f for f in self.facts if f.is_reliable(threshold)]

    def get_decisions_by_step(self, step: str) -> list[Decision]:
        """ステップ別に判断を取得."""
        return [d for d in self.decisions if d.step == step]

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "goal": self.goal.model_dump() if self.goal else None,
            "facts": [f.model_dump() for f in self.facts],
            "decisions": [d.model_dump() for d in self.decisions],
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> StandardState:
        """辞書から復元."""
        return cls(
            goal=Goal(**data["goal"]) if data.get("goal") else None,
            facts=[Fact(**f) for f in data.get("facts", [])],
            decisions=[Decision(**d) for d in data.get("decisions", [])],
        )


__all__ = [
    "Constraint",
    "ConstraintType",
    "Decision",
    "DecisionType",
    "Fact",
    "FactSource",
    "Goal",
    "StandardState",
]
