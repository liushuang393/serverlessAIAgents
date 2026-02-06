# -*- coding: utf-8 -*-
"""状態アクション定義.

状態変更を表現するアクションを定義。

設計原則:
- 不変性: 状態は直接変更せず、アクションで変更
- 追跡可能: 全ての変更が記録される
- 型安全: 厳密な型定義

使用例:
    >>> from agentflow.state.actions import create_action, ActionType
    >>>
    >>> action = create_action(
    ...     ActionType.UPDATE_PROGRESS,
    ...     {"progress": 0.5, "message": "処理中..."},
    ... )
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any
import uuid


class ActionType(str, Enum):
    """アクション種別."""

    # 実行状態
    SET_EXECUTION_STATUS = "SET_EXECUTION_STATUS"
    UPDATE_PROGRESS = "UPDATE_PROGRESS"
    SET_CURRENT_STEP = "SET_CURRENT_STEP"
    COMPLETE_STEP = "COMPLETE_STEP"
    FAIL_STEP = "FAIL_STEP"

    # コンテキスト
    SET_CONTEXT = "SET_CONTEXT"
    UPDATE_CONTEXT = "UPDATE_CONTEXT"
    MERGE_CONTEXT = "MERGE_CONTEXT"

    # 結果
    ADD_RESULT = "ADD_RESULT"
    SET_FINAL_OUTPUT = "SET_FINAL_OUTPUT"
    SET_ERROR = "SET_ERROR"

    # 計画
    SET_PLAN = "SET_PLAN"
    UPDATE_PLAN = "UPDATE_PLAN"

    # HITL
    REQUEST_APPROVAL = "REQUEST_APPROVAL"
    RECEIVE_APPROVAL = "RECEIVE_APPROVAL"

    # 履歴
    PUSH_HISTORY = "PUSH_HISTORY"
    CLEAR_HISTORY = "CLEAR_HISTORY"

    # チェックポイント
    CREATE_CHECKPOINT = "CREATE_CHECKPOINT"
    RESTORE_CHECKPOINT = "RESTORE_CHECKPOINT"

    # ==========================================================================
    # L2標準状態フィールド（Goal/Facts/Decisions）
    # ==========================================================================
    SET_GOAL = "SET_GOAL"
    UPDATE_GOAL = "UPDATE_GOAL"
    ADD_CONSTRAINT = "ADD_CONSTRAINT"
    ADD_FACT = "ADD_FACT"
    ADD_DECISION = "ADD_DECISION"
    CLEAR_FACTS = "CLEAR_FACTS"

    # ==========================================================================
    # Agent OS Task Lifecycle
    # ==========================================================================
    TASK_CREATED = "TASK_CREATED"
    TASK_PLANNED = "TASK_PLANNED"
    TASK_STARTED = "TASK_STARTED"
    TASK_VERIFIED = "TASK_VERIFIED"
    TASK_COMPLETED = "TASK_COMPLETED"
    TASK_FAILED = "TASK_FAILED"
    TASK_PAUSED = "TASK_PAUSED"
    TASK_CANCELLED = "TASK_CANCELLED"

    # カスタム
    CUSTOM = "CUSTOM"


@dataclass
class Action:
    """状態変更アクション.

    Attributes:
        id: アクションID
        type: アクション種別
        payload: ペイロード
        timestamp: タイムスタンプ
        metadata: メタデータ
    """

    id: str = field(default_factory=lambda: f"action-{uuid.uuid4().hex[:8]}")
    type: ActionType = ActionType.CUSTOM
    payload: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "type": self.type.value,
            "payload": self.payload,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata,
        }


def create_action(
    action_type: ActionType,
    payload: dict[str, Any] | None = None,
    metadata: dict[str, Any] | None = None,
) -> Action:
    """アクションを作成.

    Args:
        action_type: アクション種別
        payload: ペイロード
        metadata: メタデータ

    Returns:
        Action
    """
    return Action(
        type=action_type,
        payload=payload or {},
        metadata=metadata or {},
    )


# 便利なアクション作成関数
def set_execution_status(status: str, execution_id: str = "") -> Action:
    """実行状態を設定するアクション."""
    return create_action(
        ActionType.SET_EXECUTION_STATUS,
        {"status": status, "execution_id": execution_id},
    )


def update_progress(progress: float, message: str = "") -> Action:
    """進捗を更新するアクション."""
    return create_action(
        ActionType.UPDATE_PROGRESS,
        {"progress": progress, "message": message},
    )


def set_current_step(step_id: str, step_name: str = "") -> Action:
    """現在のステップを設定するアクション."""
    return create_action(
        ActionType.SET_CURRENT_STEP,
        {"step_id": step_id, "step_name": step_name},
    )


def complete_step(step_id: str, output: Any = None) -> Action:
    """ステップ完了アクション."""
    return create_action(
        ActionType.COMPLETE_STEP,
        {"step_id": step_id, "output": output},
    )


def fail_step(step_id: str, error: str) -> Action:
    """ステップ失敗アクション."""
    return create_action(
        ActionType.FAIL_STEP,
        {"step_id": step_id, "error": error},
    )


def set_context(context: dict[str, Any]) -> Action:
    """コンテキストを設定するアクション."""
    return create_action(ActionType.SET_CONTEXT, {"context": context})


def update_context(key: str, value: Any) -> Action:
    """コンテキストを更新するアクション."""
    return create_action(ActionType.UPDATE_CONTEXT, {"key": key, "value": value})


def merge_context(context: dict[str, Any]) -> Action:
    """コンテキストをマージするアクション."""
    return create_action(ActionType.MERGE_CONTEXT, {"context": context})


def add_result(step_id: str, result: Any) -> Action:
    """結果を追加するアクション."""
    return create_action(ActionType.ADD_RESULT, {"step_id": step_id, "result": result})


def set_final_output(output: Any) -> Action:
    """最終出力を設定するアクション."""
    return create_action(ActionType.SET_FINAL_OUTPUT, {"output": output})


def set_error(error: str, step_id: str | None = None) -> Action:
    """エラーを設定するアクション."""
    return create_action(ActionType.SET_ERROR, {"error": error, "step_id": step_id})


def set_plan(plan: dict[str, Any]) -> Action:
    """計画を設定するアクション."""
    return create_action(ActionType.SET_PLAN, {"plan": plan})


def request_approval(
    request_id: str,
    action: str,
    context: dict[str, Any] | None = None,
) -> Action:
    """承認要求アクション."""
    return create_action(
        ActionType.REQUEST_APPROVAL,
        {"request_id": request_id, "action": action, "context": context or {}},
    )


def receive_approval(request_id: str, approved: bool, comment: str = "") -> Action:
    """承認受信アクション."""
    return create_action(
        ActionType.RECEIVE_APPROVAL,
        {"request_id": request_id, "approved": approved, "comment": comment},
    )


def create_checkpoint(checkpoint_id: str, label: str = "") -> Action:
    """チェックポイント作成アクション."""
    return create_action(
        ActionType.CREATE_CHECKPOINT,
        {"checkpoint_id": checkpoint_id, "label": label},
    )


def restore_checkpoint(checkpoint_id: str) -> Action:
    """チェックポイント復元アクション."""
    return create_action(
        ActionType.RESTORE_CHECKPOINT,
        {"checkpoint_id": checkpoint_id},
    )


# ==========================================================================
# L2標準状態フィールド用アクション
# ==========================================================================
def set_goal(
    objective: str,
    constraints: list[dict[str, Any]] | None = None,
    success_criteria: list[str] | None = None,
    priority: int = 1,
    deadline: str | None = None,
    context: dict[str, Any] | None = None,
) -> Action:
    """目標を設定するアクション."""
    return create_action(
        ActionType.SET_GOAL,
        {
            "objective": objective,
            "constraints": constraints or [],
            "success_criteria": success_criteria or [],
            "priority": priority,
            "deadline": deadline,
            "context": context or {},
        },
    )


def update_goal(updates: dict[str, Any]) -> Action:
    """目標を更新するアクション."""
    return create_action(ActionType.UPDATE_GOAL, {"updates": updates})


def add_constraint(
    constraint_type: str,
    description: str,
    value: Any = None,
    is_hard: bool = True,
) -> Action:
    """制約を追加するアクション."""
    return create_action(
        ActionType.ADD_CONSTRAINT,
        {
            "type": constraint_type,
            "description": description,
            "value": value,
            "is_hard": is_hard,
        },
    )


def add_fact(
    source: str,
    source_name: str,
    data: dict[str, Any],
    confidence: float = 1.0,
    metadata: dict[str, Any] | None = None,
) -> Action:
    """事実を追加するアクション."""
    return create_action(
        ActionType.ADD_FACT,
        {
            "source": source,
            "source_name": source_name,
            "data": data,
            "confidence": confidence,
            "metadata": metadata or {},
        },
    )


def add_decision(
    step: str,
    decision_type: str,
    choice: str,
    reason: str,
    alternatives: list[str] | None = None,
    evidence_facts: list[str] | None = None,
    confidence: float = 1.0,
) -> Action:
    """判断を追加するアクション."""
    return create_action(
        ActionType.ADD_DECISION,
        {
            "step": step,
            "decision_type": decision_type,
            "choice": choice,
            "reason": reason,
            "alternatives": alternatives or [],
            "evidence_facts": evidence_facts or [],
            "confidence": confidence,
        },
    )


def clear_facts() -> Action:
    """事実をクリアするアクション."""
    return create_action(ActionType.CLEAR_FACTS, {})


# エクスポート
__all__ = [
    "ActionType",
    "Action",
    "create_action",
    "set_execution_status",
    "update_progress",
    "set_current_step",
    "complete_step",
    "fail_step",
    "set_context",
    "update_context",
    "merge_context",
    "add_result",
    "set_final_output",
    "set_error",
    "set_plan",
    "request_approval",
    "receive_approval",
    "create_checkpoint",
    "restore_checkpoint",
    # L2標準状態フィールド
    "set_goal",
    "update_goal",
    "add_constraint",
    "add_fact",
    "add_decision",
    "clear_facts",
]
