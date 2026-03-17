"""制約ソルバー - 制約条件の検証と充足.

行動の妥当性を制約条件に基づいて検証し、
制約違反を事前に検出する。

設計原則:
- 制約はハード（必須）とソフト（推奨）に分類
- 制約違反の詳細な報告
- 制約を満たすアクションの提案

使用例:
    >>> solver = ConstraintSolver()
    >>> solver.add_constraint(
    ...     name="budget_limit",
    ...     check_fn=lambda state: state.get("budget", 0) >= 0,
    ...     description="予算は0以上必須",
    ...     is_hard=True,
    ... )
    >>> result = solver.check({"budget": -1000})
    >>> print(result.is_valid)  # False
    >>> print(result.violations[0].name)  # "budget_limit"
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Callable


class ConstraintViolation(BaseModel):
    """制約違反情報.

    Attributes:
        constraint_id: 違反した制約のID
        name: 制約名
        description: 制約の説明
        is_hard: ハード制約（必須）かどうか
        current_value: 現在の値
        expected: 期待される条件
        suggestion: 修正提案
    """

    constraint_id: str = Field(..., description="制約ID")
    name: str = Field(..., description="制約名")
    description: str = Field(default="", description="説明")
    is_hard: bool = Field(default=True, description="ハード制約か")
    current_value: Any = Field(default=None, description="現在の値")
    expected: str = Field(default="", description="期待条件")
    suggestion: str = Field(default="", description="修正提案")


class SolverResult(BaseModel):
    """制約チェック結果.

    Attributes:
        is_valid: 全てのハード制約を満たすか
        violations: 違反リスト
        warnings: 警告（ソフト制約違反）
        score: 制約充足スコア（0.0-1.0）
    """

    is_valid: bool = Field(default=True, description="有効か")
    violations: list[ConstraintViolation] = Field(default_factory=list)
    warnings: list[ConstraintViolation] = Field(default_factory=list)
    score: float = Field(default=1.0, ge=0.0, le=1.0, description="充足スコア")


@dataclass
class Constraint:
    """制約条件の内部表現."""

    id: str
    name: str
    check_fn: Callable[[dict[str, Any]], bool]
    description: str = ""
    is_hard: bool = True
    priority: int = 1
    expected: str = ""
    suggestion_fn: Callable[[dict[str, Any]], str] | None = None


@dataclass
class ConstraintSolver:
    """制約ソルバー.

    制約条件を管理し、状態やアクションの妥当性を検証する。

    主な機能:
    - 制約の登録と管理
    - 状態の制約チェック
    - 違反の詳細報告
    - 修正提案の生成
    """

    constraints: dict[str, Constraint] = field(default_factory=dict)
    _logger: logging.Logger = field(default_factory=lambda: logging.getLogger("agentflow.world_model.constraint"))

    def add_constraint(
        self,
        name: str,
        check_fn: Callable[[dict[str, Any]], bool],
        description: str = "",
        is_hard: bool = True,
        priority: int = 1,
        expected: str = "",
        suggestion_fn: Callable[[dict[str, Any]], str] | None = None,
    ) -> str:
        """制約を追加.

        Args:
            name: 制約名
            check_fn: チェック関数 (state) -> bool
            description: 説明
            is_hard: ハード制約（必須）か
            priority: 優先度（1=最高）
            expected: 期待条件の説明
            suggestion_fn: 修正提案生成関数

        Returns:
            制約ID
        """
        constraint_id = f"const-{uuid.uuid4().hex[:8]}"
        constraint = Constraint(
            id=constraint_id,
            name=name,
            check_fn=check_fn,
            description=description,
            is_hard=is_hard,
            priority=priority,
            expected=expected,
            suggestion_fn=suggestion_fn,
        )
        self.constraints[constraint_id] = constraint
        self._logger.debug(f"制約追加: {name} (hard={is_hard})")
        return constraint_id

    def remove_constraint(self, constraint_id: str) -> bool:
        """制約を削除."""
        if constraint_id in self.constraints:
            del self.constraints[constraint_id]
            return True
        return False

    def check(self, state: dict[str, Any]) -> SolverResult:
        """状態が制約を満たすかチェック.

        Args:
            state: チェックする状態

        Returns:
            SolverResult
        """
        violations: list[ConstraintViolation] = []
        warnings: list[ConstraintViolation] = []

        # 優先度順にソート
        sorted_constraints = sorted(
            self.constraints.values(),
            key=lambda c: c.priority,
        )

        for constraint in sorted_constraints:
            try:
                is_satisfied = constraint.check_fn(state)
            except Exception as e:
                self._logger.warning(f"制約チェックエラー {constraint.name}: {e}")
                is_satisfied = False

            if not is_satisfied:
                violation = ConstraintViolation(
                    constraint_id=constraint.id,
                    name=constraint.name,
                    description=constraint.description,
                    is_hard=constraint.is_hard,
                    expected=constraint.expected,
                    suggestion=self._generate_suggestion(constraint, state),
                )

                if constraint.is_hard:
                    violations.append(violation)
                else:
                    warnings.append(violation)

        # スコア計算
        total = len(self.constraints)
        satisfied = total - len(violations) - len(warnings)
        score = satisfied / total if total > 0 else 1.0

        return SolverResult(
            is_valid=len(violations) == 0,
            violations=violations,
            warnings=warnings,
            score=score,
        )

    def _generate_suggestion(
        self,
        constraint: Constraint,
        state: dict[str, Any],
    ) -> str:
        """修正提案を生成."""
        if constraint.suggestion_fn:
            try:
                return constraint.suggestion_fn(state)
            except Exception:
                pass
        return f"制約 '{constraint.name}' を満たすように状態を修正してください"

    def check_action(
        self,
        action: dict[str, Any],
        current_state: dict[str, Any],
        predicted_state: dict[str, Any],
    ) -> SolverResult:
        """アクションの妥当性をチェック.

        Args:
            action: 実行予定のアクション
            current_state: 現在の状態
            predicted_state: アクション実行後の予測状態

        Returns:
            SolverResult
        """
        # 予測状態に対して制約チェック
        return self.check(predicted_state)

        # アクション固有の制約もチェック（将来拡張用）

    def get_valid_actions(
        self,
        candidates: list[dict[str, Any]],
        current_state: dict[str, Any],
        predict_fn: Callable[[dict[str, Any], dict[str, Any]], dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """制約を満たす有効なアクションをフィルタリング.

        Args:
            candidates: 候補アクションリスト
            current_state: 現在の状態
            predict_fn: 予測関数 (action, state) -> predicted_state

        Returns:
            有効なアクションリスト
        """
        valid_actions = []

        for action in candidates:
            try:
                predicted = predict_fn(action, current_state)
                result = self.check(predicted)
                if result.is_valid:
                    valid_actions.append(action)
            except Exception as e:
                self._logger.warning(f"アクション検証エラー: {e}")

        return valid_actions

    def get_constraint_summary(self) -> dict[str, Any]:
        """制約の概要を取得."""
        hard_count = sum(1 for c in self.constraints.values() if c.is_hard)
        soft_count = len(self.constraints) - hard_count

        return {
            "total": len(self.constraints),
            "hard_constraints": hard_count,
            "soft_constraints": soft_count,
            "constraints": [
                {
                    "id": c.id,
                    "name": c.name,
                    "is_hard": c.is_hard,
                    "priority": c.priority,
                    "description": c.description,
                }
                for c in self.constraints.values()
            ],
        }


__all__ = [
    "Constraint",
    "ConstraintSolver",
    "ConstraintViolation",
    "SolverResult",
]
