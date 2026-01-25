# -*- coding: utf-8 -*-
"""世界状態管理 - 現在状態・予測状態・制約の統合管理.

CausalModelとConstraintSolverを統合し、
世界の状態を一元管理する。

設計原則:
- 現在状態と予測状態の分離
- 制約による行動の妥当性検証
- 状態遷移の追跡と履歴管理

使用例:
    >>> world = WorldState()
    >>> world.add_state("budget", 100000)
    >>> world.add_constraint("positive_budget", lambda s: s.get("budget", 0) >= 0)
    >>>
    >>> # アクションの予測
    >>> prediction = world.predict_action({"spend": 50000})
    >>> if prediction.is_valid:
    ...     world.apply_action({"spend": 50000})
"""

from __future__ import annotations

import copy
import logging
import uuid
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

from agentflow.world_model.causal_model import CausalModel
from agentflow.world_model.constraint_solver import ConstraintSolver, SolverResult


class WorldStateSnapshot(BaseModel):
    """世界状態のスナップショット.

    Attributes:
        id: スナップショットID
        state: 状態のコピー
        timestamp: タイムスタンプ
        label: ラベル
        action: この状態に至ったアクション
    """

    id: str = Field(default_factory=lambda: f"wsnap-{uuid.uuid4().hex[:8]}")
    state: dict[str, Any] = Field(default_factory=dict)
    timestamp: datetime = Field(default_factory=datetime.now)
    label: str = Field(default="")
    action: dict[str, Any] | None = Field(default=None)


class ActionPrediction(BaseModel):
    """アクション予測結果.

    Attributes:
        action: 予測対象のアクション
        predicted_state: 予測される状態
        is_valid: 制約を満たすか
        constraint_result: 制約チェック結果
        confidence: 予測の信頼度
        side_effects: 副作用
    """

    action: dict[str, Any] = Field(default_factory=dict)
    predicted_state: dict[str, Any] = Field(default_factory=dict)
    is_valid: bool = Field(default=True)
    constraint_result: SolverResult | None = Field(default=None)
    confidence: float = Field(default=1.0, ge=0.0, le=1.0)
    side_effects: list[str] = Field(default_factory=list)


@dataclass
class WorldState:
    """世界状態管理クラス.

    CausalModel、ConstraintSolver、状態履歴を統合管理する。

    主な機能:
    - 状態の読み書き
    - アクションの予測と検証
    - 制約ベースのアクションフィルタリング
    - 状態履歴の管理とロールバック
    """

    causal_model: CausalModel = field(default_factory=CausalModel)
    constraint_solver: ConstraintSolver = field(default_factory=ConstraintSolver)
    _current_state: dict[str, Any] = field(default_factory=dict)
    _history: list[WorldStateSnapshot] = field(default_factory=list)
    _max_history: int = 100
    _action_handlers: dict[str, Callable[[dict[str, Any], dict[str, Any]], dict[str, Any]]] = field(
        default_factory=dict
    )
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.world_model.state")
    )

    def add_state(
        self,
        key: str,
        value: Any,
        add_to_causal: bool = True,
        description: str = "",
    ) -> None:
        """状態変数を追加.

        Args:
            key: 状態キー
            value: 初期値
            add_to_causal: 因果モデルにも追加するか
            description: 説明
        """
        self._current_state[key] = value

        if add_to_causal:
            self.causal_model.add_node(
                name=key,
                initial_value=value,
                description=description,
            )

        self._logger.debug(f"状態追加: {key} = {value}")

    def add_constraint(
        self,
        name: str,
        check_fn: Callable[[dict[str, Any]], bool],
        description: str = "",
        is_hard: bool = True,
    ) -> str:
        """制約を追加.

        Args:
            name: 制約名
            check_fn: チェック関数
            description: 説明
            is_hard: ハード制約か

        Returns:
            制約ID
        """
        return self.constraint_solver.add_constraint(
            name=name,
            check_fn=check_fn,
            description=description,
            is_hard=is_hard,
        )

    def register_action_handler(
        self,
        action_type: str,
        handler: Callable[[dict[str, Any], dict[str, Any]], dict[str, Any]],
    ) -> None:
        """アクションハンドラーを登録.

        Args:
            action_type: アクションタイプ
            handler: ハンドラー関数 (action, state) -> new_state
        """
        self._action_handlers[action_type] = handler
        self._logger.debug(f"アクションハンドラー登録: {action_type}")

    def get_state(self, key: str | None = None, default: Any = None) -> Any:
        """状態を取得.

        Args:
            key: 状態キー（Noneの場合は全状態）
            default: デフォルト値

        Returns:
            状態値
        """
        if key is None:
            return copy.deepcopy(self._current_state)
        return self._current_state.get(key, default)

    def update_state(self, updates: dict[str, Any], record_history: bool = True) -> None:
        """状態を更新.

        Args:
            updates: 更新する状態
            record_history: 履歴に記録するか
        """
        if record_history:
            self._save_snapshot(label="before_update")

        self._current_state.update(updates)
        self.causal_model.update_state(updates)

        self._logger.debug(f"状態更新: {list(updates.keys())}")

    def predict_action(
        self,
        action: dict[str, Any],
        use_causal: bool = True,
    ) -> ActionPrediction:
        """アクションの結果を予測.

        Args:
            action: 予測するアクション
            use_causal: 因果モデルを使用するか

        Returns:
            ActionPrediction
        """
        action_type = action.get("type") or action.get("action")

        # 予測状態を計算
        if action_type and action_type in self._action_handlers:
            # カスタムハンドラーで予測
            predicted = self._action_handlers[action_type](action, self._current_state)
        elif use_causal:
            # 因果モデルで予測
            interventions = {k: v for k, v in action.items() if k not in ["type", "action"]}
            predicted = self.causal_model.predict_effects(interventions, self._current_state)
        else:
            # 単純なマージ
            predicted = {**self._current_state, **action}

        # 制約チェック
        constraint_result = self.constraint_solver.check(predicted)

        # 副作用の特定
        side_effects = []
        for key, value in predicted.items():
            if key not in action and value != self._current_state.get(key):
                side_effects.append(f"{key}: {self._current_state.get(key)} -> {value}")

        return ActionPrediction(
            action=action,
            predicted_state=predicted,
            is_valid=constraint_result.is_valid,
            constraint_result=constraint_result,
            side_effects=side_effects,
        )

    def apply_action(
        self,
        action: dict[str, Any],
        skip_validation: bool = False,
    ) -> tuple[bool, dict[str, Any]]:
        """アクションを適用.

        Args:
            action: 適用するアクション
            skip_validation: 検証をスキップするか

        Returns:
            (成功フラグ, 新しい状態または違反情報)
        """
        if not skip_validation:
            prediction = self.predict_action(action)
            if not prediction.is_valid:
                return False, {
                    "error": "制約違反",
                    "violations": [v.model_dump() for v in (prediction.constraint_result.violations if prediction.constraint_result else [])],
                }

            new_state = prediction.predicted_state
        else:
            # 検証スキップ時は直接適用
            action_type = action.get("type") or action.get("action")
            if action_type and action_type in self._action_handlers:
                new_state = self._action_handlers[action_type](action, self._current_state)
            else:
                interventions = {k: v for k, v in action.items() if k not in ["type", "action"]}
                new_state = self.causal_model.predict_effects(interventions, self._current_state)

        # 履歴保存
        self._save_snapshot(label="before_action", action=action)

        # 状態更新
        self._current_state = new_state
        self.causal_model.update_state(new_state)

        self._logger.info(f"アクション適用: {action.get('type', action.get('action', 'unknown'))}")

        return True, new_state

    def get_valid_actions(
        self,
        candidates: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """有効なアクションをフィルタリング.

        Args:
            candidates: 候補アクションリスト

        Returns:
            制約を満たすアクションリスト
        """
        valid = []
        for action in candidates:
            prediction = self.predict_action(action)
            if prediction.is_valid:
                valid.append(action)
        return valid

    def _save_snapshot(
        self,
        label: str = "",
        action: dict[str, Any] | None = None,
    ) -> str:
        """スナップショットを保存."""
        snapshot = WorldStateSnapshot(
            state=copy.deepcopy(self._current_state),
            label=label,
            action=action,
        )
        self._history.append(snapshot)

        # 履歴サイズ制限
        while len(self._history) > self._max_history:
            self._history.pop(0)

        return snapshot.id

    def rollback(self, steps: int = 1) -> bool:
        """状態をロールバック.

        Args:
            steps: 戻るステップ数

        Returns:
            成功フラグ
        """
        if steps > len(self._history):
            self._logger.warning(f"ロールバック不可: 履歴が不足 ({len(self._history)} < {steps})")
            return False

        # 指定ステップ前のスナップショットを取得
        target_snapshot = self._history[-(steps + 1)]
        self._current_state = copy.deepcopy(target_snapshot.state)
        self.causal_model.update_state(self._current_state)

        # 履歴をトリム
        self._history = self._history[:-(steps)]

        self._logger.info(f"ロールバック完了: {steps}ステップ前に復帰")
        return True

    def get_history(self, limit: int = 10) -> list[WorldStateSnapshot]:
        """履歴を取得."""
        return self._history[-limit:]

    def check_constraints(self) -> SolverResult:
        """現在の状態に対して制約をチェック."""
        return self.constraint_solver.check(self._current_state)

    def get_summary(self) -> dict[str, Any]:
        """世界状態の概要を取得."""
        constraint_result = self.check_constraints()
        return {
            "state_keys": list(self._current_state.keys()),
            "history_length": len(self._history),
            "causal_nodes": len(self.causal_model.nodes),
            "causal_relations": len(self.causal_model.relations),
            "constraints": self.constraint_solver.get_constraint_summary(),
            "is_valid": constraint_result.is_valid,
            "constraint_score": constraint_result.score,
        }

    def reset(self) -> None:
        """世界状態をリセット."""
        self._current_state.clear()
        self._history.clear()
        self.causal_model.reset()
        self._logger.info("世界状態リセット完了")


__all__ = [
    "WorldStateSnapshot",
    "ActionPrediction",
    "WorldState",
]

