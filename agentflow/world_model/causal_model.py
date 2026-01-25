# -*- coding: utf-8 -*-
"""因果モデル - 状態遷移と因果関係の明示的表現.

Hassabisの「世界モデル」に相当する因果関係グラフを実装。
行動の結果を予測し、目標達成のための介入計画を立案する。

設計原則:
- 因果関係を明示的なグラフで表現
- 行動→結果の予測を可能に
- 結果→原因の推論を可能に

使用例:
    >>> causal = CausalModel()
    >>> causal.add_node("temperature", initial_value=20)
    >>> causal.add_node("heater_on", initial_value=False)
    >>> causal.add_relation(
    ...     cause_id="heater_on",
    ...     effect_id="temperature",
    ...     relation_type=RelationType.INCREASES,
    ...     effect_fn=lambda heater, temp: temp + 5 if heater else temp
    ... )
    >>> # ヒーターをONにした場合の温度を予測
    >>> predicted = causal.predict_effects({"heater_on": True})
    >>> print(predicted["temperature"])  # 25
"""

from __future__ import annotations

import logging
import uuid
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class RelationType(str, Enum):
    """因果関係のタイプ."""

    INCREASES = "increases"       # 原因が増えると結果が増える
    DECREASES = "decreases"       # 原因が増えると結果が減る
    ENABLES = "enables"           # 原因が真なら結果が可能
    PREVENTS = "prevents"         # 原因が真なら結果が不可能
    TRIGGERS = "triggers"         # 原因が発生すると結果が発生
    CUSTOM = "custom"             # カスタム関数で定義


class CausalNode(BaseModel):
    """因果グラフのノード（状態変数）.

    Attributes:
        id: ノードID
        name: ノード名（状態キー）
        description: 説明
        initial_value: 初期値
        value_type: 値の型（"numeric", "boolean", "string", "object"）
        domain: 値の範囲（数値の場合は[min, max]、離散値の場合はリスト）
    """

    id: str = Field(default_factory=lambda: f"node-{uuid.uuid4().hex[:8]}")
    name: str = Field(..., description="状態キー名")
    description: str = Field(default="", description="説明")
    initial_value: Any = Field(default=None, description="初期値")
    value_type: str = Field(default="numeric", description="値の型")
    domain: list[Any] | None = Field(default=None, description="値の範囲")


class CausalRelation(BaseModel):
    """因果関係.

    Attributes:
        id: 関係ID
        cause_id: 原因ノードID
        effect_id: 結果ノードID
        relation_type: 関係タイプ
        confidence: 信頼度（0.0-1.0）
        delay: 効果の遅延（ステップ数）
        conditions: 関係が有効な条件
    """

    id: str = Field(default_factory=lambda: f"rel-{uuid.uuid4().hex[:8]}")
    cause_id: str = Field(..., description="原因ノードID")
    effect_id: str = Field(..., description="結果ノードID")
    relation_type: RelationType = Field(default=RelationType.CUSTOM)
    confidence: float = Field(default=1.0, ge=0.0, le=1.0)
    delay: int = Field(default=0, ge=0, description="効果の遅延ステップ数")
    conditions: dict[str, Any] = Field(default_factory=dict, description="有効条件")


@dataclass
class CausalModel:
    """因果モデル - 状態遷移の因果関係を管理.

    主な機能:
    - ノード（状態変数）と関係（因果）の管理
    - 行動の結果予測
    - 原因の特定
    - 介入計画の立案
    """

    nodes: dict[str, CausalNode] = field(default_factory=dict)
    relations: list[CausalRelation] = field(default_factory=list)
    effect_functions: dict[str, Callable[..., Any]] = field(default_factory=dict)
    _current_state: dict[str, Any] = field(default_factory=dict)
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.world_model.causal")
    )

    def add_node(
        self,
        name: str,
        initial_value: Any = None,
        description: str = "",
        value_type: str = "numeric",
        domain: list[Any] | None = None,
    ) -> CausalNode:
        """ノードを追加.

        Args:
            name: 状態キー名
            initial_value: 初期値
            description: 説明
            value_type: 値の型
            domain: 値の範囲

        Returns:
            作成されたCausalNode
        """
        node = CausalNode(
            name=name,
            description=description,
            initial_value=initial_value,
            value_type=value_type,
            domain=domain,
        )
        self.nodes[node.id] = node
        self._current_state[name] = initial_value
        self._logger.debug(f"ノード追加: {name} = {initial_value}")
        return node

    def add_relation(
        self,
        cause_id: str,
        effect_id: str,
        relation_type: RelationType = RelationType.CUSTOM,
        effect_fn: Callable[..., Any] | None = None,
        confidence: float = 1.0,
        delay: int = 0,
        conditions: dict[str, Any] | None = None,
    ) -> CausalRelation:
        """因果関係を追加.

        Args:
            cause_id: 原因ノードの名前またはID
            effect_id: 結果ノードの名前またはID
            relation_type: 関係タイプ
            effect_fn: 効果関数 (cause_value, effect_value, state) -> new_effect_value
            confidence: 信頼度
            delay: 効果の遅延
            conditions: 有効条件

        Returns:
            作成されたCausalRelation
        """
        # 名前からIDを解決
        cause_node_id = self._resolve_node_id(cause_id)
        effect_node_id = self._resolve_node_id(effect_id)

        relation = CausalRelation(
            cause_id=cause_node_id,
            effect_id=effect_node_id,
            relation_type=relation_type,
            confidence=confidence,
            delay=delay,
            conditions=conditions or {},
        )
        self.relations.append(relation)

        if effect_fn:
            self.effect_functions[relation.id] = effect_fn

        self._logger.debug(f"関係追加: {cause_id} -> {effect_id}")
        return relation

    def _resolve_node_id(self, name_or_id: str) -> str:
        """ノード名またはIDからIDを解決."""
        # まずIDとして検索
        if name_or_id in self.nodes:
            return name_or_id
        # 名前として検索
        for node_id, node in self.nodes.items():
            if node.name == name_or_id:
                return node_id
        raise ValueError(f"ノードが見つかりません: {name_or_id}")

    def get_node_by_name(self, name: str) -> CausalNode | None:
        """名前でノードを取得."""
        for node in self.nodes.values():
            if node.name == name:
                return node
        return None

    def predict_effects(
        self,
        interventions: dict[str, Any],
        current_state: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """介入（アクション）の結果を予測.

        Args:
            interventions: 介入する状態変数と値 {"variable_name": new_value}
            current_state: 現在の状態（省略時は内部状態を使用）

        Returns:
            予測される状態
        """
        state = dict(current_state or self._current_state)

        # 介入を適用
        state.update(interventions)

        # 因果関係に基づいて結果を計算
        changed = True
        iterations = 0
        max_iterations = len(self.relations) * 2 + 1

        while changed and iterations < max_iterations:
            changed = False
            iterations += 1

            for relation in self.relations:
                if relation.delay > 0:
                    continue  # 遅延のある関係はスキップ

                cause_node = self.nodes.get(relation.cause_id)
                effect_node = self.nodes.get(relation.effect_id)

                if not cause_node or not effect_node:
                    continue

                cause_value = state.get(cause_node.name)
                effect_value = state.get(effect_node.name)

                # 効果関数を適用
                effect_fn = self.effect_functions.get(relation.id)
                if effect_fn:
                    new_value = effect_fn(cause_value, effect_value, state)
                else:
                    new_value = self._apply_default_effect(
                        relation.relation_type, cause_value, effect_value
                    )

                if new_value != effect_value:
                    state[effect_node.name] = new_value
                    changed = True

        return state

    def _apply_default_effect(
        self,
        relation_type: RelationType,
        cause_value: Any,
        effect_value: Any,
    ) -> Any:
        """デフォルトの効果関数を適用."""
        if relation_type == RelationType.INCREASES:
            if isinstance(cause_value, bool) and cause_value:
                return effect_value + 1 if isinstance(effect_value, (int, float)) else effect_value
            return effect_value
        elif relation_type == RelationType.DECREASES:
            if isinstance(cause_value, bool) and cause_value:
                return effect_value - 1 if isinstance(effect_value, (int, float)) else effect_value
            return effect_value
        elif relation_type == RelationType.ENABLES:
            return cause_value
        elif relation_type == RelationType.PREVENTS:
            return not cause_value if isinstance(effect_value, bool) else effect_value
        elif relation_type == RelationType.TRIGGERS:
            return cause_value
        return effect_value

    def find_causes(self, effect_name: str) -> list[CausalNode]:
        """結果に影響を与える原因ノードを特定.

        Args:
            effect_name: 結果ノードの名前

        Returns:
            原因ノードのリスト
        """
        effect_node = self.get_node_by_name(effect_name)
        if not effect_node:
            return []

        causes = []
        for relation in self.relations:
            if relation.effect_id == effect_node.id:
                cause_node = self.nodes.get(relation.cause_id)
                if cause_node:
                    causes.append(cause_node)
        return causes

    def get_intervention_plan(
        self,
        goal: dict[str, Any],
        current_state: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """目標達成のための介入計画を立案.

        簡易的な逆向き推論で、目標状態を達成するために必要な介入を特定。

        Args:
            goal: 目標状態 {"variable_name": target_value}
            current_state: 現在の状態

        Returns:
            介入リスト [{"variable": name, "value": value, "reason": str}]
        """
        state = dict(current_state or self._current_state)
        interventions = []

        for var_name, target_value in goal.items():
            current_value = state.get(var_name)

            if current_value == target_value:
                continue  # 既に目標達成

            # この変数に影響を与える原因を特定
            causes = self.find_causes(var_name)

            if not causes:
                # 直接介入が必要
                interventions.append({
                    "variable": var_name,
                    "value": target_value,
                    "reason": "直接介入（因果関係なし）",
                })
            else:
                # 原因への介入を検討
                for cause in causes:
                    interventions.append({
                        "variable": cause.name,
                        "value": True,  # 簡易: 原因を有効化
                        "reason": f"{cause.name} を変更して {var_name} に影響",
                    })

        return interventions

    def update_state(self, updates: dict[str, Any]) -> dict[str, Any]:
        """内部状態を更新.

        Args:
            updates: 更新する状態

        Returns:
            更新後の状態
        """
        self._current_state.update(updates)
        return dict(self._current_state)

    def get_state(self) -> dict[str, Any]:
        """現在の内部状態を取得."""
        return dict(self._current_state)

    def reset(self) -> None:
        """状態を初期値にリセット."""
        for node in self.nodes.values():
            self._current_state[node.name] = node.initial_value


__all__ = [
    "RelationType",
    "CausalNode",
    "CausalRelation",
    "CausalModel",
]

