"""World Model層 - 状態・因果・制約の明示的表現.

Hassabisの示唆に基づく世界モデル実装：
- CausalModel: 因果関係グラフによる状態遷移の理解
- WorldState: 現在状態 + 予測状態 + 制約の統合管理
- ConstraintSolver: 制約充足問題の解決

設計原則:
- LLM推論だけでなく、明示的な状態・因果・制約を表現
- 行動の結果を予測し、計画の妥当性を検証
- 制約違反を事前に検出し、無効なアクションを防止

使用例:
    >>> from agentflow.world_model import WorldState, CausalModel
    >>>
    >>> # 因果モデルを構築
    >>> causal = CausalModel()
    >>> causal.add_node("budget", initial_value=100000)
    >>> causal.add_node("purchase_cost", initial_value=0)
    >>> causal.add_relation("purchase_cost", "budget", effect_fn=lambda cost, budget: budget - cost)
    >>>
    >>> # 世界状態を管理
    >>> world = WorldState(causal_model=causal)
    >>> world.add_constraint("budget", lambda v: v >= 0, "予算は0以上")
    >>>
    >>> # アクションの結果を予測
    >>> prediction = world.predict_action({"action": "purchase", "cost": 50000})
    >>> print(prediction.is_valid)  # True（予算内）
    >>>
    >>> prediction = world.predict_action({"action": "purchase", "cost": 200000})
    >>> print(prediction.is_valid)  # False（予算超過）
"""

from agentflow.world_model.causal_model import (
    CausalModel,
    CausalNode,
    CausalRelation,
    RelationType,
)
from agentflow.world_model.constraint_solver import (
    ConstraintSolver,
    ConstraintViolation,
    SolverResult,
)
from agentflow.world_model.world_state import (
    ActionPrediction,
    WorldState,
    WorldStateSnapshot,
)


__all__ = [
    "ActionPrediction",
    # Causal Model
    "CausalModel",
    "CausalNode",
    "CausalRelation",
    # Constraint Solver
    "ConstraintSolver",
    "ConstraintViolation",
    "RelationType",
    "SolverResult",
    # World State
    "WorldState",
    "WorldStateSnapshot",
]
