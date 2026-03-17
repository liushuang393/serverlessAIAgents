"""Layer 3 の状態管理公開 API.

Redux風の状態管理パターンを提供。

モジュール:
- store: 状態ストア
- actions: 状態アクション
- selectors: 状態セレクター
- models: 状態モデル（Goal/Fact/Decision等）
"""

from kernel.state.actions import (
    Action,
    ActionType,
    create_action,
)
from kernel.state.selectors import (
    StateSelector,
    select,
)
from kernel.state.store import (
    GlobalStateStore,
    StateSnapshot,
    StateSubscription,
)


__all__ = [
    # Actions
    "Action",
    "ActionType",
    "create_action",
    # Store
    "GlobalStateStore",
    "StateSnapshot",
    "StateSubscription",
    # Selectors
    "StateSelector",
    "select",
]

