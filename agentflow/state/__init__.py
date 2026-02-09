"""グローバル状態管理層.

Redux風の状態管理パターンを提供。

モジュール:
- store: 状態ストア
- actions: 状態アクション
- selectors: 状態セレクター
"""

from agentflow.state.actions import (
    Action,
    ActionType,
    create_action,
)
from agentflow.state.selectors import (
    StateSelector,
    select,
)
from agentflow.state.store import (
    GlobalStateStore,
    StateSnapshot,
    StateSubscription,
)


__all__ = [
    # Actions
    "Action",
    "ActionType",
    # Store
    "GlobalStateStore",
    # Selectors
    "StateSelector",
    "StateSnapshot",
    "StateSubscription",
    "create_action",
    "select",
]
