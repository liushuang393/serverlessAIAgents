"""状態セレクター.

状態から必要な部分を効率的に取得するセレクターを提供。

設計原則:
- メモ化: 同じ入力に対して同じ結果を返す
- 合成可能: セレクターを組み合わせて新しいセレクターを作成
- 型安全: 厳密な型定義

使用例:
    >>> from agentflow.state.selectors import select, StateSelector
    >>>
    >>> # 単純な選択
    >>> progress = select(state, "execution.progress")
    >>>
    >>> # セレクター関数
    >>> selector = StateSelector("execution.progress")
    >>> progress = selector(state)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, TypeVar


if TYPE_CHECKING:
    from collections.abc import Callable


T = TypeVar("T")


@dataclass
class StateSelector:
    """状態セレクター.

    ドット区切りのパスで状態の一部を選択。

    Attributes:
        path: 選択パス（例: "execution.progress"）
        default: デフォルト値
    """

    path: str
    default: Any = None

    def __call__(self, state: dict[str, Any]) -> Any:
        """状態から値を選択.

        Args:
            state: 状態辞書

        Returns:
            選択された値
        """
        return select(state, self.path, self.default)


def select(
    state: dict[str, Any],
    path: str,
    default: Any = None,
) -> Any:
    """状態からパスで値を選択.

    ドット区切りのパスで状態をトラバースする。

    Args:
        state: 状態辞書
        path: ドット区切りのパス
        default: デフォルト値

    Returns:
        選択された値、または デフォルト値

    Example:
        >>> state = {"execution": {"progress": 0.5}}
        >>> select(state, "execution.progress")
        0.5
    """
    if not path:
        return state

    parts = path.split(".")
    current = state

    for part in parts:
        if isinstance(current, dict):
            if part in current:
                current = current[part]
            else:
                return default
        elif isinstance(current, list):
            try:
                index = int(part)
                current = current[index]
            except (ValueError, IndexError):
                return default
        else:
            return default

    return current


def create_selector[T](
    path: str,
    transform: Callable[[Any], T] | None = None,
    default: Any = None,
) -> Callable[[dict[str, Any]], T]:
    """セレクター関数を作成.

    Args:
        path: 選択パス
        transform: 変換関数（オプション）
        default: デフォルト値

    Returns:
        セレクター関数
    """
    def selector(state: dict[str, Any]) -> T:
        value = select(state, path, default)
        if transform and value is not None:
            return transform(value)
        return value

    return selector


def compose_selectors(
    *selectors: Callable[[dict[str, Any]], Any],
) -> Callable[[dict[str, Any]], dict[str, Any]]:
    """複数のセレクターを合成.

    Args:
        *selectors: セレクター関数

    Returns:
        合成されたセレクター
    """
    def composed(state: dict[str, Any]) -> dict[str, Any]:
        return {
            f"value_{i}": selector(state)
            for i, selector in enumerate(selectors)
        }

    return composed


# 便利なセレクター

def select_execution_status(state: dict[str, Any]) -> str:
    """実行状態を選択."""
    return select(state, "execution.status", "unknown")


def select_progress(state: dict[str, Any]) -> float:
    """進捗を選択."""
    return select(state, "execution.progress", 0.0)


def select_current_step(state: dict[str, Any]) -> str | None:
    """現在のステップを選択."""
    return select(state, "execution.current_step")


def select_context(state: dict[str, Any]) -> dict[str, Any]:
    """コンテキストを選択."""
    return select(state, "context", {})


def select_results(state: dict[str, Any]) -> dict[str, Any]:
    """結果を選択."""
    return select(state, "results", {})


def select_error(state: dict[str, Any]) -> str | None:
    """エラーを選択."""
    return select(state, "execution.error")


def select_plan(state: dict[str, Any]) -> dict[str, Any] | None:
    """計画を選択."""
    return select(state, "plan")


def select_history(state: dict[str, Any], limit: int = 10) -> list[dict[str, Any]]:
    """履歴を選択."""
    history = select(state, "history", [])
    return history[-limit:] if isinstance(history, list) else []


def select_checkpoints(state: dict[str, Any]) -> list[str]:
    """チェックポイント一覧を選択."""
    return select(state, "checkpoints", [])


def select_pending_approvals(state: dict[str, Any]) -> list[dict[str, Any]]:
    """保留中の承認を選択."""
    return select(state, "hitl.pending_approvals", [])


# エクスポート
__all__ = [
    "StateSelector",
    "compose_selectors",
    "create_selector",
    "select",
    "select_checkpoints",
    "select_context",
    "select_current_step",
    "select_error",
    "select_execution_status",
    "select_history",
    "select_pending_approvals",
    "select_plan",
    "select_progress",
    "select_results",
]
