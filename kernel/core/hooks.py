"""ライフサイクルフック — kernel 層.

AgentFlow エンジンのライフサイクルフックを管理する。
legacy core surface/hooks.py から移行。
"""

from collections.abc import Awaitable, Callable
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from kernel.core.types import ExecutionContext


class HookType(str, Enum):
    """ライフサイクルフックタイプ."""

    ON_START = "on_start"
    ON_NODE_EXEC = "on_node_exec"
    ON_NODE_COMPLETE = "on_node_complete"
    ON_ERROR = "on_error"
    ON_COMPLETE = "on_complete"
    ON_CANCEL = "on_cancel"


class HookEvent(BaseModel):
    """フックイベントデータ."""

    hook_type: HookType = Field(..., description="フックタイプ")
    workflow_name: str = Field(..., description="ワークフロー名")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")


# フックコールバック型エイリアス
StartHook = Callable[[ExecutionContext], Awaitable[None]]
NodeExecHook = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]
NodeCompleteHook = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]
ErrorHook = Callable[[ExecutionContext, Exception], Awaitable[None]]
CompleteHook = Callable[[ExecutionContext, dict[str, Any]], Awaitable[None]]
CancelHook = Callable[[ExecutionContext], Awaitable[None]]


class LifecycleHooks:
    """ライフサイクルフックコンテナ.

    各フックタイプにコールバックを登録し、トリガーする。
    """

    def __init__(self) -> None:
        """ライフサイクルフックコンテナを初期化."""
        self._hooks: dict[HookType, list[Callable[..., Awaitable[None]]]] = {hook_type: [] for hook_type in HookType}

    def register(self, hook_type: HookType, callback: Callable[..., Awaitable[None]]) -> None:
        """フックコールバックを登録.

        Args:
            hook_type: フックタイプ
            callback: 非同期コールバック関数
        """
        self._hooks[hook_type].append(callback)

    def unregister(self, hook_type: HookType, callback: Callable[..., Awaitable[None]]) -> None:
        """フックコールバックを解除.

        Args:
            hook_type: フックタイプ
            callback: 解除するコールバック

        Raises:
            ValueError: コールバックが未登録の場合
        """
        try:
            self._hooks[hook_type].remove(callback)
        except ValueError as e:
            msg = f"Callback not registered for {hook_type}"
            raise ValueError(msg) from e

    async def trigger(self, hook_type: HookType, *args: Any, **kwargs: Any) -> None:
        """指定タイプの全コールバックをトリガー.

        Args:
            hook_type: フックタイプ
            *args: コールバックへの位置引数
            **kwargs: コールバックへのキーワード引数
        """
        for callback in self._hooks[hook_type]:
            await callback(*args, **kwargs)

    def clear(self, hook_type: HookType | None = None) -> None:
        """フックをクリア.

        Args:
            hook_type: クリアするフックタイプ。None の場合は全フックをクリア
        """
        if hook_type is None:
            for ht in HookType:
                self._hooks[ht].clear()
        else:
            self._hooks[hook_type].clear()

    def get_hooks(self, hook_type: HookType) -> list[Callable[..., Awaitable[None]]]:
        """指定タイプの全コールバックを取得.

        Args:
            hook_type: フックタイプ

        Returns:
            登録済みコールバックのコピー
        """
        return self._hooks[hook_type].copy()
