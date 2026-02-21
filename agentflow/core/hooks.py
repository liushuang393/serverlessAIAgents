"""Lifecycle hooks for AgentFlow engine."""

from collections.abc import Awaitable, Callable
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from agentflow.core.types import ExecutionContext


class HookType(str, Enum):
    """Types of lifecycle hooks."""

    ON_START = "on_start"
    ON_NODE_EXEC = "on_node_exec"
    ON_NODE_COMPLETE = "on_node_complete"
    ON_ERROR = "on_error"
    ON_COMPLETE = "on_complete"
    ON_CANCEL = "on_cancel"


class HookEvent(BaseModel):
    """フックイベントデータ.

    フックコールバックに渡されるイベントデータを表します。
    """

    hook_type: HookType = Field(..., description="フックタイプ")
    workflow_name: str = Field(..., description="ワークフロー名")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")


# Type aliases for hook callbacks
StartHook = Callable[[ExecutionContext], Awaitable[None]]
NodeExecHook = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]
NodeCompleteHook = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]
ErrorHook = Callable[[ExecutionContext, Exception], Awaitable[None]]
CompleteHook = Callable[[ExecutionContext, dict[str, Any]], Awaitable[None]]
CancelHook = Callable[[ExecutionContext], Awaitable[None]]


class LifecycleHooks:
    """Container for lifecycle hooks.

    This class manages all lifecycle hooks for the AgentFlow engine,
    allowing users to register callbacks for various execution events.

    Example:
        >>> hooks = LifecycleHooks()
        >>> async def log_start(ctx: ExecutionContext) -> None:
        ...     print(f"Starting workflow: {ctx.workflow_id}")
        >>> hooks.register(HookType.ON_START, log_start)
    """

    def __init__(self) -> None:
        """Initialize lifecycle hooks container."""
        self._hooks: dict[HookType, list[Callable[..., Awaitable[None]]]] = {hook_type: [] for hook_type in HookType}

    def register(
        self,
        hook_type: HookType,
        callback: Callable[..., Awaitable[None]],
    ) -> None:
        """Register a hook callback.

        Args:
            hook_type: Type of hook to register.
            callback: Async callback function to execute.

        Example:
            >>> hooks = LifecycleHooks()
            >>> async def my_hook(ctx: ExecutionContext) -> None:
            ...     print(f"Hook called for {ctx.workflow_id}")
            >>> hooks.register(HookType.ON_START, my_hook)
        """
        self._hooks[hook_type].append(callback)

    def unregister(
        self,
        hook_type: HookType,
        callback: Callable[..., Awaitable[None]],
    ) -> None:
        """Unregister a hook callback.

        Args:
            hook_type: Type of hook to unregister.
            callback: Callback function to remove.

        Raises:
            ValueError: If callback is not registered.
        """
        try:
            self._hooks[hook_type].remove(callback)
        except ValueError as e:
            msg = f"Callback not registered for {hook_type}"
            raise ValueError(msg) from e

    async def trigger(
        self,
        hook_type: HookType,
        *args: Any,
        **kwargs: Any,
    ) -> None:
        """Trigger all callbacks for a hook type.

        Args:
            hook_type: Type of hook to trigger.
            *args: Positional arguments to pass to callbacks.
            **kwargs: Keyword arguments to pass to callbacks.

        Example:
            >>> hooks = LifecycleHooks()
            >>> ctx = ExecutionContext(workflow_id="test", execution_id="123")
            >>> await hooks.trigger(HookType.ON_START, ctx)
        """
        for callback in self._hooks[hook_type]:
            await callback(*args, **kwargs)

    def clear(self, hook_type: HookType | None = None) -> None:
        """Clear hooks.

        Args:
            hook_type: Specific hook type to clear. If None, clears all hooks.

        Example:
            >>> hooks = LifecycleHooks()
            >>> hooks.clear(HookType.ON_START)  # Clear only ON_START hooks
            >>> hooks.clear()  # Clear all hooks
        """
        if hook_type is None:
            for ht in HookType:
                self._hooks[ht].clear()
        else:
            self._hooks[hook_type].clear()

    def get_hooks(self, hook_type: HookType) -> list[Callable[..., Awaitable[None]]]:
        """Get all callbacks for a hook type.

        Args:
            hook_type: Type of hook to get callbacks for.

        Returns:
            List of registered callbacks.

        Example:
            >>> hooks = LifecycleHooks()
            >>> callbacks = hooks.get_hooks(HookType.ON_START)
            >>> print(len(callbacks))
            0
        """
        return self._hooks[hook_type].copy()
