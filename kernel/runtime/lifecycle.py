"""Lifecycle - 生命周期钩子.

応用とリクエストのライフサイクル管理。

使用例:
    >>> lifecycle = LifecycleManager()
    >>> lifecycle.on_startup(initialize_database)
    >>> lifecycle.on_shutdown(cleanup_connections)
    >>> lifecycle.on_request_start(log_request)
    >>> lifecycle.on_request_end(log_response)
"""

from __future__ import annotations

import asyncio
import logging
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Callable, Coroutine


_logger = logging.getLogger(__name__)


class LifecyclePhase(str, Enum):
    """ライフサイクルフェーズ."""

    STARTUP = "startup"
    READY = "ready"
    RUNNING = "running"
    SHUTDOWN = "shutdown"
    STOPPED = "stopped"


class HookPriority(int, Enum):
    """フック優先度."""

    HIGHEST = 0
    HIGH = 25
    NORMAL = 50
    LOW = 75
    LOWEST = 100


@dataclass
class HookInfo:
    """フック情報.

    Attributes:
        name: フック名
        handler: ハンドラ関数
        priority: 優先度
        timeout: タイムアウト（秒）
        required: 必須フラグ
    """

    name: str
    handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any]
    priority: int = HookPriority.NORMAL
    timeout: float = 30.0
    required: bool = True

    async def execute(self, *args: Any, **kwargs: Any) -> Any:
        """フックを実行."""
        try:
            if asyncio.iscoroutinefunction(self.handler):
                result = await asyncio.wait_for(
                    self.handler(*args, **kwargs),
                    timeout=self.timeout,
                )
            else:
                result = self.handler(*args, **kwargs)
            return result
        except TimeoutError:
            _logger.exception(f"Hook '{self.name}' timed out after {self.timeout}s")
            if self.required:
                raise
            return None
        except Exception as e:
            _logger.exception(f"Hook '{self.name}' failed: {e}")
            if self.required:
                raise
            return None


@dataclass
class LifecycleEvent:
    """ライフサイクルイベント.

    Attributes:
        phase: フェーズ
        timestamp: タイムスタンプ
        duration_ms: 経過時間
        metadata: メタデータ
    """

    phase: LifecyclePhase
    timestamp: float = field(default_factory=time.time)
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "phase": self.phase.value,
            "timestamp": self.timestamp,
            "duration_ms": self.duration_ms,
            "metadata": self.metadata,
        }


class LifecycleManager:
    """ライフサイクル管理器.

    アプリケーションとリクエストのライフサイクルフックを管理。
    """

    def __init__(self) -> None:
        """初期化."""
        self._phase = LifecyclePhase.STOPPED
        self._startup_hooks: list[HookInfo] = []
        self._shutdown_hooks: list[HookInfo] = []
        self._request_start_hooks: list[HookInfo] = []
        self._request_end_hooks: list[HookInfo] = []
        self._error_hooks: list[HookInfo] = []
        self._events: list[LifecycleEvent] = []
        self._lock = asyncio.Lock()

    @property
    def phase(self) -> LifecyclePhase:
        """現在のフェーズ."""
        return self._phase

    @property
    def events(self) -> list[LifecycleEvent]:
        """イベント履歴."""
        return list(self._events)

    def _add_hook(
        self,
        hooks: list[HookInfo],
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 30.0,
        required: bool = True,
    ) -> None:
        """フックを追加."""
        hook = HookInfo(
            name=name or handler.__name__,
            handler=handler,
            priority=priority,
            timeout=timeout,
            required=required,
        )
        hooks.append(hook)
        hooks.sort(key=lambda h: h.priority)

    def on_startup(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 30.0,
        required: bool = True,
    ) -> None:
        """起動フックを登録."""
        self._add_hook(self._startup_hooks, handler, name, priority, timeout, required)

    def on_shutdown(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 30.0,
        required: bool = False,
    ) -> None:
        """停止フックを登録."""
        self._add_hook(self._shutdown_hooks, handler, name, priority, timeout, required)

    def on_request_start(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 5.0,
        required: bool = False,
    ) -> None:
        """リクエスト開始フックを登録."""
        self._add_hook(self._request_start_hooks, handler, name, priority, timeout, required)

    def on_request_end(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 5.0,
        required: bool = False,
    ) -> None:
        """リクエスト終了フックを登録."""
        self._add_hook(self._request_end_hooks, handler, name, priority, timeout, required)

    def on_error(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 5.0,
        required: bool = False,
    ) -> None:
        """エラーフックを登録."""
        self._add_hook(self._error_hooks, handler, name, priority, timeout, required)

    async def _run_hooks(
        self,
        hooks: list[HookInfo],
        *args: Any,
        **kwargs: Any,
    ) -> list[Any]:
        """フックリストを実行."""
        results = []
        for hook in hooks:
            try:
                result = await hook.execute(*args, **kwargs)
                results.append(result)
            except Exception as e:
                _logger.exception(f"Hook '{hook.name}' failed: {e}")
                if hook.required:
                    raise
        return results

    async def startup(self) -> None:
        """起動処理を実行."""
        async with self._lock:
            if self._phase != LifecyclePhase.STOPPED:
                _logger.warning(f"Cannot startup from phase {self._phase}")
                return

            start_time = time.time()
            self._phase = LifecyclePhase.STARTUP

            try:
                await self._run_hooks(self._startup_hooks)
                self._phase = LifecyclePhase.READY
                duration = (time.time() - start_time) * 1000

                event = LifecycleEvent(
                    phase=LifecyclePhase.STARTUP,
                    duration_ms=duration,
                )
                self._events.append(event)
                _logger.info(f"Startup completed in {duration:.1f}ms")

            except Exception as e:
                self._phase = LifecyclePhase.STOPPED
                _logger.exception(f"Startup failed: {e}")
                raise

    async def shutdown(self) -> None:
        """停止処理を実行."""
        async with self._lock:
            if self._phase == LifecyclePhase.STOPPED:
                return

            start_time = time.time()
            self._phase = LifecyclePhase.SHUTDOWN

            try:
                await self._run_hooks(self._shutdown_hooks)
            except Exception as e:
                _logger.exception(f"Shutdown error: {e}")
            finally:
                self._phase = LifecyclePhase.STOPPED
                duration = (time.time() - start_time) * 1000

                event = LifecycleEvent(
                    phase=LifecyclePhase.SHUTDOWN,
                    duration_ms=duration,
                )
                self._events.append(event)
                _logger.info(f"Shutdown completed in {duration:.1f}ms")

    async def request_start(
        self,
        request_id: str,
        **metadata: Any,
    ) -> None:
        """リクエスト開始."""
        await self._run_hooks(
            self._request_start_hooks,
            request_id=request_id,
            **metadata,
        )

    async def request_end(
        self,
        request_id: str,
        duration_ms: float,
        success: bool = True,
        **metadata: Any,
    ) -> None:
        """リクエスト終了."""
        await self._run_hooks(
            self._request_end_hooks,
            request_id=request_id,
            duration_ms=duration_ms,
            success=success,
            **metadata,
        )

    async def handle_error(
        self,
        error: Exception,
        request_id: str | None = None,
        **metadata: Any,
    ) -> None:
        """エラー処理."""
        await self._run_hooks(
            self._error_hooks,
            error=error,
            request_id=request_id,
            **metadata,
        )


# グローバルインスタンス
_lifecycle_manager = LifecycleManager()


def get_lifecycle_manager() -> LifecycleManager:
    """グローバルライフサイクル管理器を取得."""
    return _lifecycle_manager


__all__ = [
    "HookInfo",
    "HookPriority",
    "LifecycleEvent",
    "LifecycleManager",
    "LifecyclePhase",
    "get_lifecycle_manager",
]
