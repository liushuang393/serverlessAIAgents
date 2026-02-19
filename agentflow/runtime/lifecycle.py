"""Lifecycle - 生命周期钩子.

应用和请求生命周期管理。

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
    """生命周期阶段."""

    STARTUP = "startup"
    READY = "ready"
    RUNNING = "running"
    SHUTDOWN = "shutdown"
    STOPPED = "stopped"


class HookPriority(int, Enum):
    """钩子优先级."""

    HIGHEST = 0
    HIGH = 25
    NORMAL = 50
    LOW = 75
    LOWEST = 100


@dataclass
class HookInfo:
    """钩子信息.

    Attributes:
        name: 钩子名称
        handler: 处理函数
        priority: 优先级
        timeout: 超时时间（秒）
        required: 是否必需
    """

    name: str
    handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any]
    priority: int = HookPriority.NORMAL
    timeout: float = 30.0
    required: bool = True

    async def execute(self, *args: Any, **kwargs: Any) -> Any:
        """执行钩子."""
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
    """生命周期事件.

    Attributes:
        phase: 阶段
        timestamp: 时间戳
        duration_ms: 持续时间
        metadata: 元数据
    """

    phase: LifecyclePhase
    timestamp: float = field(default_factory=time.time)
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "phase": self.phase.value,
            "timestamp": self.timestamp,
            "duration_ms": self.duration_ms,
            "metadata": self.metadata,
        }


class LifecycleManager:
    """生命周期管理器.

    管理应用和请求的生命周期钩子。
    """

    def __init__(self) -> None:
        """初始化."""
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
        """当前阶段."""
        return self._phase

    @property
    def events(self) -> list[LifecycleEvent]:
        """事件历史."""
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
        """添加钩子."""
        hook = HookInfo(
            name=name or handler.__name__,
            handler=handler,
            priority=priority,
            timeout=timeout,
            required=required,
        )
        hooks.append(hook)
        # 按优先级排序
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
        """注册启动钩子."""
        self._add_hook(self._startup_hooks, handler, name, priority, timeout, required)

    def on_shutdown(
        self,
        handler: Callable[..., Coroutine[Any, Any, Any]] | Callable[..., Any],
        *,
        name: str | None = None,
        priority: int = HookPriority.NORMAL,
        timeout: float = 30.0,
        required: bool = False,  # 默认shutdown不强制
    ) -> None:
        """注册关闭钩子."""
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
        """注册请求开始钩子."""
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
        """注册请求结束钩子."""
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
        """注册错误钩子."""
        self._add_hook(self._error_hooks, handler, name, priority, timeout, required)

    async def _run_hooks(
        self,
        hooks: list[HookInfo],
        *args: Any,
        **kwargs: Any,
    ) -> list[Any]:
        """运行钩子列表."""
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
        """执行启动流程."""
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
        """执行关闭流程."""
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
        """请求开始."""
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
        """请求结束."""
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
        """处理错误."""
        await self._run_hooks(
            self._error_hooks,
            error=error,
            request_id=request_id,
            **metadata,
        )


# 全局实例
_lifecycle_manager = LifecycleManager()


def get_lifecycle_manager() -> LifecycleManager:
    """获取全局生命周期管理器."""
    return _lifecycle_manager


__all__ = [
    "HookInfo",
    "HookPriority",
    "LifecycleEvent",
    "LifecycleManager",
    "LifecyclePhase",
    "get_lifecycle_manager",
]
