"""Request Scope - 请求级作用域管理.

为每个请求提供隔离的资源作用域。

使用例:
    >>> async with RequestScope.create(request_id="req-123") as scope:
    ...     scope.set("user_id", "user-456")
    ...     result = await some_operation()
"""

from __future__ import annotations

import asyncio
import logging
import time
from contextlib import asynccontextmanager
from contextvars import ContextVar
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar
from uuid import uuid4


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


_logger = logging.getLogger(__name__)

T = TypeVar("T")


@dataclass
class RequestScope:
    """请求级作用域.

    提供请求级别的资源隔离和生命周期管理。

    Attributes:
        request_id: 请求ID
        tenant_id: 租户ID
        start_time: 开始时间
        data: 请求级数据存储
        resources: 已分配资源
    """

    request_id: str
    tenant_id: str | None = None
    start_time: float = field(default_factory=time.time)
    data: dict[str, Any] = field(default_factory=dict)
    resources: list[Any] = field(default_factory=list)
    _cleanup_tasks: list[Any] = field(default_factory=list)

    def set(self, key: str, value: Any) -> None:
        """设置请求级数据."""
        self.data[key] = value

    def get(self, key: str, default: T | None = None) -> T | None:
        """获取请求级数据."""
        return self.data.get(key, default)

    def delete(self, key: str) -> bool:
        """删除请求级数据."""
        if key in self.data:
            del self.data[key]
            return True
        return False

    def register_resource(
        self,
        resource: Any,
        cleanup: Any | None = None,
    ) -> None:
        """注册需要清理的资源.

        Args:
            resource: 资源对象
            cleanup: 清理函数（可选）
        """
        self.resources.append(resource)
        if cleanup:
            self._cleanup_tasks.append(cleanup)

    async def cleanup(self) -> None:
        """清理所有资源."""
        for task in self._cleanup_tasks:
            try:
                if asyncio.iscoroutinefunction(task):
                    await task()
                else:
                    task()
            except Exception as e:
                _logger.warning(f"Resource cleanup failed: {e}")

        self.resources.clear()
        self._cleanup_tasks.clear()

    @property
    def elapsed_ms(self) -> float:
        """获取已用时间（毫秒）."""
        return (time.time() - self.start_time) * 1000

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "request_id": self.request_id,
            "tenant_id": self.tenant_id,
            "start_time": self.start_time,
            "elapsed_ms": self.elapsed_ms,
            "data_keys": list(self.data.keys()),
            "resource_count": len(self.resources),
        }

    @classmethod
    @asynccontextmanager
    async def create(
        cls,
        request_id: str | None = None,
        tenant_id: str | None = None,
    ) -> AsyncIterator[RequestScope]:
        """创建请求作用域.

        Args:
            request_id: 请求ID（自动生成如果未提供）
            tenant_id: 租户ID

        Yields:
            请求作用域
        """
        scope = cls(
            request_id=request_id or str(uuid4()),
            tenant_id=tenant_id,
        )

        # 设置上下文
        token = _current_scope.set(scope)
        _logger.debug(f"Request scope created: {scope.request_id}")

        try:
            yield scope
        finally:
            # 清理
            await scope.cleanup()
            _current_scope.reset(token)
            _logger.debug(
                f"Request scope completed: {scope.request_id} "
                f"({scope.elapsed_ms:.1f}ms)"
            )


# 当前请求作用域
_current_scope: ContextVar[RequestScope | None] = ContextVar(
    "agentflow_request_scope", default=None
)


def get_current_scope() -> RequestScope | None:
    """获取当前请求作用域."""
    return _current_scope.get()


def get_current_scope_or_raise() -> RequestScope:
    """获取当前请求作用域（不存在则抛出异常）."""
    scope = get_current_scope()
    if scope is None:
        msg = "No active request scope"
        raise RuntimeError(msg)
    return scope


def require_scope() -> RequestScope:
    """获取当前作用域（别名）."""
    return get_current_scope_or_raise()


__all__ = [
    "RequestScope",
    "get_current_scope",
    "get_current_scope_or_raise",
    "require_scope",
]
