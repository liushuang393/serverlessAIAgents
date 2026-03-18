"""Request Scope - リクエスト級スコープ管理.

各リクエストに隔離されたリソーススコープを提供。

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
    """リクエスト級スコープ.

    リクエストレベルのリソース隔離とライフサイクル管理を提供。

    Attributes:
        request_id: リクエストID
        tenant_id: テナントID
        start_time: 開始時刻
        data: リクエスト級データ
        resources: 割り当て済みリソース
    """

    request_id: str
    tenant_id: str | None = None
    start_time: float = field(default_factory=time.time)
    data: dict[str, Any] = field(default_factory=dict)
    resources: list[Any] = field(default_factory=list)
    _cleanup_tasks: list[Any] = field(default_factory=list)

    def set(self, key: str, value: Any) -> None:
        """リクエスト級データを設定."""
        self.data[key] = value

    def get(self, key: str, default: T | None = None) -> T | None:
        """リクエスト級データを取得."""
        return self.data.get(key, default)

    def delete(self, key: str) -> bool:
        """リクエスト級データを削除."""
        if key in self.data:
            del self.data[key]
            return True
        return False

    def register_resource(
        self,
        resource: Any,
        cleanup: Any | None = None,
    ) -> None:
        """クリーンアップが必要なリソースを登録.

        Args:
            resource: リソースオブジェクト
            cleanup: クリーンアップ関数（省略可）
        """
        self.resources.append(resource)
        if cleanup:
            self._cleanup_tasks.append(cleanup)

    async def cleanup(self) -> None:
        """全リソースをクリーンアップ."""
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
        """経過時間（ミリ秒）."""
        return (time.time() - self.start_time) * 1000

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
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
        """リクエストスコープを作成.

        Args:
            request_id: リクエストID（未指定時自動生成）
            tenant_id: テナントID

        Yields:
            リクエストスコープ
        """
        scope = cls(
            request_id=request_id or str(uuid4()),
            tenant_id=tenant_id,
        )

        token = _current_scope.set(scope)
        _logger.debug(f"Request scope created: {scope.request_id}")

        try:
            yield scope
        finally:
            await scope.cleanup()
            _current_scope.reset(token)
            _logger.debug(f"Request scope completed: {scope.request_id} ({scope.elapsed_ms:.1f}ms)")


_current_scope: ContextVar[RequestScope | None] = ContextVar("bizcore_request_scope", default=None)


def get_current_scope() -> RequestScope | None:
    """現在のリクエストスコープを取得."""
    return _current_scope.get()


def get_current_scope_or_raise() -> RequestScope:
    """現在のリクエストスコープを取得（存在しない場合例外）."""
    scope = get_current_scope()
    if scope is None:
        msg = "No active request scope"
        raise RuntimeError(msg)
    return scope


def require_scope() -> RequestScope:
    """現在のスコープを取得（エイリアス）."""
    return get_current_scope_or_raise()


__all__ = [
    "RequestScope",
    "get_current_scope",
    "get_current_scope_or_raise",
    "require_scope",
]
