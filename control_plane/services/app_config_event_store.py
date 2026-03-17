"""App 契約変更イベントの SSE ストア."""

from __future__ import annotations

import asyncio
import contextlib
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator


class AppConfigEventStore:
    """App ごとの契約変更イベントバス."""

    def __init__(self) -> None:
        self._subscribers: dict[str, list[asyncio.Queue[dict[str, Any]]]] = {}
        self._lock = asyncio.Lock()

    async def fire_config_change(
        self,
        app_name: str,
        *,
        contracts_rag: dict[str, Any] | None = None,
        contracts_llm: dict[str, Any] | None = None,
        config_version: str | None = None,
        updated_at: str | None = None,
    ) -> int:
        """契約変更イベントをブロードキャストする."""
        now = datetime.now(tz=UTC)
        event = {
            "event_type": "app_contracts_changed",
            "app_name": app_name,
            "contracts_rag": contracts_rag if isinstance(contracts_rag, dict) else {},
            "contracts_llm": contracts_llm if isinstance(contracts_llm, dict) else {},
            "config_version": config_version or str(int(now.timestamp() * 1000)),
            "updated_at": updated_at or now.isoformat(),
        }

        async with self._lock:
            queues = self._subscribers.get(app_name, [])
            count = 0
            for queue in list(queues):
                try:
                    queue.put_nowait(event)
                    count += 1
                except asyncio.QueueFull:
                    continue
        return count

    async def subscribe(self, app_name: str) -> AsyncGenerator[dict[str, Any]]:
        """指定 app のイベントを購読する."""
        queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue(maxsize=50)
        async with self._lock:
            self._subscribers.setdefault(app_name, []).append(queue)

        try:
            while True:
                yield await queue.get()
        except asyncio.CancelledError:
            raise
        finally:
            async with self._lock:
                subscribers = self._subscribers.get(app_name, [])
                with contextlib.suppress(ValueError):
                    subscribers.remove(queue)


_app_config_event_store: AppConfigEventStore | None = None


def init_app_config_event_store() -> AppConfigEventStore:
    """シングルトンを初期化する."""
    global _app_config_event_store
    if _app_config_event_store is None:
        _app_config_event_store = AppConfigEventStore()
    return _app_config_event_store


def get_app_config_event_store() -> AppConfigEventStore:
    """シングルトンを取得する."""
    if _app_config_event_store is None:
        msg = "AppConfigEventStore が未初期化です。init_app_config_event_store() を先に呼んでください。"
        raise RuntimeError(msg)
    return _app_config_event_store


__all__ = [
    "AppConfigEventStore",
    "get_app_config_event_store",
    "init_app_config_event_store",
]
