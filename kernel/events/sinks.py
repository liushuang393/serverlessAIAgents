"""Kernel event sink 契約."""

from __future__ import annotations

from typing import Any, Protocol


class EventSink(Protocol):
    """Kernel が依存するイベント出力先。"""

    async def emit(self, event_type: str, payload: dict[str, Any]) -> None:
        """イベントを発行する."""


class NoOpEventSink:
    """イベント無効時の sink."""

    async def emit(self, event_type: str, payload: dict[str, Any]) -> None:
        del event_type, payload
