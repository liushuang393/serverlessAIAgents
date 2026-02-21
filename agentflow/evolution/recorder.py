"""Execution recorder implementations for Evolution V2."""

from __future__ import annotations

import asyncio
from collections import defaultdict
from typing import Any

from agentflow.evolution.types import ExecutionEvent


class InMemoryExecutionRecorder:
    """In-memory recorder for run-level execution events."""

    def __init__(self) -> None:
        self._events: dict[str, list[ExecutionEvent]] = defaultdict(list)
        self._lock = asyncio.Lock()

    async def on_step_start(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        await self._append(run_id, step_id, "step_start", payload)

    async def on_step_success(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        await self._append(run_id, step_id, "step_success", payload)

    async def on_step_error(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        await self._append(run_id, step_id, "step_error", payload)

    async def on_llm_result(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        await self._append(run_id, step_id, "llm_result", payload)

    async def on_tool_result(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        await self._append(run_id, step_id, "tool_result", payload)

    async def list_events(self, run_id: str) -> list[ExecutionEvent]:
        async with self._lock:
            return list(self._events.get(run_id, []))

    async def clear(self, run_id: str | None = None) -> None:
        async with self._lock:
            if run_id is None:
                self._events.clear()
                return
            self._events.pop(run_id, None)

    async def append_event(self, event: ExecutionEvent) -> None:
        """Append a pre-built event."""
        async with self._lock:
            self._events[event.run_id].append(event)

    async def append_events(self, events: list[ExecutionEvent]) -> None:
        """Append multiple events."""
        async with self._lock:
            for event in events:
                self._events[event.run_id].append(event)

    async def _append(
        self,
        run_id: str,
        step_id: str,
        event_type: str,
        payload: dict[str, Any] | None,
    ) -> None:
        event = ExecutionEvent(
            run_id=run_id,
            step_id=step_id,
            event_type=event_type,
            payload=payload or {},
        )
        async with self._lock:
            self._events[run_id].append(event)
