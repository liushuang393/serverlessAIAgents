"""Harness 非依存の純粋 Tool 実行器."""

from __future__ import annotations

import time
from typing import TYPE_CHECKING

from contracts.tool import ToolCallStatus, ToolRequest, ToolResult
from kernel.events.sinks import EventSink, NoOpEventSink


if TYPE_CHECKING:
    from infrastructure.sandbox.tool_provider import ToolProvider


class KernelToolExecutor:
    """ToolProvider を純粋実行だけに絞って包む."""

    def __init__(self, tool_provider: ToolProvider, event_sink: EventSink | None = None) -> None:
        self._tool_provider = tool_provider
        self._event_sink = event_sink or NoOpEventSink()

    async def execute(self, request: ToolRequest) -> ToolResult:
        """ガバナンス抜きでツールを実行する."""
        await self._event_sink.emit(
            "tool.call",
            {"tool_call_id": request.tool_call_id, "name": request.name, "flow_id": request.flow_id},
        )
        started = time.perf_counter()
        try:
            raw_result = await self._tool_provider.call(request.name, **request.arguments)
            result = ToolResult(
                tool_call_id=request.tool_call_id,
                name=request.name,
                content=str(raw_result),
                status=ToolCallStatus.SUCCESS,
                trace_id=request.trace_id,
                metadata=request.metadata,
                execution_time_ms=(time.perf_counter() - started) * 1000,
            )
        except Exception as exc:
            result = ToolResult(
                tool_call_id=request.tool_call_id,
                name=request.name,
                content="",
                status=ToolCallStatus.FAILED,
                trace_id=request.trace_id,
                error=str(exc),
                metadata=request.metadata,
                execution_time_ms=(time.perf_counter() - started) * 1000,
            )
        await self._event_sink.emit("tool.result", result.to_payload())
        return result
