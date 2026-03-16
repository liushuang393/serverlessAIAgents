"""Layer 2 の Trace サービス."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from contracts.trace import TraceRecord
from infrastructure.observability import get_trace_exporter


if TYPE_CHECKING:
    from shared.registry import ComponentToggle


class TraceService:
    """TraceRecord を共有保存先へ流すサービス."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        self._exporter = get_trace_exporter(toggle)

    def publish(self, record: TraceRecord) -> TraceRecord:
        """Trace を export して返す."""
        self._exporter.export(record)
        return record

    def create_record(
        self,
        *,
        trace_id: str,
        span_id: str,
        name: str,
        parent_span_id: str | None = None,
        status: str = "ok",
        attributes: dict[str, Any] | None = None,
        events: list[dict[str, Any]] | None = None,
    ) -> TraceRecord:
        """簡易 TraceRecord を生成する."""
        return TraceRecord(
            trace_id=trace_id,
            span_id=span_id,
            parent_span_id=parent_span_id,
            name=name,
            status=status,
            attributes=attributes or {},
            events=events or [],
        )
