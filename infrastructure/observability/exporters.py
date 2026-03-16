"""観測 export の具体実装."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.trace import TraceRecord


class NoOpTraceExporter:
    """無効時の no-op exporter."""

    def export(self, record: TraceRecord) -> None:
        del record


class LoggingTraceExporter:
    """標準ロガーへ Trace を流す exporter."""

    def __init__(self, logger: logging.Logger | None = None) -> None:
        self._logger = logger or logging.getLogger("infrastructure.observability")

    def export(self, record: TraceRecord) -> None:
        self._logger.info("TRACE %s", record.to_payload())


class InMemoryTraceExporter:
    """テスト用メモリ exporter."""

    def __init__(self) -> None:
        self.records: list[TraceRecord] = []

    def export(self, record: TraceRecord) -> None:
        self.records.append(record)
