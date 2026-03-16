"""観測 export の抽象契約."""

from __future__ import annotations

from typing import TYPE_CHECKING, Protocol


if TYPE_CHECKING:
    from contracts.trace import TraceRecord


class TraceExporter(Protocol):
    """Trace export 契約."""

    def export(self, record: TraceRecord) -> None:
        """Trace を出力する."""
