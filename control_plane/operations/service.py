"""Control-plane 運用操作ログサービス."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True, slots=True)
class OperationRecord:
    """運用操作の監査ログ."""

    operation_id: str
    action: str
    metadata: dict[str, Any] = field(default_factory=dict)


class OperationsService:
    """操作ログを蓄積する."""

    def __init__(self) -> None:
        self._records: list[OperationRecord] = []

    def append(self, record: OperationRecord) -> None:
        """操作ログを追加する。"""
        self._records.append(record)

    def list_records(self) -> list[OperationRecord]:
        """全操作ログを返す。"""
        return list(self._records)
