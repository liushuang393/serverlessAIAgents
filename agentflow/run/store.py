"""Run/Replay/Compare の基盤モデル."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Protocol, runtime_checkable


MetricValue = float | int


@dataclass
class RunRecord:
    """実行記録."""

    run_id: str
    flow_id: str
    thread_id: str
    trace_id: str | None
    tenant_id: str | None
    status: str
    started_at: float
    completed_at: float | None
    metrics: dict[str, MetricValue] = field(default_factory=dict)


@runtime_checkable
class RunStore(Protocol):
    """実行記録ストアのインターフェース."""

    async def save(self, record: RunRecord) -> None:
        """実行記録を保存."""
        ...

    async def get(self, run_id: str) -> RunRecord | None:
        """実行記録を取得."""
        ...

    async def list(
        self,
        *,
        limit: int | None = None,
        offset: int = 0,
    ) -> list[RunRecord]:
        """実行記録を一覧取得."""
        ...


@dataclass
class MemoryRunStore(RunStore):
    """インメモリのRunStore実装."""

    _records: dict[str, RunRecord] = field(default_factory=dict)

    async def save(self, record: RunRecord) -> None:
        """実行記録を保存."""
        self._records[record.run_id] = record

    async def get(self, run_id: str) -> RunRecord | None:
        """実行記録を取得."""
        return self._records.get(run_id)

    async def list(
        self,
        *,
        limit: int | None = None,
        offset: int = 0,
    ) -> list[RunRecord]:
        """実行記録を一覧取得."""
        records = list(self._records.values())
        if offset:
            records = records[offset:]
        if limit is not None:
            records = records[:limit]
        return records


@dataclass(frozen=True)
class RunDiff:
    """RunRecord の差分."""

    metadata: dict[str, tuple[object, object]]
    metrics: dict[str, tuple[MetricValue | None, MetricValue | None]]

    @classmethod
    def compare(cls, left: RunRecord, right: RunRecord) -> RunDiff:
        """2つのRunRecordを比較して差分を返す."""
        metadata: dict[str, tuple[object, object]] = {}
        if left.flow_id != right.flow_id:
            metadata["flow_id"] = (left.flow_id, right.flow_id)
        if left.thread_id != right.thread_id:
            metadata["thread_id"] = (left.thread_id, right.thread_id)
        if left.trace_id != right.trace_id:
            metadata["trace_id"] = (left.trace_id, right.trace_id)
        if left.tenant_id != right.tenant_id:
            metadata["tenant_id"] = (left.tenant_id, right.tenant_id)
        if left.status != right.status:
            metadata["status"] = (left.status, right.status)
        if left.started_at != right.started_at:
            metadata["started_at"] = (left.started_at, right.started_at)
        if left.completed_at != right.completed_at:
            metadata["completed_at"] = (left.completed_at, right.completed_at)

        metrics: dict[str, tuple[MetricValue | None, MetricValue | None]] = {}
        metrics_keys: set[str] = set(left.metrics) | set(right.metrics)
        for key in sorted(metrics_keys):
            left_value: MetricValue | None = left.metrics.get(key)
            right_value: MetricValue | None = right.metrics.get(key)
            if left_value != right_value:
                metrics[key] = (left_value, right_value)

        return cls(metadata=metadata, metrics=metrics)

    def has_changes(self) -> bool:
        """差分が存在するかを判定."""
        return bool(self.metadata or self.metrics)


__all__ = [
    "MemoryRunStore",
    "MetricValue",
    "RunDiff",
    "RunRecord",
    "RunStore",
]
