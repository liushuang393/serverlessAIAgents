"""エンタープライズ監査システム.

監査ログの永続化、クエリ、コンプライアンスレポート機能を提供。

【機能】
- データベース永続化（PostgreSQL/SQLite）
- 監査ログクエリ
- コンプライアンスレポート生成
- データエクスポート

使用例:
    >>> from agentflow.governance import EnterpriseAuditLogger
    >>> audit = EnterpriseAuditLogger("postgresql://...")
    >>> audit.log_event(AuditEvent(...))
    >>> report = await audit.generate_compliance_report("2026-01-01", "2026-01-31")
"""

from __future__ import annotations

import json
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, override

from pydantic import Field

from agentflow.governance.audit import AuditEvent, AuditLogger


logger = logging.getLogger(__name__)


class AuditEventType(Enum):
    """監査イベントタイプ."""

    TOOL_EXECUTION = "tool_execution"
    AGENT_DECISION = "agent_decision"
    FLOW_START = "flow_start"
    FLOW_COMPLETE = "flow_complete"
    APPROVAL_REQUEST = "approval_request"
    APPROVAL_RESPONSE = "approval_response"
    DATA_ACCESS = "data_access"
    SECURITY_EVENT = "security_event"


class AuditSeverity(Enum):
    """監査イベント重要度."""

    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"


class EnterpriseAuditEvent(AuditEvent):
    """エンタープライズ監査イベント（拡張版）."""

    event_type: AuditEventType = AuditEventType.TOOL_EXECUTION
    severity: AuditSeverity = AuditSeverity.INFO
    agent_id: str | None = None
    agent_name: str | None = None
    input_summary: str | None = None
    output_summary: str | None = None
    execution_time_ms: float | None = None
    error_details: str | None = None
    compliance_tags: list[str] = Field(default_factory=list)
    data_classification: str | None = None


class AuditStorage(ABC):
    """監査ストレージインターフェース."""

    @abstractmethod
    async def store(self, event: EnterpriseAuditEvent) -> None:
        """イベントを保存."""
        ...

    @abstractmethod
    async def query(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
        agent_id: str | None = None,
        severity: AuditSeverity | None = None,
        limit: int = 100,
    ) -> list[EnterpriseAuditEvent]:
        """イベントをクエリ."""
        ...

    @abstractmethod
    async def count(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
    ) -> int:
        """イベント数をカウント."""
        ...


class InMemoryAuditStorage(AuditStorage):
    """インメモリ監査ストレージ（開発・テスト用）."""

    def __init__(self) -> None:
        """初期化."""
        self._events: list[EnterpriseAuditEvent] = []

    @override
    async def store(self, event: EnterpriseAuditEvent) -> None:
        """イベントを保存."""
        self._events.append(event)

    @override
    async def query(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
        agent_id: str | None = None,
        severity: AuditSeverity | None = None,
        limit: int = 100,
    ) -> list[EnterpriseAuditEvent]:
        """イベントをクエリ."""
        result = []
        for event in self._events:
            if start_date and event.timestamp < start_date:
                continue
            if end_date and event.timestamp > end_date:
                continue
            if event_type and event.event_type != event_type:
                continue
            if agent_id and event.agent_id != agent_id:
                continue
            if severity and event.severity != severity:
                continue
            result.append(event)
            if len(result) >= limit:
                break
        return result

    @override
    async def count(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
    ) -> int:
        """イベント数をカウント."""
        count = 0
        for event in self._events:
            if start_date and event.timestamp < start_date:
                continue
            if end_date and event.timestamp > end_date:
                continue
            if event_type and event.event_type != event_type:
                continue
            count += 1
        return count


class PostgresAuditStorage(AuditStorage):
    """PostgreSQL監査ストレージ."""

    def __init__(self, connection_string: str) -> None:
        """初期化."""
        self._connection_string = connection_string
        self._pool: Any = None

    async def _get_pool(self) -> Any:
        """コネクションプールを取得."""
        if self._pool is None:
            try:
                import asyncpg

                self._pool = await asyncpg.create_pool(self._connection_string)
            except ImportError:
                logger.warning("asyncpgがインストールされていません")
                raise
        return self._pool

    @override
    async def store(self, event: EnterpriseAuditEvent) -> None:
        """イベントを保存."""
        pool = await self._get_pool()
        async with pool.acquire() as conn:
            await conn.execute(
                """
                INSERT INTO audit_events (
                    event_id, timestamp, event_type, severity, tool_name,
                    decision, reason, agent_id, agent_name, user_id,
                    flow_id, run_id, metadata
                ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
                """,
                event.event_id,
                event.timestamp,
                event.event_type.value,
                event.severity.value,
                event.tool_name,
                event.decision,
                event.reason,
                event.agent_id,
                event.agent_name,
                event.user_id,
                event.flow_id,
                event.run_id,
                json.dumps(event.metadata),
            )

    @override
    async def query(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
        agent_id: str | None = None,
        severity: AuditSeverity | None = None,
        limit: int = 100,
    ) -> list[EnterpriseAuditEvent]:
        """イベントをクエリ."""
        pool = await self._get_pool()
        async with pool.acquire() as conn:
            query = "SELECT * FROM audit_events WHERE 1=1"
            params: list[Any] = []
            param_idx = 1

            if start_date:
                query += f" AND timestamp >= ${param_idx}"
                params.append(start_date)
                param_idx += 1
            if end_date:
                query += f" AND timestamp <= ${param_idx}"
                params.append(end_date)
                param_idx += 1
            if event_type:
                query += f" AND event_type = ${param_idx}"
                params.append(event_type.value)
                param_idx += 1
            if agent_id:
                query += f" AND agent_id = ${param_idx}"
                params.append(agent_id)
                param_idx += 1
            if severity:
                query += f" AND severity = ${param_idx}"
                params.append(severity.value)
                param_idx += 1

            query += f" ORDER BY timestamp DESC LIMIT ${param_idx}"
            params.append(limit)

            rows = await conn.fetch(query, *params)
            return [self._row_to_event(row) for row in rows]

    @override
    async def count(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
    ) -> int:
        """イベント数をカウント."""
        pool = await self._get_pool()
        async with pool.acquire() as conn:
            query = "SELECT COUNT(*) FROM audit_events WHERE 1=1"
            params: list[Any] = []
            param_idx = 1

            if start_date:
                query += f" AND timestamp >= ${param_idx}"
                params.append(start_date)
                param_idx += 1
            if end_date:
                query += f" AND timestamp <= ${param_idx}"
                params.append(end_date)
                param_idx += 1
            if event_type:
                query += f" AND event_type = ${param_idx}"
                params.append(event_type.value)
                param_idx += 1

            count = await conn.fetchval(query, *params)
            return int(count or 0)

    def _row_to_event(self, row: Any) -> EnterpriseAuditEvent:
        """行をイベントに変換."""
        return EnterpriseAuditEvent(
            event_id=row["event_id"],
            timestamp=row["timestamp"],
            event_type=AuditEventType(row["event_type"]),
            severity=AuditSeverity(row["severity"]),
            tool_name=row["tool_name"],
            tool_call_id=None,
            decision=row["decision"],
            reason=row["reason"],
            auth_decision=None,
            auth_mode=None,
            auth_reason=None,
            agent_id=row["agent_id"],
            agent_name=row["agent_name"],
            user_id=row["user_id"],
            flow_id=row["flow_id"],
            run_id=row["run_id"],
            trace_id=None,
            thread_id=None,
            metadata=json.loads(row["metadata"]) if row["metadata"] else {},
        )


class EnterpriseAuditLogger(AuditLogger):
    """エンタープライズ監査ロガー."""

    def __init__(
        self,
        storage: AuditStorage | None = None,
        connection_string: str | None = None,
    ) -> None:
        """初期化."""
        if storage:
            self._storage = storage
        elif connection_string and connection_string.startswith("postgresql"):
            self._storage = PostgresAuditStorage(connection_string)
        else:
            self._storage = InMemoryAuditStorage()

        self._logger = logging.getLogger(__name__)

    @override
    def log_event(self, event: AuditEvent) -> None:
        """監査イベントを記録（同期版）."""
        import asyncio

        # AuditEventをEnterpriseAuditEventに変換
        if isinstance(event, EnterpriseAuditEvent):
            enterprise_event = event
        else:
            enterprise_event = EnterpriseAuditEvent(
                event_id=event.event_id,
                timestamp=event.timestamp,
                tool_name=event.tool_name,
                tool_call_id=event.tool_call_id,
                decision=event.decision,
                reason=event.reason,
                auth_decision=event.auth_decision,
                auth_mode=event.auth_mode,
                auth_reason=event.auth_reason,
                user_id=event.user_id,
                flow_id=event.flow_id,
                run_id=event.run_id,
                trace_id=event.trace_id,
                thread_id=event.thread_id,
                metadata=event.metadata,
            )

        # 非同期で保存
        try:
            loop = asyncio.get_running_loop()
            loop.create_task(self._storage.store(enterprise_event))
        except RuntimeError:
            asyncio.run(self._storage.store(enterprise_event))

    async def log_event_async(self, event: EnterpriseAuditEvent) -> None:
        """監査イベントを記録（非同期版）."""
        await self._storage.store(event)

    async def query_events(
        self,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        event_type: AuditEventType | None = None,
        agent_id: str | None = None,
        severity: AuditSeverity | None = None,
        limit: int = 100,
    ) -> list[EnterpriseAuditEvent]:
        """監査イベントをクエリ."""
        return await self._storage.query(
            start_date=start_date,
            end_date=end_date,
            event_type=event_type,
            agent_id=agent_id,
            severity=severity,
            limit=limit,
        )

    async def generate_compliance_report(
        self,
        start_date: datetime,
        end_date: datetime,
    ) -> ComplianceReport:
        """コンプライアンスレポートを生成."""
        events = await self._storage.query(
            start_date=start_date,
            end_date=end_date,
            limit=10000,
        )

        total_events = len(events)
        by_type: dict[str, int] = {}
        by_severity: dict[str, int] = {}
        by_agent: dict[str, int] = {}
        security_events: list[EnterpriseAuditEvent] = []

        for event in events:
            # タイプ別集計
            type_key = event.event_type.value
            by_type[type_key] = by_type.get(type_key, 0) + 1

            # 重要度別集計
            sev_key = event.severity.value
            by_severity[sev_key] = by_severity.get(sev_key, 0) + 1

            # Agent別集計
            if event.agent_id:
                by_agent[event.agent_id] = by_agent.get(event.agent_id, 0) + 1

            # セキュリティイベント抽出
            if event.event_type == AuditEventType.SECURITY_EVENT:
                security_events.append(event)

        return ComplianceReport(
            start_date=start_date,
            end_date=end_date,
            total_events=total_events,
            events_by_type=by_type,
            events_by_severity=by_severity,
            events_by_agent=by_agent,
            security_events_count=len(security_events),
            critical_events_count=by_severity.get("critical", 0),
            generated_at=datetime.now(UTC),
        )


@dataclass
class ComplianceReport:
    """コンプライアンスレポート."""

    start_date: datetime
    end_date: datetime
    total_events: int
    events_by_type: dict[str, int]
    events_by_severity: dict[str, int]
    events_by_agent: dict[str, int]
    security_events_count: int
    critical_events_count: int
    generated_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "start_date": self.start_date.isoformat(),
            "end_date": self.end_date.isoformat(),
            "total_events": self.total_events,
            "events_by_type": self.events_by_type,
            "events_by_severity": self.events_by_severity,
            "events_by_agent": self.events_by_agent,
            "security_events_count": self.security_events_count,
            "critical_events_count": self.critical_events_count,
            "generated_at": self.generated_at.isoformat(),
        }

    def to_json(self) -> str:
        """JSONに変換."""
        return json.dumps(self.to_dict(), indent=2)


__all__ = [
    "AuditEventType",
    "AuditSeverity",
    "AuditStorage",
    "ComplianceReport",
    "EnterpriseAuditEvent",
    "EnterpriseAuditLogger",
    "InMemoryAuditStorage",
    "PostgresAuditStorage",
]
