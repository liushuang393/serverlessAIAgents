"""Messaging Hub 実行基盤の保存・参照ユーティリティ."""

from __future__ import annotations

import json
import sqlite3
import uuid
from dataclasses import dataclass
from datetime import UTC, datetime
from enum import StrEnum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from contracts.policy import EvalResult, PolicyDecision
from kernel.state.models import Decision, DecisionType


if TYPE_CHECKING:
    from pathlib import Path


def _utc_now_iso() -> str:
    """UTC 現在時刻を返す."""
    return datetime.now(UTC).isoformat()


def _to_json(value: Any) -> str:
    """JSON 文字列へ変換する."""
    return json.dumps(value, ensure_ascii=False, default=str)


def _from_json(value: str | None, fallback: Any) -> Any:
    """JSON 文字列を復元する."""
    if value is None or value == "":
        return fallback
    try:
        return json.loads(value)
    except json.JSONDecodeError:
        return fallback


class ExecutionProfile(StrEnum):
    """実行プロファイル."""

    LIGHTWEIGHT_DEFAULT = "lightweight_default"
    MONITORED_READ_THEN_WRITE = "monitored_read_then_write"
    CAPABILITY_ROUTE = "capability_route"
    GAP_FILL_GOVERNED = "gap_fill_governed"


class ExecutionFeedbackSource(StrEnum):
    """feedback ソース."""

    VERIFIER = "verifier"
    SCORER = "scorer"
    HUMAN = "human"
    APPROVAL = "approval"


class ActionLogStatus(StrEnum):
    """action log 状態."""

    STARTED = "started"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


class ContextHierarchy(BaseModel):
    """階層化コンテキスト."""

    global_policy: dict[str, Any] = Field(default_factory=dict)
    session_summary: dict[str, Any] = Field(default_factory=dict)
    request_context: dict[str, Any] = Field(default_factory=dict)
    open_actions: list[str] = Field(default_factory=list)
    retrieval_evidence_refs: list[dict[str, Any]] = Field(default_factory=list)


class CheckpointPolicy(BaseModel):
    """checkpoint 方針."""

    mode: str = Field(default="lightweight")
    triggers: list[str] = Field(default_factory=list)
    requires_pre_mutation: bool = Field(default=False)
    notes: list[str] = Field(default_factory=list)


class GatePolicy(BaseModel):
    """gate 方針."""

    mode: str = Field(default="lightweight")
    verification_profile: str = Field(default="basic")
    approval_actions: list[str] = Field(default_factory=list)
    allow_runtime_artifact: bool = Field(default=False)
    artifact_governance_required: bool = Field(default=False)
    notes: list[str] = Field(default_factory=list)


class ExecutionSummarySnapshot(BaseModel):
    """実行サマリー."""

    session_id: str = Field(default="")
    task_id: str = Field(default="")
    profile: ExecutionProfile = Field(default=ExecutionProfile.LIGHTWEIGHT_DEFAULT)
    status: str = Field(default="pending")
    decision_count: int = Field(default=0, ge=0)
    checkpoint_count: int = Field(default=0, ge=0)
    feedback_count: int = Field(default=0, ge=0)
    action_count: int = Field(default=0, ge=0)
    latest_checkpoint_stage: str | None = Field(default=None)
    latest_feedback: str | None = Field(default=None)
    artifact_refs: list[str] = Field(default_factory=list)


class ExecutionSessionRecord(BaseModel):
    """task 単位の execution session."""

    session_id: str = Field(default_factory=lambda: f"exec_{uuid.uuid4().hex}")
    task_id: str = Field(..., min_length=1)
    user_id: str = Field(default="system")
    conversation_id: str | None = Field(default=None)
    status: str = Field(default="pending")
    execution_profile: ExecutionProfile = Field(default=ExecutionProfile.LIGHTWEIGHT_DEFAULT)
    context_snapshot: dict[str, Any] = Field(default_factory=dict)
    summary: dict[str, Any] = Field(default_factory=dict)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: str = Field(default_factory=_utc_now_iso)
    updated_at: str = Field(default_factory=_utc_now_iso)


class ExecutionDecisionRecord(BaseModel):
    """判断記録."""

    id: str = Field(default_factory=lambda: f"exec_dec_{uuid.uuid4().hex}")
    session_id: str = Field(..., min_length=1)
    task_id: str = Field(..., min_length=1)
    decision: Decision
    policy_decision: PolicyDecision | None = Field(default=None)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: str = Field(default_factory=_utc_now_iso)


class ExecutionCheckpointRecord(BaseModel):
    """checkpoint 記録."""

    id: str = Field(default_factory=lambda: f"exec_chk_{uuid.uuid4().hex}")
    session_id: str = Field(..., min_length=1)
    task_id: str = Field(..., min_length=1)
    stage: str = Field(..., min_length=1)
    snapshot: dict[str, Any] = Field(default_factory=dict)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: str = Field(default_factory=_utc_now_iso)


class ExecutionFeedbackRecord(BaseModel):
    """feedback 記録."""

    id: str = Field(default_factory=lambda: f"exec_fb_{uuid.uuid4().hex}")
    session_id: str = Field(..., min_length=1)
    task_id: str = Field(..., min_length=1)
    source: ExecutionFeedbackSource
    title: str = Field(..., min_length=1)
    feedback: str = Field(default="")
    passed: bool | None = Field(default=None)
    score: float | None = Field(default=None)
    eval_result: EvalResult | None = Field(default=None)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: str = Field(default_factory=_utc_now_iso)


class ActionLogEntry(BaseModel):
    """粗粒度 action log."""

    id: str = Field(default_factory=lambda: f"exec_act_{uuid.uuid4().hex}")
    session_id: str = Field(..., min_length=1)
    task_id: str = Field(..., min_length=1)
    stage: str = Field(..., min_length=1)
    action_type: str = Field(..., min_length=1)
    summary: str = Field(..., min_length=1)
    status: ActionLogStatus = Field(default=ActionLogStatus.COMPLETED)
    details: dict[str, Any] = Field(default_factory=dict)
    artifact_refs: list[str] = Field(default_factory=list)
    created_at: str = Field(default_factory=_utc_now_iso)


class ExecutionInspection(BaseModel):
    """execution 参照 API のレスポンス."""

    task_id: str
    execution_session: dict[str, Any]
    latest_checkpoint: dict[str, Any] | None = None
    decisions: list[dict[str, Any]] = Field(default_factory=list)
    feedback_summary: dict[str, Any] = Field(default_factory=dict)
    action_log_summary: dict[str, Any] = Field(default_factory=dict)
    execution_summary: dict[str, Any] = Field(default_factory=dict)


class ReplayView(BaseModel):
    """read-only replay ビュー."""

    task_id: str
    execution_session: dict[str, Any]
    timeline: list[dict[str, Any]] = Field(default_factory=list)
    execution_summary: dict[str, Any] = Field(default_factory=dict)


class ExecutionMonitoringSummary(BaseModel):
    """execution monitoring 用の集計."""

    total_sessions: int = Field(default=0, ge=0)
    by_profile: dict[str, int] = Field(default_factory=dict)
    by_status: dict[str, int] = Field(default_factory=dict)
    total_actions: int = Field(default=0, ge=0)
    unsafe_action_rate: float = Field(default=0.0, ge=0.0)
    verification_failure_rate: float = Field(default=0.0, ge=0.0)
    avg_quality_score: float | None = Field(default=None)
    runtime_artifact_actions: int = Field(default=0, ge=0)
    approval_pending: int = Field(default=0, ge=0)
    avg_approval_latency_seconds: float | None = Field(default=None)


@dataclass(slots=True)
class SQLiteExecutionSubstrateStore:
    """execution substrate の SQLite ストア."""

    db_path: Path
    _initialized: bool = False

    async def initialize(self) -> None:
        """テーブルを初期化する."""
        if self._initialized:
            return
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._initialize_sync()
        self._initialized = True

    def _initialize_sync(self) -> None:
        """同期でテーブルを作成する."""
        with sqlite3.connect(self.db_path, timeout=30, check_same_thread=False) as conn:
            conn.execute("PRAGMA journal_mode=WAL;")
            conn.execute("PRAGMA foreign_keys=ON;")
            conn.executescript(
                """
                CREATE TABLE IF NOT EXISTS execution_sessions (
                    session_id TEXT PRIMARY KEY,
                    task_id TEXT NOT NULL UNIQUE,
                    user_id TEXT NOT NULL,
                    conversation_id TEXT,
                    status TEXT NOT NULL,
                    execution_profile TEXT NOT NULL,
                    context_snapshot_json TEXT NOT NULL DEFAULT '{}',
                    summary_json TEXT NOT NULL DEFAULT '{}',
                    metadata_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );
                CREATE INDEX IF NOT EXISTS idx_execution_sessions_task_id
                    ON execution_sessions(task_id);

                CREATE TABLE IF NOT EXISTS execution_decisions (
                    id TEXT PRIMARY KEY,
                    session_id TEXT NOT NULL,
                    task_id TEXT NOT NULL,
                    decision_json TEXT NOT NULL,
                    policy_decision_json TEXT,
                    metadata_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL
                );
                CREATE INDEX IF NOT EXISTS idx_execution_decisions_task_id
                    ON execution_decisions(task_id, created_at);

                CREATE TABLE IF NOT EXISTS execution_checkpoints (
                    id TEXT PRIMARY KEY,
                    session_id TEXT NOT NULL,
                    task_id TEXT NOT NULL,
                    stage TEXT NOT NULL,
                    snapshot_json TEXT NOT NULL DEFAULT '{}',
                    metadata_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL
                );
                CREATE INDEX IF NOT EXISTS idx_execution_checkpoints_task_id
                    ON execution_checkpoints(task_id, created_at);

                CREATE TABLE IF NOT EXISTS execution_feedback (
                    id TEXT PRIMARY KEY,
                    session_id TEXT NOT NULL,
                    task_id TEXT NOT NULL,
                    source TEXT NOT NULL,
                    title TEXT NOT NULL,
                    feedback TEXT NOT NULL DEFAULT '',
                    passed INTEGER,
                    score REAL,
                    eval_result_json TEXT,
                    metadata_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL
                );
                CREATE INDEX IF NOT EXISTS idx_execution_feedback_task_id
                    ON execution_feedback(task_id, created_at);

                CREATE TABLE IF NOT EXISTS execution_action_logs (
                    id TEXT PRIMARY KEY,
                    session_id TEXT NOT NULL,
                    task_id TEXT NOT NULL,
                    stage TEXT NOT NULL,
                    action_type TEXT NOT NULL,
                    summary TEXT NOT NULL,
                    status TEXT NOT NULL,
                    details_json TEXT NOT NULL DEFAULT '{}',
                    artifact_refs_json TEXT NOT NULL DEFAULT '[]',
                    created_at TEXT NOT NULL
                );
                CREATE INDEX IF NOT EXISTS idx_execution_action_logs_task_id
                    ON execution_action_logs(task_id, created_at);
                """
            )
            conn.commit()

    async def _execute(
        self,
        query: str,
        params: tuple[Any, ...] = (),
        *,
        fetch: bool = False,
    ) -> list[dict[str, Any]]:
        """SQL を実行する."""
        await self.initialize()
        return self._execute_sync(query, params, fetch)

    def _execute_sync(
        self,
        query: str,
        params: tuple[Any, ...],
        fetch: bool,
    ) -> list[dict[str, Any]]:
        """同期 SQL 実行."""
        with sqlite3.connect(self.db_path, timeout=30, check_same_thread=False) as conn:
            conn.row_factory = sqlite3.Row
            cursor = conn.execute(query, params)
            rows = [dict(row) for row in cursor.fetchall()] if fetch else []
            conn.commit()
            return rows

    async def upsert_execution_session(self, record: dict[str, Any]) -> None:
        """execution session を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_sessions (
                session_id, task_id, user_id, conversation_id, status, execution_profile,
                context_snapshot_json, summary_json, metadata_json, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("session_id", "")),
                str(record.get("task_id", "")),
                str(record.get("user_id", "system")),
                record.get("conversation_id"),
                str(record.get("status", "pending")),
                str(record.get("execution_profile", ExecutionProfile.LIGHTWEIGHT_DEFAULT.value)),
                _to_json(record.get("context_snapshot", {})),
                _to_json(record.get("summary", {})),
                _to_json(record.get("metadata", {})),
                str(record.get("created_at", _utc_now_iso())),
                str(record.get("updated_at", _utc_now_iso())),
            ),
        )

    async def get_execution_session_by_task(self, task_id: str) -> dict[str, Any] | None:
        """task_id から execution session を取得する."""
        rows = await self._execute(
            "SELECT * FROM execution_sessions WHERE task_id = ?",
            (task_id,),
            fetch=True,
        )
        if not rows:
            return None
        return self._normalize_session_row(rows[0])

    async def list_execution_sessions(self, limit: int = 5000) -> list[dict[str, Any]]:
        """execution session 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_sessions
            ORDER BY created_at DESC
            LIMIT ?
            """,
            (limit,),
            fetch=True,
        )
        return [self._normalize_session_row(row) for row in rows]

    async def add_execution_decision(self, record: dict[str, Any]) -> None:
        """decision を追加する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_decisions (
                id, session_id, task_id, decision_json, policy_decision_json, metadata_json, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("id", "")),
                str(record.get("session_id", "")),
                str(record.get("task_id", "")),
                _to_json(record.get("decision", {})),
                _to_json(record.get("policy_decision")) if record.get("policy_decision") is not None else None,
                _to_json(record.get("metadata", {})),
                str(record.get("created_at", _utc_now_iso())),
            ),
        )

    async def list_execution_decisions(self, task_id: str) -> list[dict[str, Any]]:
        """task の decision 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_decisions
            WHERE task_id = ?
            ORDER BY created_at ASC, id ASC
            """,
            (task_id,),
            fetch=True,
        )
        for row in rows:
            row["decision"] = _from_json(row.pop("decision_json", None), {})
            row["policy_decision"] = _from_json(row.pop("policy_decision_json", None), None)
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def add_execution_checkpoint(self, record: dict[str, Any]) -> None:
        """checkpoint を追加する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_checkpoints (
                id, session_id, task_id, stage, snapshot_json, metadata_json, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("id", "")),
                str(record.get("session_id", "")),
                str(record.get("task_id", "")),
                str(record.get("stage", "")),
                _to_json(record.get("snapshot", {})),
                _to_json(record.get("metadata", {})),
                str(record.get("created_at", _utc_now_iso())),
            ),
        )

    async def list_execution_checkpoints(self, task_id: str) -> list[dict[str, Any]]:
        """task の checkpoint 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_checkpoints
            WHERE task_id = ?
            ORDER BY created_at ASC, id ASC
            """,
            (task_id,),
            fetch=True,
        )
        for row in rows:
            row["snapshot"] = _from_json(row.pop("snapshot_json", None), {})
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def add_execution_feedback(self, record: dict[str, Any]) -> None:
        """feedback を追加する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_feedback (
                id, session_id, task_id, source, title, feedback, passed, score,
                eval_result_json, metadata_json, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("id", "")),
                str(record.get("session_id", "")),
                str(record.get("task_id", "")),
                str(record.get("source", ExecutionFeedbackSource.VERIFIER.value)),
                str(record.get("title", "")),
                str(record.get("feedback", "")),
                self._bool_to_int(record.get("passed")),
                float(record["score"]) if record.get("score") is not None else None,
                _to_json(record.get("eval_result")) if record.get("eval_result") is not None else None,
                _to_json(record.get("metadata", {})),
                str(record.get("created_at", _utc_now_iso())),
            ),
        )

    async def list_execution_feedback(self, task_id: str) -> list[dict[str, Any]]:
        """task の feedback 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_feedback
            WHERE task_id = ?
            ORDER BY created_at ASC, id ASC
            """,
            (task_id,),
            fetch=True,
        )
        for row in rows:
            row["passed"] = self._int_to_bool(row.get("passed"))
            row["eval_result"] = _from_json(row.pop("eval_result_json", None), None)
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def list_all_execution_feedback(self, limit: int = 5000) -> list[dict[str, Any]]:
        """全 task の feedback 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_feedback
            ORDER BY created_at DESC, id DESC
            LIMIT ?
            """,
            (limit,),
            fetch=True,
        )
        for row in rows:
            row["passed"] = self._int_to_bool(row.get("passed"))
            row["eval_result"] = _from_json(row.pop("eval_result_json", None), None)
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def add_action_log(self, record: dict[str, Any]) -> None:
        """action log を追加する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_action_logs (
                id, session_id, task_id, stage, action_type, summary, status,
                details_json, artifact_refs_json, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("id", "")),
                str(record.get("session_id", "")),
                str(record.get("task_id", "")),
                str(record.get("stage", "")),
                str(record.get("action_type", "")),
                str(record.get("summary", "")),
                str(record.get("status", ActionLogStatus.COMPLETED.value)),
                _to_json(record.get("details", {})),
                _to_json(record.get("artifact_refs", [])),
                str(record.get("created_at", _utc_now_iso())),
            ),
        )

    async def list_action_logs(self, task_id: str) -> list[dict[str, Any]]:
        """task の action log 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_action_logs
            WHERE task_id = ?
            ORDER BY created_at ASC, id ASC
            """,
            (task_id,),
            fetch=True,
        )
        for row in rows:
            row["details"] = _from_json(row.pop("details_json", None), {})
            row["artifact_refs"] = _from_json(row.pop("artifact_refs_json", None), [])
        return rows

    async def list_all_action_logs(self, limit: int = 5000) -> list[dict[str, Any]]:
        """全 task の action log 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM execution_action_logs
            ORDER BY created_at DESC, id DESC
            LIMIT ?
            """,
            (limit,),
            fetch=True,
        )
        for row in rows:
            row["details"] = _from_json(row.pop("details_json", None), {})
            row["artifact_refs"] = _from_json(row.pop("artifact_refs_json", None), [])
        return rows

    @staticmethod
    def _normalize_session_row(row: dict[str, Any]) -> dict[str, Any]:
        """session row を正規化する."""
        row["context_snapshot"] = _from_json(row.pop("context_snapshot_json", None), {})
        row["summary"] = _from_json(row.pop("summary_json", None), {})
        row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return row

    @staticmethod
    def _bool_to_int(value: bool | None) -> int | None:
        """bool を SQLite 向け整数へ変換する."""
        if value is None:
            return None
        return 1 if value else 0

    @staticmethod
    def _int_to_bool(value: Any) -> bool | None:
        """SQLite の整数 bool を復元する."""
        if value is None:
            return None
        return bool(int(value))


class ExecutionSubstrateService:
    """execution substrate の記録と参照をまとめるサービス."""

    def __init__(self, store: SQLiteExecutionSubstrateStore) -> None:
        """初期化."""
        self._store = store

    async def start_session(
        self,
        *,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        execution_profile: ExecutionProfile,
        context_snapshot: dict[str, Any],
        status: str,
        metadata: dict[str, Any] | None = None,
    ) -> ExecutionSessionRecord:
        """新規 execution session を開始する."""
        existing = await self._store.get_execution_session_by_task(task_id)
        if existing is not None:
            session = ExecutionSessionRecord.model_validate(existing)
            session.execution_profile = execution_profile
            session.context_snapshot = context_snapshot
            session.status = status
            session.metadata.update(metadata or {})
            session.updated_at = _utc_now_iso()
            await self._store.upsert_execution_session(session.model_dump(mode="json"))
            return session

        session = ExecutionSessionRecord(
            task_id=task_id,
            user_id=user_id,
            conversation_id=conversation_id,
            status=status,
            execution_profile=execution_profile,
            context_snapshot=context_snapshot,
            metadata=metadata or {},
        )
        await self._store.upsert_execution_session(session.model_dump(mode="json"))
        return session

    async def sync_task_status(
        self,
        *,
        task_id: str,
        status: str,
        context_snapshot: dict[str, Any] | None = None,
    ) -> ExecutionSessionRecord | None:
        """session の状態とコンテキストを同期する."""
        raw = await self._store.get_execution_session_by_task(task_id)
        if raw is None:
            return None
        session = ExecutionSessionRecord.model_validate(raw)
        session.status = status
        if context_snapshot is not None:
            session.context_snapshot = context_snapshot
        session.updated_at = _utc_now_iso()
        await self._refresh_summary(session)
        return session

    async def record_decision(
        self,
        *,
        task_id: str,
        step: str,
        decision_type: DecisionType,
        choice: str,
        reason: str,
        alternatives: list[str] | None = None,
        confidence: float = 1.0,
        metadata: dict[str, Any] | None = None,
        policy_decision: PolicyDecision | None = None,
    ) -> ExecutionDecisionRecord | None:
        """decision を記録する."""
        session = await self._store.get_execution_session_by_task(task_id)
        if session is None:
            return None
        record = ExecutionDecisionRecord(
            session_id=str(session.get("session_id", "")),
            task_id=task_id,
            decision=Decision(
                step=step,
                decision_type=decision_type,
                choice=choice,
                alternatives=alternatives or [],
                reason=reason,
                confidence=confidence,
                metadata=metadata or {},
            ),
            policy_decision=policy_decision,
            metadata=metadata or {},
        )
        await self._store.add_execution_decision(record.model_dump(mode="json"))
        await self._refresh_summary(ExecutionSessionRecord.model_validate(session))
        return record

    async def record_checkpoint(
        self,
        *,
        task_id: str,
        stage: str,
        snapshot: dict[str, Any],
        metadata: dict[str, Any] | None = None,
    ) -> ExecutionCheckpointRecord | None:
        """checkpoint を記録する."""
        session = await self._store.get_execution_session_by_task(task_id)
        if session is None:
            return None
        record = ExecutionCheckpointRecord(
            session_id=str(session.get("session_id", "")),
            task_id=task_id,
            stage=stage,
            snapshot=snapshot,
            metadata=metadata or {},
        )
        await self._store.add_execution_checkpoint(record.model_dump(mode="json"))
        await self._refresh_summary(ExecutionSessionRecord.model_validate(session))
        return record

    async def record_feedback(
        self,
        *,
        task_id: str,
        source: ExecutionFeedbackSource,
        title: str,
        feedback: str = "",
        passed: bool | None = None,
        score: float | None = None,
        eval_result: EvalResult | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> ExecutionFeedbackRecord | None:
        """feedback を記録する."""
        session = await self._store.get_execution_session_by_task(task_id)
        if session is None:
            return None
        record = ExecutionFeedbackRecord(
            session_id=str(session.get("session_id", "")),
            task_id=task_id,
            source=source,
            title=title,
            feedback=feedback,
            passed=passed,
            score=score,
            eval_result=eval_result,
            metadata=metadata or {},
        )
        await self._store.add_execution_feedback(record.model_dump(mode="json"))
        await self._refresh_summary(ExecutionSessionRecord.model_validate(session))
        return record

    async def record_action(
        self,
        *,
        task_id: str,
        stage: str,
        action_type: str,
        summary: str,
        status: ActionLogStatus = ActionLogStatus.COMPLETED,
        details: dict[str, Any] | None = None,
        artifact_refs: list[str] | None = None,
    ) -> ActionLogEntry | None:
        """粗粒度 action log を記録する."""
        session = await self._store.get_execution_session_by_task(task_id)
        if session is None:
            return None
        record = ActionLogEntry(
            session_id=str(session.get("session_id", "")),
            task_id=task_id,
            stage=stage,
            action_type=action_type,
            summary=summary,
            status=status,
            details=details or {},
            artifact_refs=artifact_refs or [],
        )
        await self._store.add_action_log(record.model_dump(mode="json"))
        await self._refresh_summary(ExecutionSessionRecord.model_validate(session))
        return record

    async def inspect_task(self, task_id: str) -> ExecutionInspection | None:
        """execution inspection を組み立てる."""
        gathered = await self._gather(task_id)
        session = gathered["session"]
        if session is None:
            return None
        checkpoints = gathered["checkpoints"]
        feedback = gathered["feedback"]
        actions = gathered["actions"]
        summary = self._build_summary(
            session=session,
            decisions=gathered["decisions"],
            checkpoints=checkpoints,
            feedback=feedback,
            actions=actions,
        )
        latest_checkpoint = checkpoints[-1] if checkpoints else None
        return ExecutionInspection(
            task_id=task_id,
            execution_session=session,
            latest_checkpoint=latest_checkpoint,
            decisions=gathered["decisions"],
            feedback_summary=self._build_feedback_summary(feedback),
            action_log_summary=self._build_action_log_summary(actions),
            execution_summary=summary.model_dump(mode="json"),
        )

    async def replay_task(
        self,
        *,
        task_id: str,
        agui_events: list[dict[str, Any]],
        execution_events: list[dict[str, Any]] | None = None,
    ) -> ReplayView | None:
        """execution replay を組み立てる."""
        gathered = await self._gather(task_id)
        session = gathered["session"]
        if session is None:
            return None
        timeline: list[dict[str, Any]] = []
        order_counter = 0
        for item in gathered["checkpoints"]:
            timeline.append(self._timeline_entry(item, "checkpoint", order_counter))
            order_counter += 1
        for item in gathered["decisions"]:
            timeline.append(self._timeline_entry(item, "decision", order_counter))
            order_counter += 1
        for item in gathered["feedback"]:
            timeline.append(self._timeline_entry(item, "feedback", order_counter))
            order_counter += 1
        for item in gathered["actions"]:
            timeline.append(self._timeline_entry(item, "action_log", order_counter))
            order_counter += 1
        for item in execution_events or []:
            timeline.append(self._timeline_entry(item, "execution_event", order_counter))
            order_counter += 1
        for item in agui_events:
            timeline.append(self._timeline_entry(item, "agui_event", order_counter))
            order_counter += 1
        timeline.sort(key=lambda item: (str(item.get("created_at", "")), int(item.get("_order", 0))))
        for item in timeline:
            item.pop("_order", None)
        summary = self._build_summary(
            session=session,
            decisions=gathered["decisions"],
            checkpoints=gathered["checkpoints"],
            feedback=gathered["feedback"],
            actions=gathered["actions"],
        )
        return ReplayView(
            task_id=task_id,
            execution_session=session,
            timeline=timeline,
            execution_summary=summary.model_dump(mode="json"),
        )

    async def monitoring_summary(
        self,
        *,
        approvals: list[dict[str, Any]] | None = None,
    ) -> ExecutionMonitoringSummary:
        """monitoring 用の全体集計を返す."""
        sessions = await self._store.list_execution_sessions()
        feedback = await self._store.list_all_execution_feedback()
        actions = await self._store.list_all_action_logs()

        by_profile: dict[str, int] = {}
        by_status: dict[str, int] = {}
        for session in sessions:
            profile = str(session.get("execution_profile", ExecutionProfile.LIGHTWEIGHT_DEFAULT.value))
            status = str(session.get("status", "unknown"))
            by_profile[profile] = by_profile.get(profile, 0) + 1
            by_status[status] = by_status.get(status, 0) + 1

        verifier_feedback = [item for item in feedback if str(item.get("source", "")) == "verifier"]
        verifier_failures = [
            item for item in verifier_feedback if item.get("passed") is False or float(item.get("score", 1.0) or 0.0) < 0.5
        ]
        scorer_scores = [
            float(item.get("score", 0.0))
            for item in feedback
            if str(item.get("source", "")) == "scorer" and item.get("score") is not None
        ]

        failed_actions = [
            item
            for item in actions
            if str(item.get("status", "")) in {ActionLogStatus.FAILED.value, ActionLogStatus.SKIPPED.value}
        ]
        runtime_artifact_actions = [
            item for item in actions if str(item.get("action_type", "")) == "create_runtime_artifact"
        ]

        pending_count = 0
        approval_latencies: list[float] = []
        for approval in approvals or []:
            status = str(approval.get("status", ""))
            if status == "pending":
                pending_count += 1
            created_at = approval.get("created_at")
            decided_at = approval.get("decided_at")
            if isinstance(created_at, str) and isinstance(decided_at, str):
                try:
                    created_dt = datetime.fromisoformat(created_at)
                    decided_dt = datetime.fromisoformat(decided_at)
                except ValueError:
                    continue
                approval_latencies.append(max((decided_dt - created_dt).total_seconds(), 0.0))

        return ExecutionMonitoringSummary(
            total_sessions=len(sessions),
            by_profile=by_profile,
            by_status=by_status,
            total_actions=len(actions),
            unsafe_action_rate=round(len(failed_actions) / max(len(actions), 1), 4),
            verification_failure_rate=round(len(verifier_failures) / max(len(verifier_feedback), 1), 4),
            avg_quality_score=round(sum(scorer_scores) / len(scorer_scores), 4) if scorer_scores else None,
            runtime_artifact_actions=len(runtime_artifact_actions),
            approval_pending=pending_count,
            avg_approval_latency_seconds=round(sum(approval_latencies) / len(approval_latencies), 4)
            if approval_latencies
            else None,
        )

    async def _gather(self, task_id: str) -> dict[str, Any]:
        """session まわりの記録をまとめて取得する."""
        session = await self._store.get_execution_session_by_task(task_id)
        decisions = await self._store.list_execution_decisions(task_id)
        checkpoints = await self._store.list_execution_checkpoints(task_id)
        feedback = await self._store.list_execution_feedback(task_id)
        actions = await self._store.list_action_logs(task_id)
        return {
            "session": session,
            "decisions": decisions,
            "checkpoints": checkpoints,
            "feedback": feedback,
            "actions": actions,
        }

    async def _refresh_summary(self, session: ExecutionSessionRecord) -> None:
        """現在の session summary を更新する."""
        gathered = await self._gather(session.task_id)
        session_payload = session.model_dump(mode="json")
        summary = self._build_summary(
            session=session_payload,
            decisions=gathered["decisions"],
            checkpoints=gathered["checkpoints"],
            feedback=gathered["feedback"],
            actions=gathered["actions"],
        )
        session.summary = summary.model_dump(mode="json")
        session.updated_at = _utc_now_iso()
        await self._store.upsert_execution_session(session.model_dump(mode="json"))

    @staticmethod
    def _build_summary(
        *,
        session: dict[str, Any],
        decisions: list[dict[str, Any]],
        checkpoints: list[dict[str, Any]],
        feedback: list[dict[str, Any]],
        actions: list[dict[str, Any]],
    ) -> ExecutionSummarySnapshot:
        """summary を構築する."""
        latest_checkpoint_stage = str(checkpoints[-1].get("stage", "")) if checkpoints else None
        latest_feedback = str(feedback[-1].get("title", "")) if feedback else None
        artifact_refs: list[str] = []
        for action in actions:
            refs = action.get("artifact_refs", [])
            if isinstance(refs, list):
                artifact_refs.extend(str(ref) for ref in refs if str(ref))
        deduped_artifacts = list(dict.fromkeys(artifact_refs))
        profile_raw = str(session.get("execution_profile", ExecutionProfile.LIGHTWEIGHT_DEFAULT.value))
        return ExecutionSummarySnapshot(
            session_id=str(session.get("session_id", "")),
            task_id=str(session.get("task_id", "")),
            profile=ExecutionProfile(profile_raw),
            status=str(session.get("status", "pending")),
            decision_count=len(decisions),
            checkpoint_count=len(checkpoints),
            feedback_count=len(feedback),
            action_count=len(actions),
            latest_checkpoint_stage=latest_checkpoint_stage or None,
            latest_feedback=latest_feedback or None,
            artifact_refs=deduped_artifacts,
        )

    @staticmethod
    def _build_feedback_summary(feedback: list[dict[str, Any]]) -> dict[str, Any]:
        """feedback summary を返す."""
        by_source: dict[str, int] = {}
        recent: list[dict[str, Any]] = []
        for item in feedback:
            source = str(item.get("source", "unknown"))
            by_source[source] = by_source.get(source, 0) + 1
        for item in feedback[-3:]:
            recent.append(
                {
                    "source": item.get("source"),
                    "title": item.get("title"),
                    "score": item.get("score"),
                    "passed": item.get("passed"),
                    "created_at": item.get("created_at"),
                }
            )
        return {"total": len(feedback), "by_source": by_source, "recent": recent}

    @staticmethod
    def _build_action_log_summary(actions: list[dict[str, Any]]) -> dict[str, Any]:
        """action log summary を返す."""
        by_type: dict[str, int] = {}
        by_status: dict[str, int] = {}
        recent = actions[-5:]
        for item in actions:
            action_type = str(item.get("action_type", "unknown"))
            status = str(item.get("status", "unknown"))
            by_type[action_type] = by_type.get(action_type, 0) + 1
            by_status[status] = by_status.get(status, 0) + 1
        return {
            "total": len(actions),
            "by_type": by_type,
            "by_status": by_status,
            "recent": recent,
        }

    @staticmethod
    def _timeline_entry(item: dict[str, Any], kind: str, order: int) -> dict[str, Any]:
        """timeline item を構築する."""
        created_at = str(
            item.get("created_at", item.get("completed_at", item.get("started_at", ""))),
        )
        if kind == "agui_event":
            payload = item.get("payload", {})
            if not isinstance(payload, dict):
                payload = {}
            return {
                "kind": kind,
                "created_at": created_at,
                "source": str(item.get("event_type", "agui_event")),
                "payload": payload,
                "_order": order,
            }
        if kind == "execution_event":
            payload = dict(item)
            return {
                "kind": kind,
                "created_at": created_at,
                "source": str(item.get("skill_name", "execution_event")),
                "payload": payload,
                "_order": order,
            }
        return {
            "kind": kind,
            "created_at": created_at,
            "source": kind,
            "payload": item,
            "_order": order,
        }


__all__ = [
    "ActionLogEntry",
    "ActionLogStatus",
    "CheckpointPolicy",
    "ContextHierarchy",
    "ExecutionMonitoringSummary",
    "ExecutionFeedbackRecord",
    "ExecutionFeedbackSource",
    "ExecutionInspection",
    "ExecutionProfile",
    "ExecutionSessionRecord",
    "ExecutionSubstrateService",
    "ExecutionSummarySnapshot",
    "GatePolicy",
    "ReplayView",
    "SQLiteExecutionSubstrateStore",
]
