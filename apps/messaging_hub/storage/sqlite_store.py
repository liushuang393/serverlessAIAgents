"""Messaging Hub SQLite 永続化ストア."""

from __future__ import annotations

import json
import sqlite3
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any


def _utc_now_iso() -> str:
    """UTC 現在時刻を ISO 文字列で返す."""
    return datetime.now(UTC).isoformat()


def _to_json(value: Any) -> str:
    """JSON 文字列に変換する."""
    return json.dumps(value, ensure_ascii=False, default=str)


def _from_json(value: str | None, fallback: Any) -> Any:
    """JSON 文字列を復元する."""
    if value is None or value == "":
        return fallback
    try:
        return json.loads(value)
    except json.JSONDecodeError:
        return fallback


@dataclass(slots=True)
class SQLiteMessagingHubStore:
    """Messaging Hub の SQLite ストア."""

    db_path: Path
    _initialized: bool = False

    @classmethod
    def from_default_path(cls) -> SQLiteMessagingHubStore:
        """既定パスのストアを作成."""
        default_path = Path(__file__).resolve().parents[1] / "data" / "messaging_hub.db"
        return cls(db_path=default_path)

    async def initialize(self) -> None:
        """テーブルを初期化."""
        if self._initialized:
            return
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._initialize_sync()
        self._initialized = True

    def _initialize_sync(self) -> None:
        """同期でテーブルを作成."""
        with sqlite3.connect(self.db_path, timeout=30, check_same_thread=False) as conn:
            conn.execute("PRAGMA journal_mode=WAL;")
            conn.execute("PRAGMA foreign_keys=ON;")
            conn.executescript(
                """
                CREATE TABLE IF NOT EXISTS run_records (
                    run_id TEXT PRIMARY KEY,
                    flow_id TEXT NOT NULL,
                    thread_id TEXT NOT NULL,
                    trace_id TEXT,
                    tenant_id TEXT,
                    status TEXT NOT NULL,
                    started_at REAL NOT NULL,
                    completed_at REAL,
                    metrics_json TEXT NOT NULL DEFAULT '{}'
                );

                CREATE TABLE IF NOT EXISTS approvals (
                    id TEXT PRIMARY KEY,
                    skill_name TEXT NOT NULL,
                    risk_level TEXT NOT NULL,
                    params_json TEXT NOT NULL,
                    user_id TEXT NOT NULL,
                    status TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    expires_at TEXT,
                    decided_at TEXT,
                    decided_by TEXT,
                    rejection_reason TEXT,
                    metadata_json TEXT NOT NULL DEFAULT '{}'
                );

                CREATE TABLE IF NOT EXISTS execution_events (
                    id TEXT PRIMARY KEY,
                    run_id TEXT,
                    skill_name TEXT NOT NULL,
                    params_json TEXT NOT NULL,
                    status TEXT NOT NULL,
                    started_at TEXT NOT NULL,
                    completed_at TEXT,
                    result_json TEXT,
                    error TEXT,
                    duration_ms REAL NOT NULL DEFAULT 0,
                    user_id TEXT NOT NULL,
                    approval_id TEXT,
                    metadata_json TEXT NOT NULL DEFAULT '{}'
                );

                CREATE TABLE IF NOT EXISTS evidence (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    run_id TEXT NOT NULL,
                    step_id TEXT,
                    payload_json TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS artifacts (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    run_id TEXT NOT NULL,
                    step_id TEXT,
                    artifact_type TEXT NOT NULL,
                    location TEXT,
                    payload_json TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS sr_messages (
                    message_id TEXT PRIMARY KEY,
                    conversation_id TEXT NOT NULL,
                    role TEXT NOT NULL,
                    content TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL,
                    metadata_json TEXT NOT NULL DEFAULT '{}'
                );

                CREATE TABLE IF NOT EXISTS sr_subscriptions (
                    subscription_id TEXT PRIMARY KEY,
                    client_id TEXT NOT NULL,
                    conversation_id TEXT,
                    event_types_json TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS user_profiles (
                    user_id TEXT PRIMARY KEY,
                    canonical_id TEXT NOT NULL,
                    display_name TEXT NOT NULL,
                    email TEXT,
                    channel_mappings_json TEXT NOT NULL DEFAULT '{}',
                    preferences_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS sessions (
                    session_id TEXT PRIMARY KEY,
                    user_id TEXT NOT NULL,
                    platform TEXT NOT NULL,
                    conversation_id TEXT,
                    context_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL,
                    last_active_at TEXT NOT NULL,
                    FOREIGN KEY (user_id) REFERENCES user_profiles(user_id)
                );

                CREATE TABLE IF NOT EXISTS assistant_jobs (
                    job_id TEXT PRIMARY KEY,
                    job_type TEXT NOT NULL,
                    user_id TEXT NOT NULL,
                    conversation_id TEXT,
                    status TEXT NOT NULL,
                    input_text TEXT NOT NULL DEFAULT '',
                    run_id TEXT,
                    context_json TEXT NOT NULL DEFAULT '{}',
                    result_json TEXT,
                    error TEXT,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS assistant_job_events (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    job_id TEXT NOT NULL,
                    event_type TEXT NOT NULL,
                    payload_json TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS clarification_tickets (
                    ticket_id TEXT PRIMARY KEY,
                    job_id TEXT NOT NULL,
                    status TEXT NOT NULL,
                    original_question TEXT NOT NULL,
                    questions_json TEXT NOT NULL DEFAULT '[]',
                    answers_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS generated_artifacts (
                    artifact_id TEXT PRIMARY KEY,
                    artifact_type TEXT NOT NULL,
                    name TEXT NOT NULL,
                    description TEXT NOT NULL,
                    status TEXT NOT NULL,
                    capabilities_json TEXT NOT NULL DEFAULT '[]',
                    scopes_json TEXT NOT NULL DEFAULT '[]',
                    allowed_tools_json TEXT NOT NULL DEFAULT '[]',
                    allowed_mcp_servers_json TEXT NOT NULL DEFAULT '[]',
                    dependencies_json TEXT NOT NULL DEFAULT '[]',
                    guardrails_json TEXT NOT NULL DEFAULT '[]',
                    tests_json TEXT NOT NULL DEFAULT '[]',
                    workflow_definition_json,
                    runtime_binding_json,
                    approval_request_id TEXT,
                    metadata_json TEXT NOT NULL DEFAULT '{}',
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS flight_watch_subscriptions (
                    subscription_id TEXT PRIMARY KEY,
                    user_id TEXT NOT NULL,
                    conversation_id TEXT,
                    status TEXT NOT NULL,
                    provider TEXT NOT NULL,
                    request_json TEXT NOT NULL,
                    ranking_weights_json TEXT NOT NULL DEFAULT '{}',
                    notification_targets_json TEXT NOT NULL DEFAULT '[]',
                    baseline_price REAL,
                    target_price REAL,
                    last_notified_price REAL,
                    last_checked_at TEXT,
                    next_check_at TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS flight_offer_snapshots (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    subscription_id TEXT NOT NULL,
                    provider TEXT NOT NULL,
                    lowest_price REAL,
                    offers_json TEXT NOT NULL,
                    checked_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS notification_deliveries (
                    delivery_id TEXT PRIMARY KEY,
                    subscription_id TEXT,
                    channel TEXT NOT NULL,
                    destination TEXT,
                    status TEXT NOT NULL,
                    payload_json TEXT NOT NULL,
                    error TEXT,
                    created_at TEXT NOT NULL
                );
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

    async def upsert_run_record(self, record: dict[str, Any]) -> None:
        """run_records を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO run_records (
                run_id, flow_id, thread_id, trace_id, tenant_id, status,
                started_at, completed_at, metrics_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(record.get("run_id", "")),
                str(record.get("flow_id", "messaging_hub")),
                str(record.get("thread_id", "default")),
                record.get("trace_id"),
                record.get("tenant_id"),
                str(record.get("status", "running")),
                float(record.get("started_at", 0.0)),
                record.get("completed_at"),
                _to_json(record.get("metrics", {})),
            ),
        )

    async def list_run_records(self, limit: int = 100, offset: int = 0) -> list[dict[str, Any]]:
        """run_records 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM run_records
            ORDER BY started_at DESC
            LIMIT ? OFFSET ?
            """,
            (limit, offset),
            fetch=True,
        )
        for row in rows:
            row["metrics"] = _from_json(row.pop("metrics_json", None), {})
        return rows

    async def upsert_approval(self, request: dict[str, Any]) -> None:
        """approval を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO approvals (
                id, skill_name, risk_level, params_json, user_id, status,
                created_at, expires_at, decided_at, decided_by, rejection_reason, metadata_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(request.get("id", "")),
                str(request.get("skill_name", "")),
                str(request.get("risk_level", "low")),
                _to_json(request.get("params", {})),
                str(request.get("user_id", "system")),
                str(request.get("status", "pending")),
                str(request.get("created_at", _utc_now_iso())),
                request.get("expires_at"),
                request.get("decided_at"),
                request.get("decided_by"),
                request.get("rejection_reason"),
                _to_json(request.get("metadata", {})),
            ),
        )

    async def list_approvals(
        self,
        *,
        status: str | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """approval 一覧を返す."""
        if status:
            rows = await self._execute(
                """
                SELECT * FROM approvals
                WHERE status = ?
                ORDER BY created_at DESC
                LIMIT ? OFFSET ?
                """,
                (status, limit, offset),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM approvals
                ORDER BY created_at DESC
                LIMIT ? OFFSET ?
                """,
                (limit, offset),
                fetch=True,
            )
        for row in rows:
            row["params"] = _from_json(row.pop("params_json", None), {})
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def upsert_execution_event(self, event: dict[str, Any]) -> None:
        """execution event を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO execution_events (
                id, run_id, skill_name, params_json, status, started_at, completed_at,
                result_json, error, duration_ms, user_id, approval_id, metadata_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(event.get("id", "")),
                event.get("run_id"),
                str(event.get("skill_name", "")),
                _to_json(event.get("params", {})),
                str(event.get("status", "pending")),
                str(event.get("started_at", _utc_now_iso())),
                event.get("completed_at"),
                _to_json(event.get("result")) if event.get("result") is not None else None,
                event.get("error"),
                float(event.get("duration_ms", 0.0)),
                str(event.get("user_id", "system")),
                event.get("approval_id"),
                _to_json(event.get("metadata", {})),
            ),
        )

    async def list_execution_events(
        self,
        *,
        run_id: str | None = None,
        status: str | None = None,
        skill_name: str | None = None,
        started_date: str | None = None,
        limit: int = 200,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """execution_events 一覧を返す."""
        conditions: list[str] = []
        params: list[Any] = []
        if run_id:
            conditions.append("run_id = ?")
            params.append(run_id)
        if status:
            conditions.append("status = ?")
            params.append(status)
        if skill_name:
            conditions.append("skill_name LIKE ?")
            params.append(f"%{skill_name}%")
        if started_date:
            conditions.append("started_at LIKE ?")
            params.append(f"{started_date}%")
        where_clause = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        rows = await self._execute(
            f"""
            SELECT * FROM execution_events
            {where_clause}
            ORDER BY started_at DESC
            LIMIT ? OFFSET ?
            """,
            (*params, limit, offset),
            fetch=True,
        )
        for row in rows:
            row["params"] = _from_json(row.pop("params_json", None), {})
            row["result"] = _from_json(row.pop("result_json", None), None)
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def add_evidence(
        self,
        *,
        run_id: str,
        step_id: str | None,
        payload: dict[str, Any],
    ) -> None:
        """evidence を追加する."""
        await self._execute(
            """
            INSERT INTO evidence (run_id, step_id, payload_json, created_at)
            VALUES (?, ?, ?, ?)
            """,
            (run_id, step_id, _to_json(payload), _utc_now_iso()),
        )

    async def list_evidence(self, *, run_id: str | None = None, limit: int = 200) -> list[dict[str, Any]]:
        """evidence 一覧を返す."""
        if run_id:
            rows = await self._execute(
                """
                SELECT * FROM evidence
                WHERE run_id = ?
                ORDER BY id DESC
                LIMIT ?
                """,
                (run_id, limit),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM evidence
                ORDER BY id DESC
                LIMIT ?
                """,
                (limit,),
                fetch=True,
            )
        for row in rows:
            row["payload"] = _from_json(row.pop("payload_json", None), {})
        return rows

    async def add_artifact(
        self,
        *,
        run_id: str,
        step_id: str | None,
        artifact_type: str,
        location: str | None,
        payload: dict[str, Any],
    ) -> None:
        """artifact を追加する."""
        await self._execute(
            """
            INSERT INTO artifacts (run_id, step_id, artifact_type, location, payload_json, created_at)
            VALUES (?, ?, ?, ?, ?, ?)
            """,
            (run_id, step_id, artifact_type, location, _to_json(payload), _utc_now_iso()),
        )

    async def list_artifacts(self, *, run_id: str | None = None, limit: int = 200) -> list[dict[str, Any]]:
        """artifact 一覧を返す."""
        if run_id:
            rows = await self._execute(
                """
                SELECT * FROM artifacts
                WHERE run_id = ?
                ORDER BY id DESC
                LIMIT ?
                """,
                (run_id, limit),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM artifacts
                ORDER BY id DESC
                LIMIT ?
                """,
                (limit,),
                fetch=True,
            )
        for row in rows:
            row["payload"] = _from_json(row.pop("payload_json", None), {})
        return rows

    async def upsert_sr_message(self, message: dict[str, Any]) -> None:
        """sr_chat メッセージを保存する."""
        message_id = str(message.get("message_id", ""))
        created_at = str(message.get("created_at", _utc_now_iso()))
        updated_at = str(message.get("updated_at", created_at))
        await self._execute(
            """
            INSERT OR REPLACE INTO sr_messages (
                message_id, conversation_id, role, content, created_at, updated_at, metadata_json
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
            """,
            (
                message_id,
                str(message.get("conversation_id", "default")),
                str(message.get("role", "assistant")),
                str(message.get("content", "")),
                created_at,
                updated_at,
                _to_json(message.get("metadata", {})),
            ),
        )

    async def list_sr_conversations(self, limit: int = 100) -> list[dict[str, Any]]:
        """sr_chat 会話一覧を返す。first_message_preview にユーザー最初発言の先頭50文字を含む。"""
        return await self._execute(
            """
            SELECT
                conversation_id,
                COUNT(*) AS message_count,
                MAX(updated_at) AS last_message_at,
                (
                    SELECT SUBSTR(sm2.content, 1, 50)
                    FROM sr_messages sm2
                    WHERE sm2.conversation_id = sr_messages.conversation_id
                      AND sm2.role = 'user'
                    ORDER BY sm2.created_at ASC
                    LIMIT 1
                ) AS first_message_preview
            FROM sr_messages
            GROUP BY conversation_id
            ORDER BY last_message_at DESC
            LIMIT ?
            """,
            (limit,),
            fetch=True,
        )

    async def list_sr_messages(self, conversation_id: str, limit: int = 100) -> list[dict[str, Any]]:
        """sr_chat メッセージ履歴を返す."""
        rows = await self._execute(
            """
            SELECT * FROM sr_messages
            WHERE conversation_id = ?
            ORDER BY created_at DESC
            LIMIT ?
            """,
            (conversation_id, limit),
            fetch=True,
        )
        for row in rows:
            row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return rows

    async def upsert_subscription(self, subscription: dict[str, Any]) -> None:
        """sr_chat 購読情報を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO sr_subscriptions (
                subscription_id, client_id, conversation_id, event_types_json, created_at
            ) VALUES (?, ?, ?, ?, ?)
            """,
            (
                str(subscription.get("subscription_id", "")),
                str(subscription.get("client_id", "")),
                subscription.get("conversation_id"),
                _to_json(subscription.get("event_types", [])),
                str(subscription.get("created_at", _utc_now_iso())),
            ),
        )

    # =========================================================================
    # ユーザープロファイル操作
    # =========================================================================

    async def upsert_user_profile(self, profile: dict[str, Any]) -> None:
        """user_profile を保存する."""
        now = _utc_now_iso()
        await self._execute(
            """
            INSERT OR REPLACE INTO user_profiles (
                user_id, canonical_id, display_name, email,
                channel_mappings_json, preferences_json, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, COALESCE(
                (SELECT created_at FROM user_profiles WHERE user_id = ?), ?
            ), ?)
            """,
            (
                str(profile.get("user_id", "")),
                str(profile.get("canonical_id", profile.get("user_id", ""))),
                str(profile.get("display_name", "")),
                profile.get("email"),
                _to_json(profile.get("channel_mappings", {})),
                _to_json(profile.get("preferences", {})),
                str(profile.get("user_id", "")),
                now,
                now,
            ),
        )

    async def get_user_profile(self, user_id: str) -> dict[str, Any] | None:
        """user_id でユーザープロファイルを取得する."""
        rows = await self._execute(
            "SELECT * FROM user_profiles WHERE user_id = ?",
            (user_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["channel_mappings"] = _from_json(row.pop("channel_mappings_json", None), {})
        row["preferences"] = _from_json(row.pop("preferences_json", None), {})
        return row

    async def list_user_profiles(self, limit: int = 100, offset: int = 0) -> list[dict[str, Any]]:
        """ユーザープロファイル一覧を返す."""
        rows = await self._execute(
            "SELECT * FROM user_profiles ORDER BY updated_at DESC LIMIT ? OFFSET ?",
            (limit, offset),
            fetch=True,
        )
        for row in rows:
            row["channel_mappings"] = _from_json(row.pop("channel_mappings_json", None), {})
            row["preferences"] = _from_json(row.pop("preferences_json", None), {})
        return rows

    # =========================================================================
    # セッション操作
    # =========================================================================

    async def upsert_session(self, session: dict[str, Any]) -> None:
        """session を保存する."""
        now = _utc_now_iso()
        await self._execute(
            """
            INSERT OR REPLACE INTO sessions (
                session_id, user_id, platform, conversation_id,
                context_json, created_at, last_active_at
            ) VALUES (?, ?, ?, ?, ?, COALESCE(
                (SELECT created_at FROM sessions WHERE session_id = ?), ?
            ), ?)
            """,
            (
                str(session.get("session_id", "")),
                str(session.get("user_id", "")),
                str(session.get("platform", "")),
                session.get("conversation_id"),
                _to_json(session.get("context", {})),
                str(session.get("session_id", "")),
                now,
                str(session.get("last_active_at", now)),
            ),
        )

    async def get_session(self, session_id: str) -> dict[str, Any] | None:
        """session_id でセッションを取得する."""
        rows = await self._execute(
            "SELECT * FROM sessions WHERE session_id = ?",
            (session_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["context"] = _from_json(row.pop("context_json", None), {})
        return row

    async def get_active_session(self, user_id: str, platform: str) -> dict[str, Any] | None:
        """user_id と platform で最新セッションを取得する."""
        rows = await self._execute(
            """
            SELECT * FROM sessions
            WHERE user_id = ? AND platform = ?
            ORDER BY last_active_at DESC
            LIMIT 1
            """,
            (user_id, platform),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["context"] = _from_json(row.pop("context_json", None), {})
        return row

    async def update_session_context(self, session_id: str, context: dict[str, Any]) -> None:
        """セッションのコンテキストと最終アクティブ時刻を更新する."""
        await self._execute(
            "UPDATE sessions SET context_json = ?, last_active_at = ? WHERE session_id = ?",
            (_to_json(context), _utc_now_iso(), session_id),
        )

    async def delete_session(self, session_id: str) -> None:
        """セッションを削除する."""
        await self._execute(
            "DELETE FROM sessions WHERE session_id = ?",
            (session_id,),
        )

    # =========================================================================
    # Orchestration / Job 操作
    # =========================================================================

    async def upsert_assistant_job(self, job: dict[str, Any]) -> None:
        """assistant job を保存する."""
        created_at = str(job.get("created_at", _utc_now_iso()))
        updated_at = str(job.get("updated_at", created_at))
        await self._execute(
            """
            INSERT OR REPLACE INTO assistant_jobs (
                job_id, job_type, user_id, conversation_id, status, input_text,
                run_id, context_json, result_json, error, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(job.get("job_id", "")),
                str(job.get("job_type", "orchestration")),
                str(job.get("user_id", "system")),
                job.get("conversation_id"),
                str(job.get("status", "pending")),
                str(job.get("input_text", "")),
                job.get("run_id"),
                _to_json(job.get("context", {})),
                _to_json(job.get("result")) if job.get("result") is not None else None,
                job.get("error"),
                created_at,
                updated_at,
            ),
        )

    async def get_assistant_job(self, job_id: str) -> dict[str, Any] | None:
        """job_id で assistant job を取得する."""
        rows = await self._execute(
            "SELECT * FROM assistant_jobs WHERE job_id = ?",
            (job_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["context"] = _from_json(row.pop("context_json", None), {})
        row["result"] = _from_json(row.pop("result_json", None), None)
        return row

    async def list_assistant_jobs(
        self,
        *,
        status: str | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """assistant job 一覧を返す."""
        if status:
            rows = await self._execute(
                """
                SELECT * FROM assistant_jobs
                WHERE status = ?
                ORDER BY updated_at DESC
                LIMIT ? OFFSET ?
                """,
                (status, limit, offset),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM assistant_jobs
                ORDER BY updated_at DESC
                LIMIT ? OFFSET ?
                """,
                (limit, offset),
                fetch=True,
            )
        for row in rows:
            row["context"] = _from_json(row.pop("context_json", None), {})
            row["result"] = _from_json(row.pop("result_json", None), None)
        return rows

    async def add_assistant_job_event(
        self,
        *,
        job_id: str,
        event_type: str,
        payload: dict[str, Any],
        created_at: str | None = None,
    ) -> None:
        """assistant job event を追加する."""
        await self._execute(
            """
            INSERT INTO assistant_job_events (job_id, event_type, payload_json, created_at)
            VALUES (?, ?, ?, ?)
            """,
            (job_id, event_type, _to_json(payload), str(created_at or _utc_now_iso())),
        )

    async def list_assistant_job_events(self, job_id: str, limit: int = 200) -> list[dict[str, Any]]:
        """assistant job events を返す."""
        rows = await self._execute(
            """
            SELECT * FROM assistant_job_events
            WHERE job_id = ?
            ORDER BY id ASC
            LIMIT ?
            """,
            (job_id, limit),
            fetch=True,
        )
        for row in rows:
            row["payload"] = _from_json(row.pop("payload_json", None), {})
        return rows

    async def upsert_clarification_ticket(self, ticket: dict[str, Any]) -> None:
        """clarification ticket を保存する."""
        created_at = str(ticket.get("created_at", _utc_now_iso()))
        updated_at = str(ticket.get("updated_at", created_at))
        await self._execute(
            """
            INSERT OR REPLACE INTO clarification_tickets (
                ticket_id, job_id, status, original_question, questions_json,
                answers_json, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(ticket.get("ticket_id", "")),
                str(ticket.get("job_id", "")),
                str(ticket.get("status", "pending")),
                str(ticket.get("original_question", "")),
                _to_json(ticket.get("questions", [])),
                _to_json(ticket.get("answers", {})),
                created_at,
                updated_at,
            ),
        )

    async def get_clarification_ticket(self, ticket_id: str) -> dict[str, Any] | None:
        """clarification ticket を取得する."""
        rows = await self._execute(
            "SELECT * FROM clarification_tickets WHERE ticket_id = ?",
            (ticket_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["questions"] = _from_json(row.pop("questions_json", None), [])
        row["answers"] = _from_json(row.pop("answers_json", None), {})
        return row

    async def get_clarification_ticket_by_job(self, job_id: str) -> dict[str, Any] | None:
        """job_id から clarification ticket を取得する."""
        rows = await self._execute(
            """
            SELECT * FROM clarification_tickets
            WHERE job_id = ?
            ORDER BY updated_at DESC
            LIMIT 1
            """,
            (job_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["questions"] = _from_json(row.pop("questions_json", None), [])
        row["answers"] = _from_json(row.pop("answers_json", None), {})
        return row

    # =========================================================================
    # Generated Artifact 操作
    # =========================================================================

    async def upsert_generated_artifact(self, artifact: dict[str, Any]) -> None:
        """generated artifact を保存する."""
        created_at = str(artifact.get("created_at", _utc_now_iso()))
        updated_at = str(artifact.get("updated_at", created_at))
        await self._execute(
            """
            INSERT OR REPLACE INTO generated_artifacts (
                artifact_id, artifact_type, name, description, status,
                capabilities_json, scopes_json, allowed_tools_json, allowed_mcp_servers_json,
                dependencies_json, guardrails_json, tests_json, workflow_definition_json,
                runtime_binding_json, approval_request_id, metadata_json, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(artifact.get("artifact_id", "")),
                str(artifact.get("artifact_type", "skill")),
                str(artifact.get("name", "")),
                str(artifact.get("description", "")),
                str(artifact.get("status", "draft")),
                _to_json(artifact.get("capabilities", [])),
                _to_json(artifact.get("scopes", [])),
                _to_json(artifact.get("allowed_tools", [])),
                _to_json(artifact.get("allowed_mcp_servers", [])),
                _to_json(artifact.get("dependencies", [])),
                _to_json(artifact.get("guardrails", [])),
                _to_json(artifact.get("tests", [])),
                _to_json(artifact.get("workflow_definition"))
                if artifact.get("workflow_definition") is not None
                else None,
                _to_json(artifact.get("runtime_binding")) if artifact.get("runtime_binding") is not None else None,
                artifact.get("approval_request_id"),
                _to_json(artifact.get("metadata", {})),
                created_at,
                updated_at,
            ),
        )

    async def get_generated_artifact(self, artifact_id: str) -> dict[str, Any] | None:
        """generated artifact を取得する."""
        rows = await self._execute(
            "SELECT * FROM generated_artifacts WHERE artifact_id = ?",
            (artifact_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["capabilities"] = _from_json(row.pop("capabilities_json", None), [])
        row["scopes"] = _from_json(row.pop("scopes_json", None), [])
        row["allowed_tools"] = _from_json(row.pop("allowed_tools_json", None), [])
        row["allowed_mcp_servers"] = _from_json(row.pop("allowed_mcp_servers_json", None), [])
        row["dependencies"] = _from_json(row.pop("dependencies_json", None), [])
        row["guardrails"] = _from_json(row.pop("guardrails_json", None), [])
        row["tests"] = _from_json(row.pop("tests_json", None), [])
        row["workflow_definition"] = _from_json(row.pop("workflow_definition_json", None), None)
        row["runtime_binding"] = _from_json(row.pop("runtime_binding_json", None), None)
        row["metadata"] = _from_json(row.pop("metadata_json", None), {})
        return row

    async def list_generated_artifacts(
        self,
        *,
        status: str | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """generated artifact 一覧を返す."""
        if status:
            rows = await self._execute(
                """
                SELECT * FROM generated_artifacts
                WHERE status = ?
                ORDER BY updated_at DESC
                LIMIT ? OFFSET ?
                """,
                (status, limit, offset),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM generated_artifacts
                ORDER BY updated_at DESC
                LIMIT ? OFFSET ?
                """,
                (limit, offset),
                fetch=True,
            )
        normalized: list[dict[str, Any]] = []
        for row in rows:
            artifact = await self.get_generated_artifact(str(row.get("artifact_id", "")))
            if artifact is not None:
                normalized.append(artifact)
        return normalized

    # =========================================================================
    # Flight Watch 操作
    # =========================================================================

    async def upsert_flight_watch_subscription(self, subscription: dict[str, Any]) -> None:
        """flight watch subscription を保存する."""
        created_at = str(subscription.get("created_at", _utc_now_iso()))
        updated_at = str(subscription.get("updated_at", created_at))
        await self._execute(
            """
            INSERT OR REPLACE INTO flight_watch_subscriptions (
                subscription_id, user_id, conversation_id, status, provider, request_json,
                ranking_weights_json, notification_targets_json, baseline_price, target_price,
                last_notified_price, last_checked_at, next_check_at, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(subscription.get("subscription_id", "")),
                str(subscription.get("user_id", "system")),
                subscription.get("conversation_id"),
                str(subscription.get("status", "active")),
                str(subscription.get("provider", "fake")),
                _to_json(subscription.get("request", {})),
                _to_json(subscription.get("ranking_weights", {})),
                _to_json(subscription.get("notification_targets", [])),
                subscription.get("baseline_price"),
                subscription.get("target_price"),
                subscription.get("last_notified_price"),
                subscription.get("last_checked_at"),
                str(subscription.get("next_check_at", _utc_now_iso())),
                created_at,
                updated_at,
            ),
        )

    async def get_flight_watch_subscription(self, subscription_id: str) -> dict[str, Any] | None:
        """flight watch subscription を取得する."""
        rows = await self._execute(
            "SELECT * FROM flight_watch_subscriptions WHERE subscription_id = ?",
            (subscription_id,),
            fetch=True,
        )
        if not rows:
            return None
        row = rows[0]
        row["request"] = _from_json(row.pop("request_json", None), {})
        row["ranking_weights"] = _from_json(row.pop("ranking_weights_json", None), {})
        row["notification_targets"] = _from_json(row.pop("notification_targets_json", None), [])
        return row

    async def list_flight_watch_subscriptions(
        self,
        *,
        status: str | None = None,
        due_before: str | None = None,
        limit: int = 200,
    ) -> list[dict[str, Any]]:
        """flight watch subscription 一覧を返す."""
        conditions: list[str] = []
        params: list[Any] = []
        if status:
            conditions.append("status = ?")
            params.append(status)
        if due_before:
            conditions.append("next_check_at <= ?")
            params.append(due_before)
        where_clause = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        rows = await self._execute(
            f"""
            SELECT * FROM flight_watch_subscriptions
            {where_clause}
            ORDER BY updated_at DESC
            LIMIT ?
            """,
            (*params, limit),
            fetch=True,
        )
        normalized: list[dict[str, Any]] = []
        for row in rows:
            subscription = await self.get_flight_watch_subscription(str(row.get("subscription_id", "")))
            if subscription is not None:
                normalized.append(subscription)
        return normalized

    async def add_flight_offer_snapshot(
        self,
        *,
        subscription_id: str,
        provider: str,
        lowest_price: float | None,
        offers: list[dict[str, Any]],
        checked_at: str | None = None,
    ) -> None:
        """flight offer snapshot を追加する."""
        await self._execute(
            """
            INSERT INTO flight_offer_snapshots (
                subscription_id, provider, lowest_price, offers_json, checked_at
            ) VALUES (?, ?, ?, ?, ?)
            """,
            (
                subscription_id,
                provider,
                lowest_price,
                _to_json(offers),
                str(checked_at or _utc_now_iso()),
            ),
        )

    async def list_flight_offer_snapshots(self, subscription_id: str, limit: int = 50) -> list[dict[str, Any]]:
        """flight offer snapshot 一覧を返す."""
        rows = await self._execute(
            """
            SELECT * FROM flight_offer_snapshots
            WHERE subscription_id = ?
            ORDER BY id DESC
            LIMIT ?
            """,
            (subscription_id, limit),
            fetch=True,
        )
        for row in rows:
            row["offers"] = _from_json(row.pop("offers_json", None), [])
        return rows

    async def add_notification_delivery(self, delivery: dict[str, Any]) -> None:
        """notification delivery を保存する."""
        await self._execute(
            """
            INSERT OR REPLACE INTO notification_deliveries (
                delivery_id, subscription_id, channel, destination, status,
                payload_json, error, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                str(delivery.get("delivery_id", "")),
                delivery.get("subscription_id"),
                str(delivery.get("channel", "in_app")),
                delivery.get("destination"),
                str(delivery.get("status", "pending")),
                _to_json(delivery.get("payload", {})),
                delivery.get("error"),
                str(delivery.get("created_at", _utc_now_iso())),
            ),
        )

    async def list_notification_deliveries(
        self,
        *,
        subscription_id: str | None = None,
        limit: int = 100,
    ) -> list[dict[str, Any]]:
        """notification delivery 一覧を返す."""
        if subscription_id:
            rows = await self._execute(
                """
                SELECT * FROM notification_deliveries
                WHERE subscription_id = ?
                ORDER BY created_at DESC
                LIMIT ?
                """,
                (subscription_id, limit),
                fetch=True,
            )
        else:
            rows = await self._execute(
                """
                SELECT * FROM notification_deliveries
                ORDER BY created_at DESC
                LIMIT ?
                """,
                (limit,),
                fetch=True,
            )
        for row in rows:
            row["payload"] = _from_json(row.pop("payload_json", None), {})
        return rows
