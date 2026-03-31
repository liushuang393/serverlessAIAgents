"""SQLite-backed repository for GEO Platform task state."""

from __future__ import annotations

import json
import sqlite3
import threading
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from apps.legacy_modernization_geo_platform.backend.schemas import (
    ApprovalRecord,
    ApprovalStatus,
    ArtifactRecord,
    GeoExecuteRequest,
    PublishedPageRecord,
    ReportPayload,
    TaskEvent,
    TaskStateResponse,
    TaskStatus,
)


if TYPE_CHECKING:
    from pathlib import Path


def _utcnow_iso() -> str:
    return datetime.now(UTC).isoformat()


def _row_to_approval(row: sqlite3.Row) -> ApprovalRecord:
    """sqlite3.Row を ApprovalRecord に変換するヘルパー."""
    return ApprovalRecord(
        request_id=str(row["request_id"]),
        task_id=str(row["task_id"]),
        stage=str(row["stage"]),
        object_id=str(row["object_id"]),
        risk_level=str(row["risk_level"]),
        reason=str(row["reason"]),
        status=ApprovalStatus(str(row["status"])),
        actions=list(json.loads(str(row["actions_json"]))),
        comment=row["comment"] if row["comment"] is None else str(row["comment"]),
        reviewer_name=(row["reviewer_name"] if row["reviewer_name"] is None else str(row["reviewer_name"])),
        created_at=datetime.fromisoformat(str(row["created_at"])),
        updated_at=datetime.fromisoformat(str(row["updated_at"])),
    )


class GeoRepository:
    """Persist tasks, artifacts, approvals, events, and reports.

    永続接続を1本保持し、threading.Lock で排他制御する。
    毎回 connect/close するオーバーヘッドを排除しつつ、
    SQLite の single-writer 特性に適合させる。
    """

    def __init__(self, db_path: Path) -> None:
        """Initialize the repository and create the schema if needed."""
        self._db_path = db_path
        self._lock = threading.Lock()
        self._db_path.parent.mkdir(parents=True, exist_ok=True)
        self._conn: sqlite3.Connection = self._create_connection()
        self._init_db()

    def _create_connection(self) -> sqlite3.Connection:
        conn = sqlite3.connect(self._db_path, check_same_thread=False)
        conn.row_factory = sqlite3.Row
        conn.execute("PRAGMA journal_mode=WAL")
        conn.execute("PRAGMA busy_timeout=5000")
        return conn

    def _connect(self) -> sqlite3.Connection:
        """永続接続を返す（既存の公開メソッドとの後方互換用）."""
        return self._conn

    def close(self) -> None:
        """永続接続を明示的にクローズする."""
        with self._lock:
            self._conn.close()

    def __del__(self) -> None:
        try:
            self._conn.close()
        except Exception:  # noqa: BLE001
            pass

    def _init_db(self) -> None:
        with self._lock:
            cursor = self._conn.cursor()
            cursor.executescript(
                """
                CREATE TABLE IF NOT EXISTS tasks (
                    task_id TEXT PRIMARY KEY,
                    campaign_name TEXT NOT NULL,
                    package_name TEXT NOT NULL,
                    status TEXT NOT NULL,
                    current_stage TEXT,
                    request_json TEXT NOT NULL,
                    error_message TEXT,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS task_events (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    task_id TEXT NOT NULL,
                    event_json TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS artifacts (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    task_id TEXT NOT NULL,
                    artifact_name TEXT NOT NULL,
                    stage TEXT NOT NULL,
                    path TEXT NOT NULL,
                    summary TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS approvals (
                    request_id TEXT PRIMARY KEY,
                    task_id TEXT NOT NULL,
                    stage TEXT NOT NULL,
                    object_id TEXT NOT NULL,
                    risk_level TEXT NOT NULL,
                    reason TEXT NOT NULL,
                    status TEXT NOT NULL,
                    actions_json TEXT NOT NULL,
                    comment TEXT,
                    reviewer_name TEXT,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS published_pages (
                    slug TEXT PRIMARY KEY,
                    task_id TEXT NOT NULL,
                    title TEXT NOT NULL,
                    page_url TEXT NOT NULL,
                    html_path TEXT NOT NULL,
                    created_at TEXT NOT NULL
                );

                CREATE TABLE IF NOT EXISTS reports (
                    task_id TEXT PRIMARY KEY,
                    markdown TEXT NOT NULL,
                    summary_json TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );
                """,
            )
            self._conn.commit()

    def create_task(self, task_id: str, request: GeoExecuteRequest, status: TaskStatus) -> None:
        """Insert a new task row."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO tasks (
                    task_id, campaign_name, package_name, status, current_stage,
                    request_json, error_message, created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    task_id,
                    request.campaign_name,
                    request.package,
                    status.value,
                    None,
                    request.model_dump_json(),
                    None,
                    now,
                    now,
                ),
            )
            self._conn.commit()

    def update_task(
        self,
        task_id: str,
        *,
        status: TaskStatus,
        current_stage: str | None = None,
        error_message: str | None = None,
    ) -> None:
        """Update task status and optional current stage."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                UPDATE tasks
                SET status = ?, current_stage = ?, error_message = ?, updated_at = ?
                WHERE task_id = ?
                """,
                (status.value, current_stage, error_message, now, task_id),
            )
            self._conn.commit()

    def add_event(self, event: TaskEvent) -> None:
        """Persist an event payload."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO task_events (task_id, event_json, created_at)
                VALUES (?, ?, ?)
                """,
                (event.task_id, event.model_dump_json(), now),
            )
            self._conn.commit()

    def list_events(self, task_id: str) -> list[TaskEvent]:
        """Return all events for a task."""
        with self._lock:
            rows = self._conn.execute(
                "SELECT event_json FROM task_events WHERE task_id = ? ORDER BY id ASC",
                (task_id,),
            ).fetchall()
        return [TaskEvent.model_validate_json(str(row["event_json"])) for row in rows]

    def add_artifact(
        self,
        task_id: str,
        *,
        artifact_name: str,
        stage: str,
        path: Path,
        summary: str,
    ) -> None:
        """Record an artifact output."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO artifacts (task_id, artifact_name, stage, path, summary, created_at)
                VALUES (?, ?, ?, ?, ?, ?)
                """,
                (task_id, artifact_name, stage, str(path), summary, now),
            )
            self._conn.commit()

    def list_artifacts(self, task_id: str) -> list[ArtifactRecord]:
        """List artifacts for a task."""
        with self._lock:
            rows = self._conn.execute(
                """
                SELECT artifact_name, stage, path, summary, created_at
                FROM artifacts
                WHERE task_id = ?
                ORDER BY id ASC
                """,
                (task_id,),
            ).fetchall()
        records: list[ArtifactRecord] = []
        for row in rows:
            records.append(
                ArtifactRecord(
                    artifact_name=str(row["artifact_name"]),
                    stage=str(row["stage"]),
                    path=str(row["path"]),
                    summary=str(row["summary"]),
                    created_at=datetime.fromisoformat(str(row["created_at"])),
                ),
            )
        return records

    def create_approval(
        self,
        *,
        request_id: str,
        task_id: str,
        stage: str,
        object_id: str,
        risk_level: str,
        reason: str,
        actions: list[str],
    ) -> ApprovalRecord:
        """Create a pending approval record."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO approvals (
                    request_id, task_id, stage, object_id, risk_level, reason, status,
                    actions_json, comment, reviewer_name, created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    request_id,
                    task_id,
                    stage,
                    object_id,
                    risk_level,
                    reason,
                    ApprovalStatus.PENDING.value,
                    json.dumps(actions),
                    None,
                    None,
                    now,
                    now,
                ),
            )
            self._conn.commit()
        return self.get_approval(request_id)

    def update_approval(
        self,
        request_id: str,
        *,
        status: ApprovalStatus,
        comment: str | None,
        reviewer_name: str | None,
    ) -> ApprovalRecord:
        """Update an approval decision."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                UPDATE approvals
                SET status = ?, comment = ?, reviewer_name = ?, updated_at = ?
                WHERE request_id = ?
                """,
                (status.value, comment, reviewer_name, now, request_id),
            )
            self._conn.commit()
        return self.get_approval(request_id)

    def get_approval(self, request_id: str) -> ApprovalRecord:
        """Fetch a single approval record."""
        with self._lock:
            row = self._conn.execute(
                """
                SELECT request_id, task_id, stage, object_id, risk_level, reason, status,
                       actions_json, comment, reviewer_name, created_at, updated_at
                FROM approvals
                WHERE request_id = ?
                """,
                (request_id,),
            ).fetchone()
        if row is None:
            msg = f"Approval not found: {request_id}"
            raise KeyError(msg)
        return _row_to_approval(row)

    def list_approvals(self, task_id: str) -> list[ApprovalRecord]:
        """List approvals for a task (1 クエリで全カラム取得、N+1 回避)."""
        with self._lock:
            rows = self._conn.execute(
                """
                SELECT request_id, task_id, stage, object_id, risk_level, reason, status,
                       actions_json, comment, reviewer_name, created_at, updated_at
                FROM approvals
                WHERE task_id = ?
                ORDER BY created_at ASC
                """,
                (task_id,),
            ).fetchall()
        return [_row_to_approval(row) for row in rows]

    def save_published_page(self, record: PublishedPageRecord, task_id: str) -> None:
        """Upsert a published page mapping."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO published_pages (slug, task_id, title, page_url, html_path, created_at)
                VALUES (?, ?, ?, ?, ?, ?)
                ON CONFLICT(slug) DO UPDATE SET
                    task_id = excluded.task_id,
                    title = excluded.title,
                    page_url = excluded.page_url,
                    html_path = excluded.html_path,
                    created_at = excluded.created_at
                """,
                (
                    record.slug,
                    task_id,
                    record.title,
                    record.page_url,
                    record.html_path,
                    now,
                ),
            )
            self._conn.commit()

    def list_published_pages(self, task_id: str) -> list[PublishedPageRecord]:
        """List published pages for a task."""
        with self._lock:
            rows = self._conn.execute(
                """
                SELECT slug, title, page_url, html_path
                FROM published_pages
                WHERE task_id = ?
                ORDER BY slug ASC
                """,
                (task_id,),
            ).fetchall()
        return [
            PublishedPageRecord(
                slug=str(row["slug"]),
                title=str(row["title"]),
                page_url=str(row["page_url"]),
                html_path=str(row["html_path"]),
            )
            for row in rows
        ]

    def get_published_page(self, slug: str) -> PublishedPageRecord | None:
        """Resolve a published page by slug."""
        with self._lock:
            row = self._conn.execute(
                """
                SELECT slug, title, page_url, html_path
                FROM published_pages
                WHERE slug = ?
                """,
                (slug,),
            ).fetchone()
        if row is None:
            return None
        return PublishedPageRecord(
            slug=str(row["slug"]),
            title=str(row["title"]),
            page_url=str(row["page_url"]),
            html_path=str(row["html_path"]),
        )

    def save_report(self, task_id: str, markdown: str, summary: dict[str, Any]) -> None:
        """Upsert a structured report."""
        now = _utcnow_iso()
        with self._lock:
            self._conn.execute(
                """
                INSERT INTO reports (task_id, markdown, summary_json, created_at, updated_at)
                VALUES (?, ?, ?, ?, ?)
                ON CONFLICT(task_id) DO UPDATE SET
                    markdown = excluded.markdown,
                    summary_json = excluded.summary_json,
                    updated_at = excluded.updated_at
                """,
                (task_id, markdown, json.dumps(summary), now, now),
            )
            self._conn.commit()

    def get_report(self, task_id: str) -> ReportPayload | None:
        """Return the stored report if present."""
        with self._lock:
            row = self._conn.execute(
                """
                SELECT task_id, markdown, summary_json, created_at, updated_at
                FROM reports
                WHERE task_id = ?
                """,
                (task_id,),
            ).fetchone()
        if row is None:
            return None
        return ReportPayload(
            task_id=str(row["task_id"]),
            markdown=str(row["markdown"]),
            summary=dict(json.loads(str(row["summary_json"]))),
            created_at=datetime.fromisoformat(str(row["created_at"])),
            updated_at=datetime.fromisoformat(str(row["updated_at"])),
        )

    def get_task_state(self, task_id: str) -> TaskStateResponse | None:
        """タスクの集約状態を 1 ロック内で一括取得する（N+1 解消）."""
        with self._lock:
            task_row = self._conn.execute(
                """
                SELECT task_id, campaign_name, package_name, status,
                       current_stage, request_json, error_message
                FROM tasks WHERE task_id = ?
                """,
                (task_id,),
            ).fetchone()
            if task_row is None:
                return None

            event_rows = self._conn.execute(
                "SELECT event_json FROM task_events WHERE task_id = ? ORDER BY id ASC",
                (task_id,),
            ).fetchall()

            artifact_rows = self._conn.execute(
                """
                SELECT artifact_name, stage, path, summary, created_at
                FROM artifacts WHERE task_id = ? ORDER BY id ASC
                """,
                (task_id,),
            ).fetchall()

            approval_rows = self._conn.execute(
                """
                SELECT request_id, task_id, stage, object_id, risk_level, reason, status,
                       actions_json, comment, reviewer_name, created_at, updated_at
                FROM approvals WHERE task_id = ? ORDER BY created_at ASC
                """,
                (task_id,),
            ).fetchall()

            report_row = self._conn.execute(
                """
                SELECT task_id, markdown, summary_json, created_at, updated_at
                FROM reports WHERE task_id = ?
                """,
                (task_id,),
            ).fetchone()

            page_rows = self._conn.execute(
                """
                SELECT slug, title, page_url, html_path
                FROM published_pages WHERE task_id = ? ORDER BY slug ASC
                """,
                (task_id,),
            ).fetchall()

        events = [TaskEvent.model_validate_json(str(r["event_json"])) for r in event_rows]
        artifacts = [
            ArtifactRecord(
                artifact_name=str(r["artifact_name"]),
                stage=str(r["stage"]),
                path=str(r["path"]),
                summary=str(r["summary"]),
                created_at=datetime.fromisoformat(str(r["created_at"])),
            )
            for r in artifact_rows
        ]
        approvals = [_row_to_approval(r) for r in approval_rows]
        report: ReportPayload | None = None
        if report_row is not None:
            report = ReportPayload(
                task_id=str(report_row["task_id"]),
                markdown=str(report_row["markdown"]),
                summary=dict(json.loads(str(report_row["summary_json"]))),
                created_at=datetime.fromisoformat(str(report_row["created_at"])),
                updated_at=datetime.fromisoformat(str(report_row["updated_at"])),
            )
        pages = [
            PublishedPageRecord(
                slug=str(r["slug"]),
                title=str(r["title"]),
                page_url=str(r["page_url"]),
                html_path=str(r["html_path"]),
            )
            for r in page_rows
        ]

        return TaskStateResponse(
            task_id=str(task_row["task_id"]),
            status=TaskStatus(str(task_row["status"])),
            current_stage=(
                task_row["current_stage"] if task_row["current_stage"] is None else str(task_row["current_stage"])
            ),
            campaign_name=str(task_row["campaign_name"]),
            package=str(task_row["package_name"]),
            request=GeoExecuteRequest.model_validate_json(str(task_row["request_json"])),
            events=events,
            artifacts=artifacts,
            approvals=approvals,
            report=report,
            published_pages=pages,
            error_message=(
                task_row["error_message"] if task_row["error_message"] is None else str(task_row["error_message"])
            ),
        )
