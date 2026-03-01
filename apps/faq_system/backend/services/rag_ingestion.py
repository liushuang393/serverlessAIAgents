"""RAG ingestion orchestrator for FAQ app."""

from __future__ import annotations

import asyncio
import contextlib
import hashlib
import json
import logging
import re
import time
import unicodedata
import uuid
from collections import deque
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.db.models import IngestionCheckpoint, IngestionRun, IngestionRunItem
from apps.faq_system.backend.db.session import ensure_database_ready, get_db_session
from sqlalchemy import Select, select

from agentflow.knowledge.document_loader import UniversalLoader
from agentflow.providers.db_provider import SQLAlchemyDBProvider
from agentflow.security import DataSanitizer


try:
    import sqlglot
    from sqlglot import exp as sql_exp
except ImportError:  # pragma: no cover - optional dependency
    sqlglot = None
    sql_exp = None

try:
    import trafilatura
except ImportError:  # pragma: no cover - optional dependency
    trafilatura = None

try:
    from tenacity import AsyncRetrying, retry_if_exception_type, stop_after_attempt, wait_exponential
except ImportError:  # pragma: no cover - optional dependency
    AsyncRetrying = None
    retry_if_exception_type = None
    stop_after_attempt = None
    wait_exponential = None


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator, Awaitable, Callable

    from apps.faq_system.backend.services.rag_runtime_config import RAGDataSourceConfig, RAGRuntimeConfig
    from sqlalchemy.sql.elements import ClauseElement
else:
    RAGDataSourceConfig = Any
    RAGRuntimeConfig = Any


logger = logging.getLogger(__name__)

_IDENTIFIER_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
_CONTROL_CHAR_RE = re.compile(r"[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]")
_WHITESPACE_RE = re.compile(r"[ \t]+")
_NEWLINES_RE = re.compile(r"\n{3,}")

_SQL_DIALECT_MAP: dict[str, str] = {
    "postgresql": "postgres",
    "postgres": "postgres",
    "mysql": "mysql",
    "sqlite": "sqlite",
    "mssql": "tsql",
    "sqlserver": "tsql",
}

_UNSUPPORTED_SOURCE_MESSAGE = {
    "web": "web source extraction is staged and not implemented yet",
    "api": "api source extraction is staged and not implemented yet",
    "s3": "s3 source extraction is staged and not implemented yet",
}


@dataclass(slots=True)
class IngestionScheduleEntry:
    """Schedule definition for one source."""

    source_id: str
    schedule: str
    label: str


@dataclass(slots=True)
class CheckpointState:
    """Incremental ingest checkpoint."""

    source_id: str
    cursor_text: str | None = None
    cursor_time: datetime | None = None
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class IngestionContext:
    """One ingestion run context."""

    run_id: str
    config: RAGRuntimeConfig
    dry_run: bool
    rag_service: Any | None
    seen_hashes: set[str] = field(default_factory=set)


class SourceAdapter:
    """Source adapter protocol."""

    async def ingest(
        self,
        *,
        context: IngestionContext,
        source: RAGDataSourceConfig,
    ) -> dict[str, Any]:
        """Ingest one source and return source result payload."""
        raise NotImplementedError


class FileSourceAdapter(SourceAdapter):
    """File source ingestion adapter."""

    def __init__(self, orchestrator: RAGIngestionOrchestrator) -> None:
        self._orchestrator = orchestrator
        self._loader = UniversalLoader()

    async def ingest(
        self,
        *,
        context: IngestionContext,
        source: RAGDataSourceConfig,
    ) -> dict[str, Any]:
        uri = _clean_text(source.uri)
        if not uri:
            return _failed_source_result(
                source=source,
                message="file source requires uri/path",
            )

        path = Path(uri)
        if not path.exists():
            return _failed_source_result(source=source, message=f"path not found: {uri}")

        glob_pattern = _clean_text(source.options.get("glob")) or "*"
        recursive = _coerce_bool(source.options.get("recursive"), default=True)

        if context.dry_run:
            return {
                "source_id": source.source_id,
                "type": "file",
                "status": "success",
                "planned_action": "load_files",
                "path": str(path),
                "glob": glob_pattern,
                "recursive": recursive,
            }

        if path.is_dir():
            chunks = await self._loader.load_directory(path, pattern=glob_pattern, recursive=recursive)
        else:
            chunks = await self._loader.load(path)

        added = 0
        deduped = 0
        masked = 0
        skipped_empty = 0

        for chunk in chunks:
            normalized = _normalize_text(chunk.content)
            if not normalized:
                skipped_empty += 1
                continue

            sanitized = self._orchestrator._sanitizer.sanitize(normalized)
            sanitized_text = _normalize_text(sanitized.sanitized_text)
            if not sanitized_text:
                skipped_empty += 1
                continue

            content_hash = _content_hash(sanitized_text)
            if content_hash in context.seen_hashes:
                deduped += 1
                continue
            context.seen_hashes.add(content_hash)

            if sanitized.detections:
                masked += 1

            metadata = {
                "source_id": source.source_id,
                "source_type": "file",
                "source_label": source.label,
                "source_uri": uri,
                "document_source": chunk.source,
                "chunk_id": chunk.id,
                "chunk_index": chunk.chunk_index,
                "total_chunks": chunk.total_chunks,
                "content_hash": content_hash,
                "detections": len(sanitized.detections),
            }
            if chunk.metadata:
                metadata.update(chunk.metadata)

            add_result = await self._orchestrator._add_document_with_retry(
                rag_service=context.rag_service,
                content=sanitized_text,
                source=chunk.source or uri,
                metadata=metadata,
            )
            added += int(add_result.get("count", 0))

        return {
            "source_id": source.source_id,
            "type": "file",
            "status": "success",
            "chunk_count": len(chunks),
            "added_chunks": added,
            "deduped_chunks": deduped,
            "masked_chunks": masked,
            "skipped_empty_chunks": skipped_empty,
            "path": str(path),
        }


class DatabaseSourceAdapter(SourceAdapter):
    """Database source ingestion adapter."""

    def __init__(self, orchestrator: RAGIngestionOrchestrator) -> None:
        self._orchestrator = orchestrator

    async def ingest(
        self,
        *,
        context: IngestionContext,
        source: RAGDataSourceConfig,
    ) -> dict[str, Any]:
        options = source.options
        dialect = (
            _clean_text(options.get("dialect"))
            or _clean_text(options.get("database_type"))
            or _clean_text(context.config.database_kind)
            or "postgresql"
        ).lower()
        row_limit = _coerce_positive_int(options.get("row_limit"), default=200, minimum=1, maximum=5000)
        db_url = _clean_text(source.uri) or _clean_text(context.config.database_url)
        if not db_url:
            return _failed_source_result(
                source=source,
                message="database uri is missing (source.uri/runtime.database.url)",
            )

        if not _is_supported_db_url(db_url):
            return _failed_source_result(
                source=source,
                message=f"unsupported db url scheme: {db_url.split('://', 1)[0]}",
            )

        read_mode = (_clean_text(options.get("read_mode")) or "query").lower()
        if read_mode not in {"table", "query"}:
            return _failed_source_result(source=source, message=f"unsupported read_mode: {read_mode}")

        checkpoint = await self._orchestrator._load_checkpoint(source.source_id)
        time_column = _clean_text(options.get("time_column"))
        cursor_column = _clean_text(options.get("cursor_column"))

        if read_mode == "query":
            raw_query = _clean_text(options.get("query"))
            if not raw_query:
                return _failed_source_result(source=source, message="read_mode=query requires options.query")
            guard = _guard_select_sql(raw_query, dialect=dialect)
            if not guard["ok"]:
                return _failed_source_result(source=source, message=guard["message"])
            queries = [
                _apply_incremental_filters(
                    sql=guard["sql"],
                    dialect=dialect,
                    row_limit=row_limit,
                    time_column=time_column,
                    cursor_column=cursor_column,
                    checkpoint=checkpoint,
                )
            ]
        else:
            table_names = _resolve_table_names(options)
            if not table_names:
                return _failed_source_result(
                    source=source,
                    message="read_mode=table requires options.table or options.tables",
                )
            queries = []
            for table_name in table_names:
                sql = _build_table_select_sql(table_name=table_name, row_limit=row_limit, dialect=dialect)
                queries.append(
                    _apply_incremental_filters(
                        sql=sql,
                        dialect=dialect,
                        row_limit=row_limit,
                        time_column=time_column,
                        cursor_column=cursor_column,
                        checkpoint=checkpoint,
                    )
                )

        if context.dry_run:
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "success",
                "planned_action": "query",
                "dialect": dialect,
                "read_mode": read_mode,
                "query_count": len(queries),
                "queries": queries,
                "checkpoint": {
                    "cursor_text": checkpoint.cursor_text if checkpoint else None,
                    "cursor_time": checkpoint.cursor_time.isoformat() if checkpoint and checkpoint.cursor_time else None,
                },
            }

        provider = SQLAlchemyDBProvider(db_url)
        total_rows = 0
        added_chunks = 0
        deduped_rows = 0
        masked_rows = 0

        next_cursor_text = checkpoint.cursor_text if checkpoint else None
        next_cursor_time = checkpoint.cursor_time if checkpoint else None

        try:
            await _retry_async(provider.connect)
            for sql in queries:
                rows = await _retry_async(provider.execute_raw, sql)
                if row_limit > 0:
                    rows = rows[:row_limit]
                total_rows += len(rows)

                for row in rows:
                    content = _build_db_row_content(
                        source=source,
                        sql=sql,
                        row=row,
                        dialect=dialect,
                    )
                    normalized = _normalize_text(content)
                    sanitized = self._orchestrator._sanitizer.sanitize(normalized)
                    sanitized_text = _normalize_text(sanitized.sanitized_text)
                    if not sanitized_text:
                        continue

                    content_hash = _content_hash(sanitized_text)
                    if content_hash in context.seen_hashes:
                        deduped_rows += 1
                        continue
                    context.seen_hashes.add(content_hash)

                    if sanitized.detections:
                        masked_rows += 1

                    metadata = {
                        "source_id": source.source_id,
                        "source_type": "database",
                        "source_label": source.label,
                        "source_uri": source.uri,
                        "dialect": dialect,
                        "read_mode": read_mode,
                        "content_hash": content_hash,
                        "detections": len(sanitized.detections),
                    }
                    add_result = await self._orchestrator._add_document_with_retry(
                        rag_service=context.rag_service,
                        content=sanitized_text,
                        source=source.label or source.uri or "database",
                        metadata=metadata,
                    )
                    added_chunks += int(add_result.get("count", 0))

                    if time_column:
                        candidate_time = _extract_datetime(row.get(time_column))
                        if candidate_time is not None and (next_cursor_time is None or candidate_time > next_cursor_time):
                            next_cursor_time = candidate_time
                    if cursor_column:
                        candidate_cursor = _clean_text(row.get(cursor_column))
                        if candidate_cursor and (next_cursor_text is None or candidate_cursor > next_cursor_text):
                            next_cursor_text = candidate_cursor

            await self._orchestrator._save_checkpoint(
                source_id=source.source_id,
                cursor_text=next_cursor_text,
                cursor_time=next_cursor_time,
                metadata={
                    "time_column": time_column,
                    "cursor_column": cursor_column,
                    "dialect": dialect,
                },
            )
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "success",
                "dialect": dialect,
                "read_mode": read_mode,
                "query_count": len(queries),
                "row_count": total_rows,
                "added_chunks": added_chunks,
                "deduped_rows": deduped_rows,
                "masked_rows": masked_rows,
                "checkpoint": {
                    "cursor_text": next_cursor_text,
                    "cursor_time": next_cursor_time.isoformat() if next_cursor_time else None,
                },
            }
        except Exception as exc:  # pragma: no cover - defensive
            logger.exception("Database ingestion failed: %s", exc)
            return _failed_source_result(source=source, message=str(exc))
        finally:
            with contextlib.suppress(Exception):
                await provider.disconnect()


class StagedSourceAdapter(SourceAdapter):
    """Not implemented source adapter."""

    def __init__(self, source_type: str) -> None:
        self._source_type = source_type

    async def ingest(
        self,
        *,
        context: IngestionContext,
        source: RAGDataSourceConfig,
    ) -> dict[str, Any]:
        uri = _clean_text(source.uri)
        if not uri:
            return _failed_source_result(source=source, message="uri is required")

        preview: dict[str, Any] = {}
        if self._source_type == "web" and trafilatura is not None and context.dry_run:
            try:
                fetched = trafilatura.fetch_url(uri)
                extracted = trafilatura.extract(fetched) if fetched else None
                preview = {
                    "preview_available": bool(extracted),
                    "preview_length": len(extracted or ""),
                }
            except Exception:
                preview = {"preview_available": False}

        return {
            "source_id": source.source_id,
            "type": self._source_type,
            "status": "skipped",
            "reason": "not_implemented",
            "message": _UNSUPPORTED_SOURCE_MESSAGE[self._source_type],
            "uri": uri,
            **preview,
        }


class RAGIngestionOrchestrator:
    """Config-driven ingestion executor."""

    def __init__(
        self,
        *,
        config_loader: Callable[[], RAGRuntimeConfig],
        rag_service_getter: Callable[[str], Any],
        run_limit: int = 50,
    ) -> None:
        self._config_loader = config_loader
        self._rag_service_getter = rag_service_getter
        self._runs: deque[dict[str, Any]] = deque(maxlen=max(10, run_limit))
        self._run_events: dict[str, deque[dict[str, Any]]] = {}
        self._event_subscribers: dict[str, list[asyncio.Queue[dict[str, Any]]]] = {}
        self._background_tasks: dict[str, asyncio.Task[None]] = {}
        self._run_store: dict[str, dict[str, Any]] = {}
        self._checkpoints_mem: dict[str, CheckpointState] = {}
        self._db_available = True
        self._sanitizer = DataSanitizer()
        self._adapters: dict[str, SourceAdapter] = {
            "file": FileSourceAdapter(self),
            "database": DatabaseSourceAdapter(self),
            "web": StagedSourceAdapter("web"),
            "api": StagedSourceAdapter("api"),
            "s3": StagedSourceAdapter("s3"),
        }

    async def list_runs(self, limit: int = 20) -> list[dict[str, Any]]:
        """Return recent ingestion runs (latest first)."""
        normalized_limit = max(1, min(limit, 200))
        db_runs = await self._list_runs_from_db(limit=normalized_limit)
        if db_runs:
            return db_runs
        return list(self._runs)[:normalized_limit]

    async def get_run(self, run_id: str) -> dict[str, Any] | None:
        """Return one run detail."""
        run = await self._get_run_from_db(run_id)
        if run is not None:
            return run
        for item in self._runs:
            if item.get("run_id") == run_id:
                return item
        return None

    async def list_run_events(self, run_id: str, limit: int = 200) -> list[dict[str, Any]]:
        """Return in-memory events for one run."""
        if run_id not in self._run_events:
            return []
        normalized_limit = max(1, min(limit, 1000))
        events = list(self._run_events[run_id])
        return events[-normalized_limit:]

    async def subscribe_run_events(self, run_id: str) -> AsyncGenerator[dict[str, Any]]:
        """Subscribe run events for SSE streaming."""
        queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue(maxsize=100)
        subscribers = self._event_subscribers.setdefault(run_id, [])
        subscribers.append(queue)
        try:
            while True:
                event = await queue.get()
                yield event
        except asyncio.CancelledError:
            return
        finally:
            with contextlib.suppress(ValueError):
                subscribers.remove(queue)

    def resolve_schedule_entries(self) -> list[IngestionScheduleEntry]:
        """Resolve effective cron entries.

        優先度: source.schedule > rag.indexing_schedule > none
        """
        config = self._config_loader()
        default_schedule = _clean_text(config.indexing_schedule)
        entries: list[IngestionScheduleEntry] = []

        for source in config.data_sources:
            if not source.enabled:
                continue
            schedule = _clean_text(source.schedule) or default_schedule
            if not schedule:
                continue
            entries.append(
                IngestionScheduleEntry(
                    source_id=source.source_id,
                    schedule=schedule,
                    label=source.label or source.uri or source.source_type,
                )
            )
        return entries

    async def enqueue_ingest(
        self,
        *,
        source_ids: list[str] | None = None,
        dry_run: bool = False,
    ) -> dict[str, Any]:
        """Queue ingest run and execute in background."""
        run_id = f"ingest-{uuid.uuid4().hex[:12]}"
        requested = [item.strip() for item in source_ids or [] if item and item.strip()]
        await self._create_run_record(
            run_id=run_id,
            status="queued",
            trigger_mode="async",
            dry_run=dry_run,
            source_ids=requested,
        )
        await self._emit_event(
            run_id,
            "queued",
            {
                "status": "queued",
                "dry_run": dry_run,
                "source_ids": requested,
            },
        )

        task = asyncio.create_task(
            self._run_background(run_id=run_id, source_ids=requested or None, dry_run=dry_run),
            name=f"faq_rag_ingest_{run_id}",
        )
        self._background_tasks[run_id] = task
        run = await self.get_run(run_id)
        return run or {"run_id": run_id, "status": "queued", "dry_run": dry_run}

    async def ingest(
        self,
        *,
        source_ids: list[str] | None = None,
        dry_run: bool = False,
    ) -> dict[str, Any]:
        """Execute ingestion run synchronously."""
        run_id = f"ingest-{uuid.uuid4().hex[:12]}"
        requested = [item.strip() for item in source_ids or [] if item and item.strip()]
        await self._create_run_record(
            run_id=run_id,
            status="running",
            trigger_mode="sync",
            dry_run=dry_run,
            source_ids=requested,
        )
        await self._emit_event(
            run_id,
            "running",
            {
                "status": "running",
                "dry_run": dry_run,
                "source_ids": requested,
            },
        )
        return await self._execute_run(
            run_id=run_id,
            source_ids=requested or None,
            dry_run=dry_run,
        )

    async def _run_background(
        self,
        *,
        run_id: str,
        source_ids: list[str] | None,
        dry_run: bool,
    ) -> None:
        try:
            await self._update_run_status(run_id=run_id, status="running")
            await self._emit_event(run_id, "running", {"status": "running"})
            await self._execute_run(run_id=run_id, source_ids=source_ids, dry_run=dry_run)
        except Exception as exc:  # pragma: no cover - defensive
            logger.exception("Background ingest failed: %s", exc)
            await self._finalize_run(
                run_id=run_id,
                status="failed",
                summary={
                    "total_sources": 0,
                    "success_sources": 0,
                    "failed_sources": 1,
                    "skipped_sources": 0,
                },
                error_message=str(exc),
            )
            await self._emit_event(run_id, "failed", {"status": "failed", "message": str(exc)})
        finally:
            self._background_tasks.pop(run_id, None)

    async def _execute_run(
        self,
        *,
        run_id: str,
        source_ids: list[str] | None,
        dry_run: bool,
    ) -> dict[str, Any]:
        config = self._config_loader()
        started_perf = time.perf_counter()

        rag_service = None if dry_run else self._rag_service_getter(config.rag_collection)
        context = IngestionContext(
            run_id=run_id,
            config=config,
            dry_run=dry_run,
            rag_service=rag_service,
        )

        if not config.rag_enabled:
            summary = {
                "total_sources": 0,
                "success_sources": 0,
                "failed_sources": 0,
                "skipped_sources": 1,
            }
            await self._finalize_run(
                run_id=run_id,
                status="skipped",
                summary=summary,
                error_message="rag_disabled",
            )
            payload = await self.get_run(run_id)
            return payload or {
                "run_id": run_id,
                "status": "skipped",
                "reason": "rag_disabled",
                "dry_run": dry_run,
            }

        requested = {item.strip() for item in source_ids or [] if item and item.strip()}
        candidates = [source for source in config.data_sources if source.enabled]
        if requested:
            candidates = [source for source in candidates if source.source_id in requested]

        if not candidates:
            summary = {
                "total_sources": 0,
                "success_sources": 0,
                "failed_sources": 0,
                "skipped_sources": 1,
            }
            await self._finalize_run(
                run_id=run_id,
                status="skipped",
                summary=summary,
                error_message="no_sources",
            )
            payload = await self.get_run(run_id)
            return payload or {
                "run_id": run_id,
                "status": "skipped",
                "reason": "no_sources",
                "dry_run": dry_run,
            }

        success_count = 0
        failed_count = 0
        skipped_count = 0
        results: list[dict[str, Any]] = []

        for source in candidates:
            adapter = self._adapters.get(source.source_type.lower())
            if adapter is None:
                item = _failed_source_result(source=source, message=f"unsupported source type: {source.source_type}")
            else:
                try:
                    item = await adapter.ingest(context=context, source=source)
                except Exception as exc:  # pragma: no cover - defensive
                    logger.exception("Source ingest failed: source=%s error=%s", source.source_id, exc)
                    item = _failed_source_result(source=source, message=str(exc))

            results.append(item)
            await self._append_run_item(run_id=run_id, item=item)
            await self._emit_event(
                run_id,
                "source_processed",
                {
                    "source_id": source.source_id,
                    "source_type": source.source_type,
                    "result": item,
                },
            )

            status = item.get("status")
            if status == "success":
                success_count += 1
            elif status == "skipped":
                skipped_count += 1
            else:
                failed_count += 1

        duration_ms = int((time.perf_counter() - started_perf) * 1000)
        status = "success" if failed_count == 0 else ("partial_success" if success_count > 0 else "failed")
        summary = {
            "total_sources": len(candidates),
            "success_sources": success_count,
            "failed_sources": failed_count,
            "skipped_sources": skipped_count,
            "duration_ms": duration_ms,
        }
        await self._finalize_run(
            run_id=run_id,
            status=status,
            summary=summary,
            error_message=None,
        )
        await self._emit_event(
            run_id,
            "completed",
            {
                "status": status,
                "summary": summary,
            },
        )

        payload = await self.get_run(run_id)
        if payload is not None:
            self._runs.appendleft(payload)
            return payload

        return {
            "run_id": run_id,
            "status": status,
            "dry_run": dry_run,
            "summary": summary,
            "results": results,
        }

    async def _list_runs_from_db(self, *, limit: int) -> list[dict[str, Any]]:
        if not self._db_available:
            return self._list_runs_from_memory(limit)
        try:
            await ensure_database_ready()
            stmt: Select[tuple[IngestionRun]] = (
                select(IngestionRun).order_by(IngestionRun.started_at.desc()).limit(limit)
            )
            async with get_db_session() as session:
                rows = (await session.execute(stmt)).scalars().all()
            return [self._serialize_run_model(run) for run in rows]
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)
            return self._list_runs_from_memory(limit)

    async def _get_run_from_db(self, run_id: str) -> dict[str, Any] | None:
        if not self._db_available:
            return self._run_store.get(run_id)
        try:
            await ensure_database_ready()
            run_stmt: Select[tuple[IngestionRun]] = select(IngestionRun).where(IngestionRun.id == run_id)
            item_stmt: Select[tuple[IngestionRunItem]] = (
                select(IngestionRunItem)
                .where(IngestionRunItem.run_id == run_id)
                .order_by(IngestionRunItem.id.asc())
            )
            async with get_db_session() as session:
                run = (await session.execute(run_stmt)).scalars().first()
                if run is None:
                    return self._run_store.get(run_id)
                items = (await session.execute(item_stmt)).scalars().all()
            return self._serialize_run_model(run, items=items)
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)
            return self._run_store.get(run_id)

    async def _create_run_record(
        self,
        *,
        run_id: str,
        status: str,
        trigger_mode: str,
        dry_run: bool,
        source_ids: list[str],
    ) -> None:
        now = datetime.now(tz=UTC)
        self._run_store[run_id] = {
            "run_id": run_id,
            "status": status,
            "trigger_mode": trigger_mode,
            "dry_run": dry_run,
            "source_ids": source_ids,
            "summary": {},
            "error_message": None,
            "started_at": now.isoformat(),
            "finished_at": None,
            "duration_ms": 0,
            "results": [],
        }
        if not self._db_available:
            return
        try:
            await ensure_database_ready()
            async with get_db_session() as session:
                session.add(
                    IngestionRun(
                        id=run_id,
                        app_name=self._config_loader().app_name,
                        status=status,
                        trigger_mode=trigger_mode,
                        dry_run=dry_run,
                        source_ids_json=source_ids,
                        summary_json={},
                        error_message=None,
                        started_at=now,
                        finished_at=None,
                        created_at=now,
                        updated_at=now,
                    )
                )
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)

    async def _update_run_status(self, *, run_id: str, status: str) -> None:
        run = self._run_store.get(run_id)
        if run is not None:
            run["status"] = status
        if not self._db_available:
            return
        try:
            await ensure_database_ready()
            stmt: Select[tuple[IngestionRun]] = select(IngestionRun).where(IngestionRun.id == run_id)
            async with get_db_session() as session:
                model = (await session.execute(stmt)).scalars().first()
                if model is None:
                    return
                model.status = status
                model.updated_at = datetime.now(tz=UTC)
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)

    async def _append_run_item(self, *, run_id: str, item: dict[str, Any]) -> None:
        now = datetime.now(tz=UTC)
        status = str(item.get("status", "failed"))
        source_type = str(item.get("type", "unknown"))
        source_id = str(item.get("source_id", "unknown"))
        message = _clean_text(item.get("message"))

        stats_keys = {
            "row_count",
            "added_chunks",
            "deduped_rows",
            "deduped_chunks",
            "masked_rows",
            "masked_chunks",
            "chunk_count",
            "query_count",
            "skipped_empty_chunks",
        }
        stats = {key: item[key] for key in stats_keys if key in item}

        run = self._run_store.get(run_id)
        if run is not None:
            results = run.get("results")
            if isinstance(results, list):
                results.append(item)
        if not self._db_available:
            return
        try:
            await ensure_database_ready()
            async with get_db_session() as session:
                session.add(
                    IngestionRunItem(
                        run_id=run_id,
                        source_id=source_id,
                        source_type=source_type,
                        status=status,
                        message=message,
                        stats_json=stats,
                        payload_json=item,
                        started_at=now,
                        finished_at=now,
                        created_at=now,
                    )
                )
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)

    async def _finalize_run(
        self,
        *,
        run_id: str,
        status: str,
        summary: dict[str, Any],
        error_message: str | None,
    ) -> None:
        now = datetime.now(tz=UTC)
        run = self._run_store.get(run_id)
        if run is not None:
            started_at = _extract_datetime(run.get("started_at"))
            run["status"] = status
            run["summary"] = summary
            run["error_message"] = error_message
            run["finished_at"] = now.isoformat()
            run["duration_ms"] = _duration_ms(started_at, now)
        if not self._db_available:
            return
        try:
            await ensure_database_ready()
            stmt: Select[tuple[IngestionRun]] = select(IngestionRun).where(IngestionRun.id == run_id)
            async with get_db_session() as session:
                model = (await session.execute(stmt)).scalars().first()
                if model is None:
                    return
                model.status = status
                model.summary_json = summary
                model.error_message = error_message
                model.finished_at = now
                model.updated_at = now
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)

    async def _load_checkpoint(self, source_id: str) -> CheckpointState | None:
        if not self._db_available:
            return self._checkpoints_mem.get(source_id)
        try:
            await ensure_database_ready()
            stmt: Select[tuple[IngestionCheckpoint]] = select(IngestionCheckpoint).where(
                IngestionCheckpoint.source_id == source_id
            )
            async with get_db_session() as session:
                model = (await session.execute(stmt)).scalars().first()
            if model is None:
                return self._checkpoints_mem.get(source_id)
            checkpoint = CheckpointState(
                source_id=model.source_id,
                cursor_text=model.cursor_text,
                cursor_time=model.cursor_time,
                metadata=model.metadata_json or {},
            )
            self._checkpoints_mem[source_id] = checkpoint
            return checkpoint
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)
            return self._checkpoints_mem.get(source_id)

    async def _save_checkpoint(
        self,
        *,
        source_id: str,
        cursor_text: str | None,
        cursor_time: datetime | None,
        metadata: dict[str, Any],
    ) -> None:
        now = datetime.now(tz=UTC)
        self._checkpoints_mem[source_id] = CheckpointState(
            source_id=source_id,
            cursor_text=cursor_text,
            cursor_time=cursor_time,
            metadata=metadata,
        )
        if not self._db_available:
            return
        try:
            await ensure_database_ready()
            stmt: Select[tuple[IngestionCheckpoint]] = select(IngestionCheckpoint).where(
                IngestionCheckpoint.source_id == source_id
            )
            async with get_db_session() as session:
                model = (await session.execute(stmt)).scalars().first()
                if model is None:
                    session.add(
                        IngestionCheckpoint(
                            source_id=source_id,
                            cursor_text=cursor_text,
                            cursor_time=cursor_time,
                            metadata_json=metadata,
                            created_at=now,
                            updated_at=now,
                        )
                    )
                    return
                model.cursor_text = cursor_text
                model.cursor_time = cursor_time
                model.metadata_json = metadata
                model.updated_at = now
        except Exception as exc:  # pragma: no cover - defensive
            self._mark_db_unavailable(exc)

    async def _emit_event(self, run_id: str, event_type: str, payload: dict[str, Any]) -> None:
        event = {
            "run_id": run_id,
            "event_type": event_type,
            "timestamp": datetime.now(tz=UTC).isoformat(),
            **payload,
        }
        if run_id not in self._run_events:
            self._run_events[run_id] = deque(maxlen=500)
        self._run_events[run_id].append(event)

        subscribers = self._event_subscribers.get(run_id, [])
        for queue in subscribers:
            with contextlib.suppress(asyncio.QueueFull):
                queue.put_nowait(event)

    async def _add_document_with_retry(
        self,
        *,
        rag_service: Any | None,
        content: str,
        source: str,
        metadata: dict[str, Any],
    ) -> dict[str, Any]:
        if rag_service is None:
            return {"count": 0}

        async def _execute() -> dict[str, Any]:
            result = await rag_service.execute(
                action="add_document",
                content=content,
                source=source,
                metadata=metadata,
            )
            if not result.success:
                msg = result.error_message or "failed to add document"
                raise RuntimeError(msg)
            return result.data if isinstance(result.data, dict) else {}

        return await _retry_async(_execute)

    def _serialize_run_model(
        self,
        run: IngestionRun,
        *,
        items: list[IngestionRunItem] | None = None,
    ) -> dict[str, Any]:
        duration_ms = _duration_ms(run.started_at, run.finished_at)
        payload = {
            "run_id": run.id,
            "status": run.status,
            "trigger_mode": run.trigger_mode,
            "dry_run": bool(run.dry_run),
            "source_ids": run.source_ids_json or [],
            "summary": run.summary_json or {},
            "error_message": run.error_message,
            "started_at": run.started_at.isoformat() if run.started_at else None,
            "finished_at": run.finished_at.isoformat() if run.finished_at else None,
            "duration_ms": duration_ms,
        }
        if items is not None:
            payload["results"] = [
                {
                    "id": item.id,
                    "source_id": item.source_id,
                    "type": item.source_type,
                    "status": item.status,
                    "message": item.message,
                    "stats": item.stats_json or {},
                    **(item.payload_json or {}),
                }
                for item in items
            ]
        return payload

    def _list_runs_from_memory(self, limit: int) -> list[dict[str, Any]]:
        runs = sorted(
            self._run_store.values(),
            key=lambda item: str(item.get("started_at", "")),
            reverse=True,
        )
        return runs[:limit]

    def _mark_db_unavailable(self, exc: Exception) -> None:
        if self._db_available:
            logger.warning("RAG ingestion DB persistence disabled, fallback to memory: %s", exc)
        self._db_available = False


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _coerce_bool(value: Any, *, default: bool) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        lowered = value.strip().lower()
        if lowered in {"1", "true", "yes", "on"}:
            return True
        if lowered in {"0", "false", "no", "off"}:
            return False
    return default


def _coerce_positive_int(value: Any, *, default: int, minimum: int, maximum: int) -> int:
    try:
        parsed = int(value)
    except (TypeError, ValueError):
        return default
    return max(minimum, min(maximum, parsed))


def _normalize_text(text: str) -> str:
    normalized = unicodedata.normalize("NFKC", text)
    normalized = _CONTROL_CHAR_RE.sub(" ", normalized)
    normalized = normalized.replace("\r\n", "\n").replace("\r", "\n")
    normalized = _WHITESPACE_RE.sub(" ", normalized)
    normalized = _NEWLINES_RE.sub("\n\n", normalized)
    return normalized.strip()


def _content_hash(content: str) -> str:
    return hashlib.sha256(content.encode("utf-8")).hexdigest()


def _resolve_table_names(options: dict[str, Any]) -> list[str]:
    names: list[str] = []
    table = _clean_text(options.get("table"))
    if table:
        names.append(table)
    tables_value = options.get("tables")
    if isinstance(tables_value, list):
        for item in tables_value:
            name = _clean_text(item)
            if name:
                names.append(name)
    elif isinstance(tables_value, str):
        for part in tables_value.split(","):
            name = _clean_text(part)
            if name:
                names.append(name)

    normalized: list[str] = []
    for name in names:
        if not _IDENTIFIER_RE.match(name):
            continue
        if name not in normalized:
            normalized.append(name)
    return normalized


def _build_table_select_sql(table_name: str, *, row_limit: int, dialect: str) -> str:
    if dialect in {"mssql", "sqlserver"}:
        return f"SELECT TOP {row_limit} * FROM {table_name}"
    return f"SELECT * FROM {table_name} LIMIT {row_limit}"


def _is_supported_db_url(db_url: str) -> bool:
    lowered = db_url.strip().lower()
    return lowered.startswith(("postgresql", "postgres", "mysql", "sqlite", "mssql", "sqlserver"))


def _build_db_row_content(
    *,
    source: RAGDataSourceConfig,
    sql: str,
    row: dict[str, Any],
    dialect: str,
) -> str:
    payload = {
        "source_id": source.source_id,
        "label": source.label,
        "type": source.source_type,
        "dialect": dialect,
        "sql": sql,
        "row": row,
    }
    return json.dumps(payload, ensure_ascii=False, default=str)


def _extract_datetime(value: Any) -> datetime | None:
    if value is None:
        return None
    if isinstance(value, datetime):
        return value if value.tzinfo else value.replace(tzinfo=UTC)
    text = _clean_text(value)
    if not text:
        return None
    with contextlib.suppress(ValueError):
        parsed = datetime.fromisoformat(text.replace("Z", "+00:00"))
        return parsed if parsed.tzinfo else parsed.replace(tzinfo=UTC)
    return None


def _duration_ms(started_at: datetime | None, finished_at: datetime | None) -> int:
    if started_at is None or finished_at is None:
        return 0
    return int(max((finished_at - started_at).total_seconds() * 1000, 0))


def _failed_source_result(*, source: RAGDataSourceConfig, message: str) -> dict[str, Any]:
    return {
        "source_id": source.source_id,
        "type": source.source_type,
        "status": "failed",
        "message": message,
    }


def _sqlglot_dialect(dialect: str) -> str | None:
    normalized = dialect.strip().lower()
    return _SQL_DIALECT_MAP.get(normalized, normalized)


def _guard_select_sql(sql: str, *, dialect: str) -> dict[str, Any]:
    query = sql.strip()
    if not query:
        return {"ok": False, "message": "empty SQL query", "sql": ""}

    if sqlglot is None or sql_exp is None:
        return _regex_guard_select_sql(query)

    try:
        parsed = sqlglot.parse(query, read=_sqlglot_dialect(dialect))
    except Exception as exc:
        return {"ok": False, "message": f"invalid SQL: {exc}", "sql": ""}

    if len(parsed) != 1:
        return {"ok": False, "message": "multi-statement SQL is not allowed", "sql": ""}

    stmt = parsed[0]
    forbidden_nodes: tuple[type[ClauseElement], ...] = (
        sql_exp.Delete,
        sql_exp.Update,
        sql_exp.Insert,
        sql_exp.Create,
        sql_exp.Drop,
        sql_exp.Alter,
        sql_exp.Truncate,
        sql_exp.Command,
    )
    for node in forbidden_nodes:
        if stmt.find(node) is not None:
            return {"ok": False, "message": "only SELECT/CTE queries are allowed", "sql": ""}

    has_select = isinstance(stmt, sql_exp.Select) or stmt.find(sql_exp.Select) is not None
    if not has_select:
        return {"ok": False, "message": "only SELECT/CTE queries are allowed", "sql": ""}

    normalized_sql = stmt.sql(dialect=_sqlglot_dialect(dialect) or "")
    return {"ok": True, "message": "", "sql": normalized_sql}


def _regex_guard_select_sql(sql: str) -> dict[str, Any]:
    without_comments = re.sub(r"/\*.*?\*/", " ", sql, flags=re.DOTALL)
    without_comments = re.sub(r"--.*?$", " ", without_comments, flags=re.MULTILINE)
    statement = without_comments.strip().rstrip(";").strip()
    if ";" in statement:
        return {"ok": False, "message": "multi-statement SQL is not allowed", "sql": ""}
    upper = statement.upper()
    if not upper.startswith(("SELECT", "WITH")):
        return {"ok": False, "message": "only SELECT/CTE queries are allowed", "sql": ""}
    if re.search(r"\b(INSERT|UPDATE|DELETE|DROP|ALTER|CREATE|TRUNCATE|GRANT|REVOKE|EXEC|CALL)\b", upper):
        return {"ok": False, "message": "only SELECT/CTE queries are allowed", "sql": ""}
    return {"ok": True, "message": "", "sql": statement}


def _has_limit_sql(sql: str) -> bool:
    upper = sql.upper()
    if " LIMIT " in upper:
        return True
    if " FETCH NEXT " in upper:
        return True
    return bool(re.search(r"\bTOP\s+\d+\b", upper))


def _apply_limit(sql: str, *, row_limit: int, dialect: str) -> str:
    normalized = sql.strip().rstrip(";")
    if _has_limit_sql(normalized):
        return normalized
    if dialect in {"mssql", "sqlserver"}:
        return f"SELECT TOP {row_limit} * FROM ({normalized}) AS afq_sub"
    return f"SELECT * FROM ({normalized}) AS afq_sub LIMIT {row_limit}"


def _apply_incremental_filters(
    *,
    sql: str,
    dialect: str,
    row_limit: int,
    time_column: str | None,
    cursor_column: str | None,
    checkpoint: CheckpointState | None,
) -> str:
    limited_sql = _apply_limit(sql, row_limit=row_limit, dialect=dialect)
    filters: list[str] = []

    if checkpoint is not None and time_column and checkpoint.cursor_time is not None and _IDENTIFIER_RE.match(time_column):
        timestamp_text = checkpoint.cursor_time.astimezone(UTC).isoformat()
        filters.append(f"afq_sub.{time_column} > '{timestamp_text}'")

    if checkpoint is not None and cursor_column and checkpoint.cursor_text and _IDENTIFIER_RE.match(cursor_column):
        escaped = checkpoint.cursor_text.replace("'", "''")
        filters.append(f"afq_sub.{cursor_column} > '{escaped}'")

    if not filters:
        return limited_sql

    if dialect in {"mssql", "sqlserver"}:
        return f"SELECT TOP {row_limit} * FROM ({sql}) AS afq_sub WHERE {' AND '.join(filters)}"
    return f"SELECT * FROM ({sql}) AS afq_sub WHERE {' AND '.join(filters)} LIMIT {row_limit}"


async def _retry_async(func: Callable[..., Awaitable[Any]], *args: Any, **kwargs: Any) -> Any:
    if (
        AsyncRetrying is None
        or stop_after_attempt is None
        or wait_exponential is None
        or retry_if_exception_type is None
    ):
        return await func(*args, **kwargs)

    async for attempt in AsyncRetrying(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=0.2, min=0.2, max=3),
        retry=retry_if_exception_type(Exception),
        reraise=True,
    ):
        with attempt:
            return await func(*args, **kwargs)
    return await func(*args, **kwargs)


def _is_select_only_sql(sql: str) -> bool:
    """Backward-compatible helper for tests."""
    guard = _guard_select_sql(sql, dialect="postgresql")
    return bool(guard["ok"])
