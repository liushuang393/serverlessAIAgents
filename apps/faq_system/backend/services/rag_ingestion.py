"""RAG ingestion orchestrator for FAQ app."""

from __future__ import annotations

import json
import logging
import re
import time
import uuid
from collections import deque
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Callable

from agentflow.providers.db_provider import SQLAlchemyDBProvider

from apps.faq_system.backend.services.rag_runtime_config import (
    RAGDataSourceConfig,
    RAGRuntimeConfig,
)


logger = logging.getLogger(__name__)

_IDENTIFIER_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
_FORBIDDEN_SQL_TOKENS = (
    "INSERT",
    "UPDATE",
    "DELETE",
    "DROP",
    "ALTER",
    "CREATE",
    "TRUNCATE",
    "GRANT",
    "REVOKE",
    "EXEC",
    "CALL",
)


@dataclass(slots=True)
class IngestionScheduleEntry:
    """Schedule definition for one source."""

    source_id: str
    schedule: str
    label: str


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

    def list_runs(self, limit: int = 20) -> list[dict[str, Any]]:
        """Return recent ingestion runs (latest first)."""
        normalized_limit = max(1, min(limit, len(self._runs)))
        return list(self._runs)[:normalized_limit]

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

    async def ingest(
        self,
        *,
        source_ids: list[str] | None = None,
        dry_run: bool = False,
    ) -> dict[str, Any]:
        """Execute ingestion run."""
        config = self._config_loader()
        started_at = datetime.now().isoformat()
        run_id = f"ingest-{uuid.uuid4().hex[:12]}"
        started_perf = time.perf_counter()

        if not config.rag_enabled:
            run = {
                "run_id": run_id,
                "status": "skipped",
                "reason": "rag_disabled",
                "dry_run": dry_run,
                "started_at": started_at,
                "finished_at": datetime.now().isoformat(),
                "duration_ms": 0,
                "results": [],
            }
            self._runs.appendleft(run)
            return run

        requested = {item.strip() for item in source_ids or [] if item and item.strip()}
        candidates = [source for source in config.data_sources if source.enabled]
        if requested:
            candidates = [source for source in candidates if source.source_id in requested]

        if not candidates:
            run = {
                "run_id": run_id,
                "status": "skipped",
                "reason": "no_sources",
                "dry_run": dry_run,
                "started_at": started_at,
                "finished_at": datetime.now().isoformat(),
                "duration_ms": 0,
                "results": [],
            }
            self._runs.appendleft(run)
            return run

        results: list[dict[str, Any]] = []
        success_count = 0
        failed_count = 0
        skipped_count = 0

        for source in candidates:
            item = await self._ingest_source(
                config=config,
                source=source,
                dry_run=dry_run,
            )
            results.append(item)
            status = item.get("status")
            if status == "success":
                success_count += 1
            elif status == "skipped":
                skipped_count += 1
            else:
                failed_count += 1

        duration_ms = int((time.perf_counter() - started_perf) * 1000)
        status = "success" if failed_count == 0 else ("partial_success" if success_count > 0 else "failed")
        run = {
            "run_id": run_id,
            "status": status,
            "dry_run": dry_run,
            "started_at": started_at,
            "finished_at": datetime.now().isoformat(),
            "duration_ms": duration_ms,
            "summary": {
                "total_sources": len(candidates),
                "success_sources": success_count,
                "failed_sources": failed_count,
                "skipped_sources": skipped_count,
            },
            "results": results,
        }
        self._runs.appendleft(run)
        return run

    async def _ingest_source(
        self,
        *,
        config: RAGRuntimeConfig,
        source: RAGDataSourceConfig,
        dry_run: bool,
    ) -> dict[str, Any]:
        source_type = source.source_type.lower()
        if source_type == "database":
            return await self._ingest_database_source(config=config, source=source, dry_run=dry_run)
        if source_type == "file":
            return await self._ingest_file_source(config=config, source=source, dry_run=dry_run)
        if source_type in {"web", "api", "s3"}:
            return await self._ingest_uri_reference(config=config, source=source, dry_run=dry_run)
        return {
            "source_id": source.source_id,
            "type": source.source_type,
            "status": "skipped",
            "message": f"unsupported source type: {source.source_type}",
        }

    async def _ingest_file_source(
        self,
        *,
        config: RAGRuntimeConfig,
        source: RAGDataSourceConfig,
        dry_run: bool,
    ) -> dict[str, Any]:
        raw_path = _clean_text(source.uri)
        if not raw_path:
            return {
                "source_id": source.source_id,
                "type": "file",
                "status": "failed",
                "message": "file source requires uri/path",
            }

        path = Path(raw_path)
        if not path.exists():
            return {
                "source_id": source.source_id,
                "type": "file",
                "status": "failed",
                "message": f"path not found: {raw_path}",
            }

        if dry_run:
            return {
                "source_id": source.source_id,
                "type": "file",
                "status": "success",
                "planned_action": "add_file",
                "file_path": str(path),
            }

        rag_service = self._rag_service_getter(config.rag_collection)
        result = await rag_service.execute(action="add_file", file_path=str(path))
        if not result.success:
            return {
                "source_id": source.source_id,
                "type": "file",
                "status": "failed",
                "message": result.error_message or "failed to add file",
            }
        return {
            "source_id": source.source_id,
            "type": "file",
            "status": "success",
            "count": int(result.data.get("count", 0)),
            "file": str(path),
        }

    async def _ingest_uri_reference(
        self,
        *,
        config: RAGRuntimeConfig,
        source: RAGDataSourceConfig,
        dry_run: bool,
    ) -> dict[str, Any]:
        uri = _clean_text(source.uri)
        if not uri:
            return {
                "source_id": source.source_id,
                "type": source.source_type,
                "status": "failed",
                "message": "uri is required",
            }

        if dry_run:
            return {
                "source_id": source.source_id,
                "type": source.source_type,
                "status": "success",
                "planned_action": "add_document",
                "uri": uri,
            }

        rag_service = self._rag_service_getter(config.rag_collection)
        content = (
            f"External data source reference\n"
            f"type: {source.source_type}\n"
            f"label: {source.label}\n"
            f"uri: {uri}\n"
        )
        result = await rag_service.execute(
            action="add_document",
            content=content,
            source=uri,
            metadata={
                "source_type": source.source_type,
                "source_id": source.source_id,
                "uri": uri,
            },
        )
        if not result.success:
            return {
                "source_id": source.source_id,
                "type": source.source_type,
                "status": "failed",
                "message": result.error_message or "failed to add uri reference",
            }
        return {
            "source_id": source.source_id,
            "type": source.source_type,
            "status": "success",
            "count": int(result.data.get("count", 0)),
        }

    async def _ingest_database_source(
        self,
        *,
        config: RAGRuntimeConfig,
        source: RAGDataSourceConfig,
        dry_run: bool,
    ) -> dict[str, Any]:
        options = source.options
        dialect = (
            _clean_text(options.get("dialect"))
            or _clean_text(options.get("database_type"))
            or _clean_text(config.database_kind)
            or "postgresql"
        ).lower()
        row_limit = _coerce_positive_int(options.get("row_limit"), default=200, minimum=1, maximum=5000)
        db_url = _clean_text(source.uri) or _clean_text(config.database_url)
        if not db_url:
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "failed",
                "message": "database uri is missing (source.uri/runtime.database.url)",
            }

        read_mode = (_clean_text(options.get("read_mode")) or "table").lower()
        if read_mode not in {"table", "query"}:
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "failed",
                "message": f"unsupported read_mode: {read_mode}",
            }

        queries: list[str] = []
        if read_mode == "query":
            raw_query = _clean_text(options.get("query"))
            if not raw_query:
                return {
                    "source_id": source.source_id,
                    "type": "database",
                    "status": "failed",
                    "message": "read_mode=query requires options.query",
                }
            if not _is_select_only_sql(raw_query):
                return {
                    "source_id": source.source_id,
                    "type": "database",
                    "status": "failed",
                    "message": "only SELECT/CTE queries are allowed",
                }
            queries.append(_apply_row_limit(raw_query, row_limit=row_limit, dialect=dialect))
        else:
            table_names = _resolve_table_names(options)
            if not table_names:
                return {
                    "source_id": source.source_id,
                    "type": "database",
                    "status": "failed",
                    "message": "read_mode=table requires options.table or options.tables",
                }
            for table_name in table_names:
                queries.append(_build_table_select_sql(table_name, row_limit=row_limit, dialect=dialect))

        if dry_run:
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "success",
                "planned_action": "query",
                "dialect": dialect,
                "read_mode": read_mode,
                "query_count": len(queries),
                "queries": queries,
            }

        if not _is_supported_db_url(db_url):
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "failed",
                "message": f"unsupported db url scheme: {db_url.split('://', 1)[0]}",
            }

        provider = SQLAlchemyDBProvider(db_url)
        try:
            await provider.connect()
            rag_service = self._rag_service_getter(config.rag_collection)

            total_rows = 0
            total_chunks = 0
            executed_queries: list[dict[str, Any]] = []
            for sql in queries:
                rows = await provider.execute_raw(sql)
                rows = rows[:row_limit]
                total_rows += len(rows)
                executed_queries.append({"sql": sql, "rows": len(rows)})

                content = _build_db_document_content(
                    source=source,
                    sql=sql,
                    rows=rows,
                    dialect=dialect,
                )
                add_result = await rag_service.execute(
                    action="add_document",
                    content=content,
                    source=source.label or source.uri or "database",
                    metadata={
                        "source_type": "database",
                        "source_id": source.source_id,
                        "dialect": dialect,
                        "read_mode": read_mode,
                        "row_count": len(rows),
                        "sql": sql,
                        "time_column": _clean_text(options.get("time_column")),
                    },
                )
                if not add_result.success:
                    return {
                        "source_id": source.source_id,
                        "type": "database",
                        "status": "failed",
                        "message": add_result.error_message or "failed to persist database rows into RAG",
                        "sql": sql,
                    }
                total_chunks += int(add_result.data.get("count", 0))

            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "success",
                "dialect": dialect,
                "query_count": len(queries),
                "row_count": total_rows,
                "chunk_count": total_chunks,
                "queries": executed_queries,
            }
        except Exception as exc:  # pragma: no cover - defensive
            logger.exception("Database ingestion failed: %s", exc)
            return {
                "source_id": source.source_id,
                "type": "database",
                "status": "failed",
                "message": str(exc),
            }
        finally:
            try:
                await provider.disconnect()
            except Exception:  # pragma: no cover - defensive
                logger.debug("database provider disconnect failed", exc_info=True)


def _clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _coerce_positive_int(value: Any, *, default: int, minimum: int, maximum: int) -> int:
    try:
        parsed = int(value)
    except (TypeError, ValueError):
        return default
    return max(minimum, min(maximum, parsed))


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
    if dialect == "mssql":
        return f"SELECT TOP {row_limit} * FROM {table_name}"
    return f"SELECT * FROM {table_name} LIMIT {row_limit}"


def _is_select_only_sql(sql: str) -> bool:
    stripped = sql.strip()
    if not stripped:
        return False

    without_comments = re.sub(r"/\*.*?\*/", " ", stripped, flags=re.DOTALL)
    without_comments = re.sub(r"--.*?$", " ", without_comments, flags=re.MULTILINE)
    normalized = without_comments.strip()
    if not normalized:
        return False

    statement = normalized.rstrip(";").strip()
    if ";" in statement:
        return False

    upper = statement.upper()
    if not (upper.startswith("SELECT") or upper.startswith("WITH")):
        return False
    if upper.startswith("WITH") and "SELECT" not in upper:
        return False

    for token in _FORBIDDEN_SQL_TOKENS:
        if re.search(rf"\b{token}\b", upper):
            return False
    return True


def _apply_row_limit(sql: str, *, row_limit: int, dialect: str) -> str:
    normalized = sql.strip().rstrip(";")
    upper = normalized.upper()
    if " LIMIT " in upper or " FETCH NEXT " in upper or re.search(r"\bTOP\s+\d+\b", upper):
        return normalized
    if dialect == "mssql":
        return f"SELECT TOP {row_limit} * FROM ({normalized}) AS afq_sub"
    return f"SELECT * FROM ({normalized}) AS afq_sub LIMIT {row_limit}"


def _is_supported_db_url(db_url: str) -> bool:
    lowered = db_url.strip().lower()
    return lowered.startswith(
        (
            "postgresql",
            "postgres",
            "mysql",
            "sqlite",
            "mssql",
            "sqlserver",
        )
    )


def _build_db_document_content(
    *,
    source: RAGDataSourceConfig,
    sql: str,
    rows: list[dict[str, Any]],
    dialect: str,
) -> str:
    payload = {
        "source_id": source.source_id,
        "label": source.label,
        "type": source.source_type,
        "dialect": dialect,
        "sql": sql,
        "row_count": len(rows),
        "rows": rows,
    }
    return json.dumps(payload, ensure_ascii=False, default=str)

