"""RAG ingestion orchestrator tests."""

from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any

import pytest
from apps.faq_system.backend.services import rag_ingestion
from apps.faq_system.backend.services.rag_ingestion import (
    RAGIngestionOrchestrator,
    _is_select_only_sql,
)
from apps.faq_system.backend.services.rag_runtime_config import (
    RAGDataSourceConfig,
    RAGRuntimeConfig,
)


class _FakeServiceResult:
    def __init__(self, *, count: int = 1, success: bool = True, error_message: str | None = None) -> None:
        self.success = success
        self.error_message = error_message
        self.data: dict[str, Any] = {"count": count}


class _FakeRAGService:
    def __init__(self) -> None:
        self.calls: list[dict[str, Any]] = []

    async def execute(self, **kwargs: Any) -> _FakeServiceResult:
        self.calls.append(kwargs)
        return _FakeServiceResult(count=1)


def _runtime_config_with_sources(sources: list[RAGDataSourceConfig]) -> RAGRuntimeConfig:
    return RAGRuntimeConfig(
        app_name="faq_system",
        config_path=Path(__file__).resolve(),
        rag_enabled=True,
        sql_enabled=True,
        rag_collection="faq_docs",
        rag_chunk_strategy="recursive",
        rag_reranker="bm25",
        rag_top_k=5,
        indexing_schedule="0 */6 * * *",
        data_sources=sources,
        sql_schema={"sales": ["id", "amount"]},
        sql_dialect="postgresql",
        database_url="postgresql+asyncpg://u:p@localhost:5432/faq",
        database_kind="postgresql",
    )


def test_select_only_sql_guard() -> None:
    assert _is_select_only_sql("SELECT * FROM sales LIMIT 10") is True
    assert _is_select_only_sql("WITH t AS (SELECT 1) SELECT * FROM t") is True
    assert _is_select_only_sql("DELETE FROM sales") is False
    assert _is_select_only_sql("SELECT * FROM sales; DROP TABLE sales;") is False


def test_schedule_priority_source_over_global() -> None:
    source = RAGDataSourceConfig(
        source_id="db-main",
        source_type="database",
        uri="postgresql+asyncpg://u:p@localhost:5432/faq",
        label="db",
        enabled=True,
        schedule="*/15 * * * *",
        options={
            "database_type": "postgresql",
            "dialect": "postgresql",
            "read_mode": "query",
            "query": "SELECT 1",
        },
    )
    runtime = _runtime_config_with_sources([source])
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: _FakeRAGService(),
    )

    entries = orchestrator.resolve_schedule_entries()
    assert len(entries) == 1
    assert entries[0].source_id == "db-main"
    assert entries[0].schedule == "*/15 * * * *"


@pytest.mark.asyncio
async def test_database_ingest_dry_run_returns_planned_queries() -> None:
    source = RAGDataSourceConfig(
        source_id="db-main",
        source_type="database",
        uri="postgresql+asyncpg://u:p@localhost:5432/faq",
        label="db",
        enabled=True,
        schedule=None,
        options={
            "database_type": "postgresql",
            "dialect": "postgresql",
            "read_mode": "query",
            "query": "SELECT id, amount FROM sales",
            "row_limit": 100,
        },
    )
    runtime = _runtime_config_with_sources([source])
    fake = _FakeRAGService()
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: fake,
    )

    run = await orchestrator.ingest(dry_run=True)
    assert run["status"] == "success"
    assert run["summary"]["success_sources"] == 1
    result = run["results"][0]
    assert result["status"] == "success"
    assert result["planned_action"] == "query"
    assert result["query_count"] == 1
    assert fake.calls == []


@pytest.mark.asyncio
async def test_database_ingest_rejects_non_select_query() -> None:
    source = RAGDataSourceConfig(
        source_id="db-main",
        source_type="database",
        uri="postgresql+asyncpg://u:p@localhost:5432/faq",
        label="db",
        enabled=True,
        schedule=None,
        options={
            "database_type": "postgresql",
            "dialect": "postgresql",
            "read_mode": "query",
            "query": "DELETE FROM sales",
        },
    )
    runtime = _runtime_config_with_sources([source])
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: _FakeRAGService(),
    )

    run = await orchestrator.ingest(dry_run=False)
    assert run["status"] == "failed"
    assert run["summary"]["failed_sources"] == 1
    assert "only SELECT" in run["results"][0]["message"]


@pytest.mark.asyncio
async def test_file_ingest_masks_pii_and_dedupes(tmp_path: Path) -> None:
    docs_dir = tmp_path / "docs"
    docs_dir.mkdir()
    content = "Contact me at john.doe@example.com\n" * 3
    (docs_dir / "a.md").write_text(content, encoding="utf-8")
    (docs_dir / "b.md").write_text(content, encoding="utf-8")

    source = RAGDataSourceConfig(
        source_id="file-main",
        source_type="file",
        uri=str(docs_dir),
        label="docs",
        enabled=True,
        schedule=None,
        options={"glob": "*.md"},
    )
    runtime = _runtime_config_with_sources([source])
    fake = _FakeRAGService()
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: fake,
    )

    run = await orchestrator.ingest(dry_run=False)
    result = run["results"][0]
    assert run["status"] == "success"
    assert result["status"] == "success"
    assert result["masked_chunks"] >= 1
    assert result["deduped_chunks"] >= 1
    assert len(fake.calls) >= 1
    first_content = str(fake.calls[0].get("content", ""))
    assert "example.com" in first_content
    assert "john.doe@example.com" not in first_content


@pytest.mark.asyncio
async def test_database_incremental_checkpoint(monkeypatch: pytest.MonkeyPatch) -> None:
    rows: list[dict[str, Any]] = [
        {"id": 1, "question": "q1", "answer": "a1"},
        {"id": 2, "question": "q2", "answer": "a2"},
    ]
    executed_sqls: list[str] = []

    class _FakeProvider:
        def __init__(self, _db_url: str) -> None:
            pass

        async def connect(self) -> None:
            return

        async def disconnect(self) -> None:
            return

        async def execute_raw(self, sql: str) -> list[dict[str, Any]]:
            executed_sqls.append(sql)
            if "afq_sub.id > '2'" in sql:
                return [row for row in rows if int(row["id"]) > 2]
            return list(rows)

    monkeypatch.setattr(rag_ingestion, "SQLAlchemyDBProvider", _FakeProvider)

    source = RAGDataSourceConfig(
        source_id="db-main",
        source_type="database",
        uri="postgresql+asyncpg://u:p@localhost:5432/faq",
        label="db",
        enabled=True,
        schedule=None,
        options={
            "database_type": "postgresql",
            "dialect": "postgresql",
            "read_mode": "table",
            "table": "faq_entries",
            "row_limit": 100,
            "cursor_column": "id",
        },
    )
    runtime = _runtime_config_with_sources([source])
    fake = _FakeRAGService()
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: fake,
    )

    first = await orchestrator.ingest(dry_run=False)
    assert first["status"] == "success"
    assert first["results"][0]["row_count"] == 2

    checkpoint_after_first = await orchestrator._load_checkpoint("db-main")
    assert checkpoint_after_first is not None
    assert checkpoint_after_first.cursor_text == "2"

    rows.append({"id": 3, "question": "q3", "answer": "a3"})
    second = await orchestrator.ingest(dry_run=False)
    assert second["status"] == "success"
    assert second["results"][0]["row_count"] == 1

    checkpoint_after_second = await orchestrator._load_checkpoint("db-main")
    assert checkpoint_after_second is not None
    assert checkpoint_after_second.cursor_text == "3"
    assert any("afq_sub.id > '2'" in sql for sql in executed_sqls)


@pytest.mark.asyncio
async def test_staged_sources_return_not_implemented() -> None:
    source = RAGDataSourceConfig(
        source_id="web-main",
        source_type="web",
        uri="https://example.com/docs",
        label="docs",
        enabled=True,
        schedule=None,
        options={},
    )
    runtime = _runtime_config_with_sources([source])
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: _FakeRAGService(),
    )

    run = await orchestrator.ingest(dry_run=False)
    assert run["status"] == "success"
    result = run["results"][0]
    assert result["status"] == "skipped"
    assert result["reason"] == "not_implemented"


@pytest.mark.asyncio
async def test_async_ingest_status_transition() -> None:
    source = RAGDataSourceConfig(
        source_id="web-main",
        source_type="web",
        uri="https://example.com/docs",
        label="docs",
        enabled=True,
        schedule=None,
        options={},
    )
    runtime = _runtime_config_with_sources([source])
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: _FakeRAGService(),
    )

    queued = await orchestrator.enqueue_ingest(dry_run=True)
    run_id = str(queued["run_id"])

    final_run: dict[str, Any] | None = None
    for _ in range(30):
        await asyncio.sleep(0.05)
        final_run = await orchestrator.get_run(run_id)
        if final_run and final_run.get("status") in {"success", "failed", "partial_success", "skipped"}:
            break

    assert final_run is not None
    assert final_run["status"] in {"success", "failed", "partial_success", "skipped"}

    events = await orchestrator.list_run_events(run_id)
    event_types = {event["event_type"] for event in events}
    assert "queued" in event_types
    assert "running" in event_types
    assert "completed" in event_types or "failed" in event_types
