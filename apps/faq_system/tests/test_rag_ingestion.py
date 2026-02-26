"""RAG ingestion orchestrator tests."""

from __future__ import annotations

from pathlib import Path

import pytest

from apps.faq_system.backend.services.rag_ingestion import (
    RAGIngestionOrchestrator,
    _is_select_only_sql,
)
from apps.faq_system.backend.services.rag_runtime_config import (
    RAGDataSourceConfig,
    RAGRuntimeConfig,
)


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
        rag_service_getter=lambda _: None,
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
    orchestrator = RAGIngestionOrchestrator(
        config_loader=lambda: runtime,
        rag_service_getter=lambda _: None,
    )

    run = await orchestrator.ingest(dry_run=True)
    assert run["status"] == "success"
    assert run["summary"]["success_sources"] == 1
    result = run["results"][0]
    assert result["status"] == "success"
    assert result["planned_action"] == "query"
    assert result["query_count"] == 1


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
        rag_service_getter=lambda _: None,
    )

    run = await orchestrator.ingest(dry_run=False)
    assert run["status"] == "failed"
    assert run["summary"]["failed_sources"] == 1
    assert "only SELECT" in run["results"][0]["message"]
