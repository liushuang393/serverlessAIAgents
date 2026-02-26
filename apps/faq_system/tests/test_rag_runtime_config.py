"""RAG runtime config parser tests."""

from __future__ import annotations

import json
from pathlib import Path

from apps.faq_system.backend.services.rag_runtime_config import load_rag_runtime_config


def _write_config(path: Path, payload: dict) -> None:
    path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")


def test_runtime_config_full_settings(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    _write_config(
        config_path,
        {
            "name": "faq_system",
            "contracts": {
                "rag": {
                    "enabled": True,
                    "collections": ["faq_docs"],
                    "chunk_strategy": "sentence",
                    "rerank_model": "cohere",
                    "default_top_k": 8,
                    "data_sources": [
                        {
                            "id": "db-main",
                            "type": "database",
                            "uri": "postgresql+asyncpg://u:p@localhost:5432/faq",
                            "label": "main db",
                            "enabled": True,
                            "options": {
                                "database_type": "postgresql",
                                "dialect": "postgresql",
                                "read_mode": "query",
                                "query": "SELECT 1",
                            },
                        }
                    ],
                }
            },
            "services": {
                "sql": {
                    "dialect": "postgresql",
                    "schema": {"sales": ["id", "amount"]},
                }
            },
            "runtime": {
                "database": {
                    "kind": "postgresql",
                    "url": "postgresql+asyncpg://u:p@localhost:5432/faq",
                }
            },
        },
    )

    runtime = load_rag_runtime_config(config_path)
    assert runtime.rag_enabled is True
    assert runtime.sql_enabled is True
    assert runtime.hybrid_enabled is True
    assert runtime.rag_collection == "faq_docs"
    assert runtime.sql_schema == {"sales": ["id", "amount"]}
    assert runtime.database_url == "postgresql+asyncpg://u:p@localhost:5432/faq"
    assert len(runtime.data_sources) == 1
    assert runtime.data_sources[0].source_id == "db-main"


def test_runtime_config_without_rag_sql_disables_features(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    _write_config(config_path, {"name": "faq_system"})

    runtime = load_rag_runtime_config(config_path)
    assert runtime.rag_enabled is False
    assert runtime.sql_enabled is False
    assert runtime.hybrid_enabled is False
    assert runtime.data_sources == []


def test_runtime_config_partial_sql_without_db_url_stays_disabled(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    _write_config(
        config_path,
        {
            "name": "faq_system",
            "services": {"sql": {"dialect": "postgresql"}},
        },
    )

    runtime = load_rag_runtime_config(config_path)
    assert runtime.rag_enabled is False
    assert runtime.sql_enabled is False
    assert runtime.hybrid_enabled is False


def test_runtime_config_invalid_json_returns_safe_defaults(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    config_path.write_text("{ invalid json", encoding="utf-8")

    runtime = load_rag_runtime_config(config_path)
    assert runtime.rag_enabled is False
    assert runtime.sql_enabled is False
    assert runtime.app_name == "faq_system"
