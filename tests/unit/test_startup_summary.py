"""Startup summary の単体テスト."""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING

from infrastructure.observability import startup


if TYPE_CHECKING:
    from pathlib import Path


def _write_app_config(tmp_path: Path, *, llm_contracts: dict, runtime_database: dict | None = None) -> Path:
    payload = {
        "name": "demo_app",
        "display_name": "Demo App",
        "version": "1.0.0",
        "contracts": {
            "llm": llm_contracts,
        },
        "runtime": {
            "database": runtime_database or {},
        },
    }
    app_config_path = tmp_path / "app_config.json"
    app_config_path.write_text(json.dumps(payload), encoding="utf-8")
    return app_config_path


def _global_summary() -> dict:
    return {
        "llm": {
            "provider": "global-provider",
            "model": "global-model",
            "base_url": "https://global.example/v1",
            "supported_models": [],
        },
        "db": {
            "backend": "memory",
            "url": None,
        },
        "vectordb": {
            "backend": "memory",
            "path": None,
            "collection": None,
            "index": None,
        },
        "embedding": {
            "model": "global-embedding-model",
        },
    }


def test_collect_startup_summary_uses_global_defaults(monkeypatch) -> None:
    monkeypatch.setattr(startup, "_collect_global_summary", lambda: _global_summary())

    summary = startup.collect_startup_summary()

    assert summary["llm"]["provider"] == "global-provider"
    assert summary["llm"]["model"] == "global-model"
    assert summary["llm"]["supported_models"] == []
    assert summary["db"]["backend"] == "memory"
    assert summary["embedding"]["model"] == "global-embedding-model"


def test_collect_startup_summary_resolves_app_scoped_supported_models(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setattr(startup, "_collect_global_summary", lambda: _global_summary())
    monkeypatch.setattr(
        startup,
        "_build_model_lookup",
        lambda: {
            "platform_text_default": {
                "alias": "platform_text_default",
                "model_id": "platform_text_default",
                "provider": "openai",
                "model": "gpt-4.1",
                "model_type": "text",
                "api_base": "https://api.openai.com/v1",
            },
            "platform_embedding_default": {
                "alias": "platform_embedding_default",
                "model_id": "platform_embedding_default",
                "provider": "openai",
                "model": "text-embedding-3-small",
                "model_type": "embedding",
                "api_base": "https://api.openai.com/v1",
            },
            "coding_openai": {
                "alias": "coding_openai",
                "model_id": "coding_openai",
                "provider": "openai",
                "model": "gpt-4.1-mini",
                "model_type": "text",
                "api_base": "https://api.openai.com/v1",
            },
        },
    )
    app_config_path = _write_app_config(
        tmp_path,
        llm_contracts={
            "enabled": True,
            "defaults": {
                "text": {
                    "provider": "openai",
                    "model_id": "platform_text_default",
                    "model_type": "text",
                },
                "embedding": {
                    "provider": "openai",
                    "model_id": "platform_embedding_default",
                    "model_type": "embedding",
                },
            },
            "agent_overrides": {
                "CodeAgent": {
                    "text": {
                        "provider": "openai",
                        "model_id": "coding_openai",
                        "model_type": "text",
                    }
                }
            },
            "allowed_modalities": ["text", "embedding"],
            "extra_model_refs": [
                {
                    "provider": "openai",
                    "model_id": "coding_openai",
                    "model_type": "text",
                }
            ],
        },
        runtime_database={
            "kind": "sqlite",
            "url": "sqlite:///./demo.db",
        },
    )

    summary = startup.collect_startup_summary(app_config_path=app_config_path)

    assert summary["llm"]["provider"] == "openai"
    assert summary["llm"]["model"] == "gpt-4.1"
    assert summary["embedding"]["model"] == "text-embedding-3-small"
    assert summary["db"]["backend"] == "sqlite"
    assert len(summary["llm"]["supported_models"]) == 3
    assert [item["model_id"] for item in summary["llm"]["supported_models"]] == [
        "coding_openai",
        "platform_text_default",
        "platform_embedding_default",
    ]


def test_collect_startup_summary_marks_unresolved_models(monkeypatch, tmp_path: Path, caplog) -> None:
    monkeypatch.setattr(startup, "_collect_global_summary", lambda: _global_summary())
    monkeypatch.setattr(
        startup,
        "_build_model_lookup",
        lambda: {
            "platform_text_default": {
                "alias": "platform_text_default",
                "model_id": "platform_text_default",
                "provider": "openai",
                "model": "gpt-4.1",
                "model_type": "text",
                "api_base": "https://api.openai.com/v1",
            }
        },
    )
    app_config_path = _write_app_config(
        tmp_path,
        llm_contracts={
            "enabled": True,
            "defaults": {
                "text": {
                    "provider": "openai",
                    "model_id": "platform_text_default",
                    "model_type": "text",
                }
            },
            "agent_overrides": {},
            "allowed_modalities": ["text"],
            "extra_model_refs": [
                {
                    "provider": "openai",
                    "model_id": "missing_model",
                    "model_type": "text",
                }
            ],
        },
    )

    with caplog.at_level(logging.WARNING, logger="bizcore.startup"):
        summary = startup.collect_startup_summary(app_config_path=app_config_path)

    assert "Unresolved app model ref" in caplog.text
    unresolved = [item for item in summary["llm"]["supported_models"] if not item["resolved"]]
    assert len(unresolved) == 1
    assert unresolved[0]["model_id"] == "missing_model"


def test_log_startup_info_masks_urls_and_prints_fixed_sections(monkeypatch, caplog) -> None:
    monkeypatch.setattr(
        startup,
        "collect_startup_summary",
        lambda **_: {
            "llm": {
                "provider": "openai",
                "model": "gpt-4.1",
                "base_url": "https://user:secret@example.com/v1",
                "supported_models": [
                    {
                        "model_type": "text",
                        "model_id": "platform_text_default",
                        "provider": "openai",
                        "model": "gpt-4.1",
                        "resolved": True,
                    },
                    {
                        "model_type": "embedding",
                        "model_id": "missing_embedding",
                        "provider": None,
                        "model": None,
                        "resolved": False,
                    },
                ],
            },
            "db": {
                "backend": "postgresql",
                "url": "postgresql+asyncpg://demo:secret@localhost:5432/demo_db",
            },
            "vectordb": {
                "backend": "",
                "path": "",
                "collection": "",
                "index": "",
            },
            "embedding": {
                "model": None,
            },
        },
    )

    with caplog.at_level(logging.INFO, logger="bizcore.startup"):
        startup.log_startup_info(app_name="Demo App")

    assert "[LLM] Active Provider: openai" in caplog.text
    assert "[LLM] Active Model: gpt-4.1" in caplog.text
    assert "[LLM] Base URL: ***@example.com/v1" in caplog.text
    assert "[LLM] Supported Models (App):" in caplog.text
    assert "text: platform_text_default -> openai / gpt-4.1" in caplog.text
    assert "embedding: missing_embedding -> unresolved" in caplog.text
    assert "[DB] URL: ***@localhost:5432/demo_db" in caplog.text
    assert "[VectorDB] Backend: Not configured" in caplog.text
    assert "[VectorDB] Collection: Not configured" in caplog.text
    assert "[Embedding] Model: Not configured" in caplog.text
