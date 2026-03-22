"""Tests for LLM gateway config and secret resolution."""

from __future__ import annotations

from typing import TYPE_CHECKING

import yaml

from infrastructure.llm.gateway.config import build_provider_runtime_statuses, load_gateway_config, resolve_secret


if TYPE_CHECKING:
    from pathlib import Path


def test_resolve_secret_prefers_env_over_dotenv(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)
    (tmp_path / ".env").write_text("OPENAI_API_KEY=dotenv-key\n", encoding="utf-8")
    monkeypatch.setenv("OPENAI_API_KEY", "env-key")

    value, source = resolve_secret("OPENAI_API_KEY", config_path=config_path)
    assert value == "env-key"
    assert source == "ENV"


def test_resolve_secret_falls_back_to_dotenv(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)
    (tmp_path / ".env").write_text("GEMINI_API_KEY=dotenv-gemini\n", encoding="utf-8")
    monkeypatch.delenv("GEMINI_API_KEY", raising=False)

    value, source = resolve_secret("GEMINI_API_KEY", config_path=config_path)
    assert value == "dotenv-gemini"
    assert source == ".env"


def test_load_gateway_config_applies_env_overrides(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)

    monkeypatch.setenv("LLM_GATEWAY_PRIORITY", "cost")
    monkeypatch.setenv("LLM_GATEWAY_LOAD_BALANCE", "least_latency")
    monkeypatch.setenv("LLM_ROLE_REASONING_ALIAS", "coding_openai")

    config = load_gateway_config(config_path)
    assert config.routing_policy.priority == "cost"
    assert config.routing_policy.load_balance_strategy == "least_latency"
    assert config.registry["reasoning"] == "coding_openai"


def test_provider_runtime_status_unavailable_when_key_missing(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    config = load_gateway_config(config_path)
    monkeypatch.delenv("OPENAI_API_KEY", raising=False)

    statuses = build_provider_runtime_statuses(config, config_path=config_path)
    by_name = {status.name: status for status in statuses}

    assert by_name["openai"].status == "unavailable"
    assert by_name["openai"].last_error == "api_key_not_configured"
    assert by_name["local"].status == "unavailable"
    assert by_name["local"].last_error == "linked_engine_disabled:vllm"


def test_load_gateway_config_migrates_legacy_yaml_defaults(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    config_path.parent.mkdir(parents=True, exist_ok=True)
    legacy_payload = {
        "gateway": {
            "default_role": "reasoning",
            "request_timeout_seconds": 120,
            "max_retries": 2,
        },
        "providers": [
            {
                "name": "openai",
                "api_base": "https://api.openai.com/v1",
                "api_key_env": "OPENAI_API_KEY",
                "models": [],
                "enabled": True,
            }
        ],
        "inference_engines": [
            {
                "name": "vllm",
                "engine_type": "vllm",
                "base_url": "http://127.0.0.1:18001",
                "health_path": "/health",
                "metrics_path": "/metrics",
                "model_list_path": "/v1/models",
                "enabled": False,
            }
        ],
        "models": [
            {
                "alias": "reasoning_claude",
                "provider": "anthropic",
                "model": "claude-sonnet-4-20250514",
                "enabled": True,
                "modalities": ["text", "tool_call"],
                "quality_score": 0.95,
                "avg_latency_ms": 1200.0,
                "cost": {"input_per_1k": 0.003, "output_per_1k": 0.015},
            }
        ],
        "registry": {"reasoning": "reasoning_claude"},
        "routing_policy": {
            "priority": "latency",
            "fallback_chain": {},
            "load_balance_strategy": "round_robin",
            "cost_budget": None,
        },
    }
    config_path.write_text(yaml.safe_dump(legacy_payload, sort_keys=False), encoding="utf-8")

    config = load_gateway_config(config_path)
    model_ids = {str(model.model_id or model.alias) for model in config.models}

    assert "platform_text_default" in model_ids
    assert "platform_embedding_default" in model_ids
    assert "platform_image_default" in model_ids
    assert "platform_speech_to_text_default" in model_ids
    assert "platform_text_to_speech_default" in model_ids

    reloaded = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    persisted_models = {
        str(item.get("model_id") or item.get("alias")) for item in reloaded.get("models", []) if isinstance(item, dict)
    }
    assert "platform_text_default" in persisted_models
