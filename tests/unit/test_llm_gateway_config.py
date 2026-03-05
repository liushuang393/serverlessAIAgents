"""Tests for LLM gateway config and secret resolution."""

from __future__ import annotations

from pathlib import Path

from agentflow.llm.gateway.config import build_provider_runtime_statuses, load_gateway_config, resolve_secret


def test_resolve_secret_prefers_env_over_dotenv(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)
    (tmp_path / ".env").write_text("OPENAI_API_KEY=dotenv-key\n", encoding="utf-8")
    monkeypatch.setenv("OPENAI_API_KEY", "env-key")

    value, source = resolve_secret("OPENAI_API_KEY", config_path=config_path)
    assert value == "env-key"
    assert source == "ENV"


def test_resolve_secret_falls_back_to_dotenv(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)
    (tmp_path / ".env").write_text("GEMINI_API_KEY=dotenv-gemini\n", encoding="utf-8")
    monkeypatch.delenv("GEMINI_API_KEY", raising=False)

    value, source = resolve_secret("GEMINI_API_KEY", config_path=config_path)
    assert value == "dotenv-gemini"
    assert source == ".env"


def test_load_gateway_config_applies_env_overrides(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    _ = load_gateway_config(config_path)

    monkeypatch.setenv("LLM_GATEWAY_PRIORITY", "cost")
    monkeypatch.setenv("LLM_GATEWAY_LOAD_BALANCE", "least_latency")
    monkeypatch.setenv("LLM_ROLE_REASONING_ALIAS", "coding_openai")

    config = load_gateway_config(config_path)
    assert config.routing_policy.priority == "cost"
    assert config.routing_policy.load_balance_strategy == "least_latency"
    assert config.registry["reasoning"] == "coding_openai"


def test_provider_runtime_status_unavailable_when_key_missing(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    config = load_gateway_config(config_path)
    monkeypatch.delenv("OPENAI_API_KEY", raising=False)

    statuses = build_provider_runtime_statuses(config, config_path=config_path)
    by_name = {status.name: status for status in statuses}

    assert by_name["openai"].status == "unavailable"
    assert by_name["openai"].last_error == "api_key_not_configured"
    assert by_name["local"].status == "available"
