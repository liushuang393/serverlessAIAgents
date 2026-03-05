"""Tests for Platform LLMManagementService."""

from __future__ import annotations

from pathlib import Path

from apps.platform.services.llm_management import LLMManagementService


def test_get_overview_contains_required_sections(tmp_path: Path) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    overview = service.get_overview()
    assert "providers" in overview
    assert "inference_engines" in overview
    assert "models" in overview
    assert "registry" in overview
    assert "routing_policy" in overview


def test_update_registry_normalizes_role_and_alias(tmp_path: Path) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    service = LLMManagementService(config_path=config_path)

    updated = service.update_registry({"Reasoning": "Coding_OpenAI", "cheap": "cheap_gemini"})
    assert updated["reasoning"] == "coding_openai"
    assert updated["cheap"] == "cheap_gemini"


def test_provider_runtime_marks_missing_key_as_unavailable(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / ".agentflow" / "llm_gateway.yaml"
    monkeypatch.delenv("OPENAI_API_KEY", raising=False)
    service = LLMManagementService(config_path=config_path)

    statuses = service.get_provider_runtime()
    by_name = {item["name"]: item for item in statuses}
    assert by_name["openai"]["status"] == "unavailable"
