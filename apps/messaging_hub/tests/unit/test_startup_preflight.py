"""Messaging Hub startup preflight tests."""

from __future__ import annotations

from typing import Any

import pytest

from apps.messaging_hub import main
from contracts.app.contracts import LLMContractConfig


@pytest.mark.asyncio
async def test_run_startup_preflight_runs_db_then_llm(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """起動 preflight が DB → LLM の順で実行されること."""
    called: list[str] = []

    async def _fake_db_probe() -> None:
        called.append("db")

    async def _fake_llm_probe() -> None:
        called.append("llm")

    monkeypatch.setattr(main, "_probe_db_connectivity", _fake_db_probe)
    monkeypatch.setattr(main, "_probe_llm_connectivity", _fake_llm_probe)

    await main._run_startup_preflight()

    assert called == ["db", "llm"]


@pytest.mark.asyncio
async def test_run_startup_preflight_stops_on_db_failure(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """DB preflight が失敗した場合は LLM preflight を実行しないこと."""

    async def _fake_db_probe() -> None:
        msg = "db preflight failed"
        raise RuntimeError(msg)

    async def _fake_llm_probe() -> None:
        msg = "LLM probe must not run after DB failure"
        raise AssertionError(msg)

    monkeypatch.setattr(main, "_probe_db_connectivity", _fake_db_probe)
    monkeypatch.setattr(main, "_probe_llm_connectivity", _fake_llm_probe)

    with pytest.raises(RuntimeError, match="db preflight failed"):
        await main._run_startup_preflight()


@pytest.mark.asyncio
async def test_probe_llm_connectivity_requires_contracts(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """contracts.llm が欠落/無効なら即失敗すること."""
    monkeypatch.setattr(main, "load_app_llm_contracts", lambda _app_name: None)

    with pytest.raises(RuntimeError, match="contracts\\.llm is missing or disabled"):
        await main._probe_llm_connectivity()


@pytest.mark.asyncio
async def test_probe_llm_connectivity_runs_runtime_probe(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """contracts 解決後に LLM 実呼び出し probe を実行すること."""
    llm_contract = LLMContractConfig.model_validate(
        {
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
            "extra_model_refs": [],
        }
    )

    class _FakeLLM:
        async def generate(self, **kwargs: Any) -> dict[str, Any]:
            metadata = kwargs.get("metadata", {})
            assert metadata.get("app_name") == "messaging_hub"
            assert metadata.get("agent_name") == "startup_preflight"
            return {"content": "pong", "model": "mock/probe", "latency_ms": 1.0}

    monkeypatch.setattr(main, "load_app_llm_contracts", lambda _app_name: llm_contract)
    monkeypatch.setattr(main, "resolve_contract_model_alias", lambda **_kwargs: "platform_text_default")
    monkeypatch.setattr(main, "get_llm", lambda **_kwargs: _FakeLLM())

    await main._probe_llm_connectivity()
