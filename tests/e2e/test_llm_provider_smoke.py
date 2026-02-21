"""Opt-in smoke tests for real LLM providers.

These tests are intentionally skipped unless `RUN_PROVIDER_E2E=1` is set.
They are meant to validate provider connectivity with minimal calls.
"""

from __future__ import annotations

import os

import pytest

from agentflow.providers.llm_provider import get_llm, reset_llm


def _require_enabled() -> None:
    if os.getenv("RUN_PROVIDER_E2E") != "1":
        pytest.skip("Set RUN_PROVIDER_E2E=1 to run real-provider smoke tests.")


def _configure_provider(
    monkeypatch: pytest.MonkeyPatch,
    *,
    provider: str,
    api_key_env: str,
    model_env: str,
    default_model: str,
) -> None:
    _require_enabled()
    api_key = os.getenv(api_key_env)
    if not api_key:
        pytest.skip(f"{api_key_env} is not set.")

    monkeypatch.setenv("LLM_PROVIDER", provider)
    monkeypatch.setenv(api_key_env, api_key)
    monkeypatch.setenv(model_env, os.getenv(model_env, default_model))
    monkeypatch.setenv("LLM_TIMEOUT", os.getenv("LLM_TIMEOUT", "20"))
    reset_llm()


@pytest.fixture(autouse=True)
def _reset_llm_fixture():
    reset_llm()
    yield
    reset_llm()


@pytest.mark.e2e
@pytest.mark.real_llm
@pytest.mark.asyncio
async def test_openai_provider_smoke(monkeypatch: pytest.MonkeyPatch) -> None:
    _configure_provider(
        monkeypatch,
        provider="openai",
        api_key_env="OPENAI_API_KEY",
        model_env="OPENAI_MODEL",
        default_model="gpt-4o-mini",
    )
    llm = get_llm(_new_instance=True)
    response = await llm.chat(
        [{"role": "user", "content": "Reply with exactly: pong"}],
        max_tokens=8,
    )
    assert isinstance(response.get("content"), str)
    assert response["content"].strip() != ""


@pytest.mark.e2e
@pytest.mark.real_llm
@pytest.mark.asyncio
async def test_anthropic_provider_smoke(monkeypatch: pytest.MonkeyPatch) -> None:
    _configure_provider(
        monkeypatch,
        provider="anthropic",
        api_key_env="ANTHROPIC_API_KEY",
        model_env="ANTHROPIC_MODEL",
        default_model="claude-3-5-haiku-20241022",
    )
    llm = get_llm(_new_instance=True)
    response = await llm.chat(
        [{"role": "user", "content": "Reply with exactly: pong"}],
        max_tokens=8,
    )
    assert isinstance(response.get("content"), str)
    assert response["content"].strip() != ""


@pytest.mark.e2e
@pytest.mark.real_llm
@pytest.mark.asyncio
async def test_google_provider_smoke(monkeypatch: pytest.MonkeyPatch) -> None:
    _configure_provider(
        monkeypatch,
        provider="google",
        api_key_env="GOOGLE_API_KEY",
        model_env="GOOGLE_MODEL",
        default_model="gemini-1.5-flash",
    )
    llm = get_llm(_new_instance=True)
    response = await llm.chat(
        [{"role": "user", "content": "Reply with exactly: pong"}],
        max_tokens=8,
    )
    assert isinstance(response.get("content"), str)
    assert response["content"].strip() != ""
