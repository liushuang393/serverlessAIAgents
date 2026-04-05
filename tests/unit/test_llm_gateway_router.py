"""Unit tests for LiteLLMGateway runtime model discovery."""

from __future__ import annotations

import asyncio

import pytest

from infrastructure.llm.gateway import LiteLLMGateway, LLMGatewayConfig


def test_extract_loaded_models_supports_ollama_payload() -> None:
    gateway = LiteLLMGateway(config=LLMGatewayConfig())

    payload = {
        "models": [
            {"name": "gemma4", "model": "gemma4", "size": 123},
            {"name": "qwen2.5:72b", "model": "qwen2.5:72b", "size": 456},
        ]
    }

    assert gateway._extract_loaded_models(payload) == ["gemma4", "qwen2.5:72b"]


@pytest.mark.asyncio
async def test_list_ollama_models_parses_cli_output() -> None:
    gateway = LiteLLMGateway(config=LLMGatewayConfig())

    class _FakeProc:
        returncode = 0

        async def communicate(self) -> tuple[bytes, bytes]:
            stdout = (
                "NAME ID SIZE MODIFIED\n"
                "gemma4 abc123 5.4 GB 2 hours ago\n"
                "qwen2.5:72b def456 42 GB 1 day ago\n"
            ).encode("utf-8")
            return stdout, b""

    async def _fake_create_subprocess_exec(*args, **kwargs):  # type: ignore[no-untyped-def]
        del args, kwargs
        return _FakeProc()

    original = asyncio.create_subprocess_exec
    asyncio.create_subprocess_exec = _fake_create_subprocess_exec  # type: ignore[assignment]
    try:
        models = await gateway._list_ollama_models()
    finally:
        asyncio.create_subprocess_exec = original  # type: ignore[assignment]

    assert models == ["gemma4", "qwen2.5:72b"]
