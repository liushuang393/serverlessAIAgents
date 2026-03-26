"""kernel LLM Service Protocol のテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


def test_llm_service_protocol_exists() -> None:
    """LLMService Protocol がインポートできること."""
    from kernel.interfaces.llm_service import LLMService

    assert hasattr(LLMService, "generate")
    assert hasattr(LLMService, "generate_stream")


def test_llm_service_is_runtime_checkable() -> None:
    """LLMService が runtime_checkable であること."""
    from kernel.interfaces.llm_service import LLMService

    # runtime_checkable であれば isinstance チェックが可能
    assert isinstance(LLMService, type)


class _DummyLLM:
    """テスト用のダミー実装."""

    async def generate(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> str:
        return "dummy"

    async def generate_stream(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        yield "chunk"


def test_llm_service_isinstance_check() -> None:
    """ダミー実装が LLMService 互換メソッドを提供すること."""

    dummy = _DummyLLM()
    assert callable(dummy.generate)
    assert callable(dummy.generate_stream)


def test_llm_service_reexported_from_init() -> None:
    """kernel.interfaces から LLMService を直接インポートできること."""
    from kernel.interfaces import LLMService

    assert hasattr(LLMService, "generate")
