"""kernel/interfaces/llm_service.py — Kernel が LLM にアクセスするための抽象."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


@runtime_checkable
class LLMService(Protocol):
    """LLM 呼び出しの抽象インターフェース.

    Kernel はこの Protocol のみに依存し、
    具体的な Provider や Gateway の実装を知らない。
    """

    async def generate(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> str:
        """テキスト生成（非ストリーミング）.

        Args:
            prompt: 入力プロンプト
            model: 使用するモデル名（None の場合はデフォルト）
            temperature: 生成温度
            max_tokens: 最大トークン数
            **kwargs: プロバイダ固有のオプション

        Returns:
            生成されたテキスト
        """
        ...

    async def generate_stream(
        self,
        prompt: str,
        *,
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 4096,
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """ストリーミングテキスト生成.

        Args:
            prompt: 入力プロンプト
            model: 使用するモデル名（None の場合はデフォルト）
            temperature: 生成温度
            max_tokens: 最大トークン数
            **kwargs: プロバイダ固有のオプション

        Yields:
            生成テキストのチャンク
        """
        ...
