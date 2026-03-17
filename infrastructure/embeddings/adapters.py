"""Layer 1 embeddings 具体実装."""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from infrastructure.embeddings.ports import EmbeddingBackend


class NoOpEmbeddingBackend:
    """無効化時に使う no-op embeddings backend."""

    @property
    def dimension(self) -> int:
        return 0

    def get_model_name(self) -> str:
        return "noop"

    async def embed_query(self, text: str) -> list[float]:
        del text
        return []

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        del texts
        return []


class MockEmbeddingBackend:
    """テスト向け固定埋め込み backend."""

    @property
    def dimension(self) -> int:
        return 8

    def get_model_name(self) -> str:
        return "mock"

    async def embed_query(self, text: str) -> list[float]:
        seed = sum(ord(char) for char in text)
        return [((seed + offset) % 17) / 16.0 for offset in range(self.dimension)]

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        return [await self.embed_query(text) for text in texts]


class LegacyEmbeddingBackend:
    """既存埋め込み provider を包む互換 backend."""

    def __init__(self, backend: EmbeddingBackend | None = None) -> None:
        if backend is None:
            provider_module = importlib.import_module("infrastructure.embeddings.provider")
            backend = provider_module.get_embedding()
        self._backend = backend

    @property
    def dimension(self) -> int:
        return self._backend.dimension

    def get_model_name(self) -> str:
        return self._backend.get_model_name()

    async def embed_query(self, text: str) -> list[float]:
        return await self._backend.embed_query(text)

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        return await self._backend.embed_documents(texts)
