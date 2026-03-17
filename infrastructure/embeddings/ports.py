"""Layer 1 embeddings 抽象契約."""

from __future__ import annotations

from typing import Protocol


class EmbeddingBackend(Protocol):
    """Shared Gateway が利用する埋め込み backend 契約."""

    async def embed_query(self, text: str) -> list[float]:
        """単一クエリをベクトル化する。"""

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        """複数文書をベクトル化する。"""

    @property
    def dimension(self) -> int:
        """埋め込み次元数を返す。"""

    def get_model_name(self) -> str:
        """利用モデル名を返す。"""
