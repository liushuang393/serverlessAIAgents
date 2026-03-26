"""Layer 2 embeddings gateway."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from shared.registry import ComponentToggle


class SharedEmbeddingGateway:
    """埋め込み backend を隠蔽する共通 gateway."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        # 遅延 import: infrastructure 依存をトップレベルから排除
        from infrastructure.embeddings.registry import get_embedding_backend

        self._backend = get_embedding_backend(toggle)

    async def embed_query(self, text: str) -> list[float]:
        """単一クエリをベクトル化する。"""
        return await self._backend.embed_query(text)

    async def embed_documents(self, texts: list[str]) -> list[list[float]]:
        """複数文書をベクトル化する。"""
        return await self._backend.embed_documents(texts)

    @property
    def dimension(self) -> int:
        """埋め込み次元数を返す。"""
        return self._backend.dimension

    def model_name(self) -> str:
        """利用モデル名を返す。"""
        return self._backend.get_model_name()
