"""Layer 2 rerank gateway."""

from __future__ import annotations

from typing import TYPE_CHECKING

from infrastructure.rerank.registry import get_rerank_backend


if TYPE_CHECKING:
    from infrastructure.rerank.ports import RerankResult

    from shared.registry import ComponentToggle


class SharedRerankGateway:
    """rerank backend を隠蔽する共通 gateway."""

    def __init__(self, toggle: ComponentToggle | None = None) -> None:
        self._backend = get_rerank_backend(toggle)

    async def rerank(self, query: str, documents: list[str], *, top_k: int = 5) -> list[RerankResult]:
        """問い合わせに対して上位文書を返す。"""
        return await self._backend.rerank(query, documents, top_k=top_k)
