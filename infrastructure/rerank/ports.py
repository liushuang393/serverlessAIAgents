"""Layer 1 rerank 抽象契約."""

from __future__ import annotations

from typing import Protocol

from contracts.base import ContractModel
from pydantic import Field


class RerankResult(ContractModel):
    """rerank 済み候補."""

    index: int = Field(..., description="元文書インデックス")
    text: str = Field(..., description="対象文書")
    score: float = Field(..., description="関連度スコア")


class RerankBackend(Protocol):
    """Shared Gateway が利用する rerank backend 契約."""

    async def rerank(self, query: str, documents: list[str], *, top_k: int = 5) -> list[RerankResult]:
        """問い合わせと文書群を並び替える。"""
