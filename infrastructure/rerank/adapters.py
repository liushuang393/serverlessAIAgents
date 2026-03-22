"""Layer 1 rerank 具体実装."""

from __future__ import annotations

from collections import Counter

from infrastructure.rerank.ports import RerankResult


class NoOpRerankBackend:
    """無効化時に使う no-op rerank backend."""

    async def rerank(self, query: str, documents: list[str], *, top_k: int = 5) -> list[RerankResult]:
        del query
        return [RerankResult(index=index, text=text, score=0.0) for index, text in enumerate(documents[:top_k])]


class MockRerankBackend:
    """文字トークンの重なりで簡易スコアリングする mock backend."""

    async def rerank(self, query: str, documents: list[str], *, top_k: int = 5) -> list[RerankResult]:
        query_tokens = Counter(query.lower().split())
        ranked: list[RerankResult] = []
        for index, text in enumerate(documents):
            doc_tokens = Counter(text.lower().split())
            overlap = sum(min(query_tokens[token], doc_tokens[token]) for token in query_tokens)
            ranked.append(RerankResult(index=index, text=text, score=float(overlap)))
        return sorted(ranked, key=lambda item: item.score, reverse=True)[:top_k]
