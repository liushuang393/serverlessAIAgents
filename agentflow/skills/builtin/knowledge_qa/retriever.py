# -*- coding: utf-8 -*-
"""検索スキル - Retriever.

関連ドキュメントを検索するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class RetrievalConfig:
    """検索設定."""

    top_k: int = 5
    similarity_threshold: float = 0.7
    rerank: bool = True


@dataclass
class RetrievedChunk:
    """検索されたチャンク."""

    doc_id: str
    chunk_id: str
    content: str
    score: float
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class RetrievalResult:
    """検索結果."""

    chunks: list[RetrievedChunk]
    query: str
    total_found: int


class Retriever(AgentBlock):
    """検索スキル."""

    def __init__(
        self,
        config: RetrievalConfig | None = None,
        vector_store: Any | None = None,
    ) -> None:
        super().__init__()
        self._config = config or RetrievalConfig()
        self._vector_store = vector_store

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        query = input_data.get("query", "")
        top_k = input_data.get("top_k", self._config.top_k)

        result = await self.retrieve(query=query, top_k=top_k)

        return {
            "chunks": [
                {
                    "doc_id": c.doc_id,
                    "content": c.content,
                    "score": c.score,
                }
                for c in result.chunks
            ],
            "query": result.query,
            "total_found": result.total_found,
        }

    async def retrieve(
        self,
        query: str,
        top_k: int | None = None,
    ) -> RetrievalResult:
        """関連ドキュメントを検索."""
        top_k = top_k or self._config.top_k
        logger.info("検索開始: query=%s, top_k=%d", query[:50], top_k)

        # プレースホルダー実装（実際はベクトルDBを使用）
        chunks = [
            RetrievedChunk(
                doc_id="doc-001",
                chunk_id="chunk-001",
                content=f"関連コンテンツ: {query[:20]}に関する情報...",
                score=0.92,
                metadata={"title": "サンプルドキュメント"},
            ),
            RetrievedChunk(
                doc_id="doc-002",
                chunk_id="chunk-002",
                content="追加の関連情報...",
                score=0.85,
            ),
        ]

        return RetrievalResult(
            chunks=chunks[:top_k],
            query=query,
            total_found=len(chunks),
        )
