"""ドキュメント取込スキル - Document Ingester.

ドキュメントを取り込み、チャンク化してベクトル化するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class IngestConfig:
    """取込設定."""

    chunk_size: int = 500
    chunk_overlap: int = 50
    supported_formats: list[str] = field(
        default_factory=lambda: ["md", "txt", "pdf", "html"]
    )


@dataclass
class IngestedDoc:
    """取り込んだドキュメント."""

    doc_id: str
    title: str
    chunks_count: int
    format: str
    size_bytes: int
    ingested_at: datetime = field(default_factory=datetime.now)


@dataclass
class IngestResult:
    """取込結果."""

    documents: list[IngestedDoc]
    total_chunks: int
    total_size_bytes: int
    errors: list[str] = field(default_factory=list)


class DocIngester(AgentBlock):
    """ドキュメント取込スキル."""

    def __init__(self, config: IngestConfig | None = None) -> None:
        super().__init__()
        self._config = config or IngestConfig()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        documents = input_data.get("documents", [])

        result = await self.ingest(documents=documents)

        return {
            "documents": [
                {
                    "doc_id": d.doc_id,
                    "title": d.title,
                    "chunks_count": d.chunks_count,
                }
                for d in result.documents
            ],
            "total_chunks": result.total_chunks,
            "total_size_bytes": result.total_size_bytes,
            "errors": result.errors,
        }

    async def ingest(
        self,
        documents: list[dict[str, Any]] | None = None,
    ) -> IngestResult:
        """ドキュメントを取り込み."""
        import uuid

        documents = documents or []
        logger.info("ドキュメント取込開始: %d docs", len(documents))

        ingested: list[IngestedDoc] = []
        total_chunks = 0

        for i, doc in enumerate(documents):
            doc_id = doc.get("id", f"doc-{uuid.uuid4().hex[:8]}")
            title = doc.get("title", f"Document {i+1}")
            content = doc.get("content", "")

            # チャンク化（簡易実装）
            chunks = self._chunk_text(content)

            ingested.append(IngestedDoc(
                doc_id=doc_id,
                title=title,
                chunks_count=len(chunks),
                format=doc.get("format", "txt"),
                size_bytes=len(content.encode()),
            ))
            total_chunks += len(chunks)

        return IngestResult(
            documents=ingested,
            total_chunks=total_chunks,
            total_size_bytes=sum(d.size_bytes for d in ingested),
        )

    def _chunk_text(self, text: str) -> list[str]:
        """テキストをチャンク化."""
        if not text:
            return []

        chunks = []
        for i in range(0, len(text), self._config.chunk_size - self._config.chunk_overlap):
            chunk = text[i:i + self._config.chunk_size]
            if chunk:
                chunks.append(chunk)
        return chunks
