"""DocumentPreviewBackend - DocumentManager 上の preview を使う軽量検索."""

from __future__ import annotations

import json
import re
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


if TYPE_CHECKING:
    from shared.rag.document_manager import DocumentManager
    from shared.rag.models import DocumentRecordModel


class DocumentPreviewBackend(RetrievalBackend):
    """DocumentManager の preview_chunks を lexical fallback として使う."""

    def __init__(
        self,
        collection: str,
        document_manager: DocumentManager,
        *,
        chunk_strategy: str = "recursive",
        chunk_size: int = 1000,
        chunk_overlap: int = 200,
        top_k: int = 5,
    ) -> None:
        super().__init__(backend_type=BackendType.FILE_SYSTEM, name=f"preview:{collection}")
        self._collection = collection
        self._document_manager = document_manager
        self._chunk_strategy = chunk_strategy
        self._chunk_size = chunk_size
        self._chunk_overlap = chunk_overlap
        self._top_k = top_k

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        top_k = query.options.get("top_k", query.top_k or self._top_k)
        documents = await self._document_manager.list_documents(self._collection, limit=max(top_k * 8, 100))

        candidates: list[RetrievedDocument] = []
        for document in documents:
            if not self._matches_filters(document, query.filters):
                continue

            previews = await self._document_manager.preview_chunks(
                document.document_id,
                chunk_strategy=self._chunk_strategy,
                chunk_size=self._chunk_size,
                chunk_overlap=self._chunk_overlap,
            )
            if not previews:
                continue

            best_content = ""
            best_score = 0.0
            for preview in previews[:3]:
                content = str(preview.get("content") or "")
                score = self._keyword_overlap_score(query.query, content)
                if score > best_score:
                    best_content = content
                    best_score = score

            if best_score <= 0.0:
                continue

            metadata = self._build_metadata(document)
            metadata["retrieval_backend"] = "preview"
            candidates.append(
                RetrievedDocument(
                    doc_id=document.document_id,
                    content=best_content,
                    score=best_score,
                    source=document.filename,
                    metadata=metadata,
                )
            )

        candidates.sort(key=lambda item: item.score, reverse=True)
        return RetrievalResult(
            query=query.query,
            documents=candidates[:top_k],
            total_found=len(candidates),
            backend_type=BackendType.FILE_SYSTEM,
            metadata={"collection": self._collection, "backend": self.name},
        )

    async def health_check(self) -> bool:
        return True

    @staticmethod
    def _keyword_overlap_score(query_text: str, content: str) -> float:
        query_terms = {
            token
            for token in re.findall(r"[A-Za-z0-9]{2,}", query_text.lower())
            if len(token) >= 3
        }
        if not query_terms:
            return 0.0

        lowered_content = content.lower()
        matches = sum(1 for token in query_terms if token in lowered_content)
        phrase_bonus = 0.15 if query_text.lower() in lowered_content else 0.0
        return min((matches / len(query_terms)) + phrase_bonus, 1.0)

    @staticmethod
    def _build_metadata(document: DocumentRecordModel) -> dict[str, Any]:
        metadata: dict[str, Any] = {
            "document_id": document.document_id,
            "collection_name": document.collection_name,
        }
        if document.document_group_id:
            metadata["document_group_id"] = document.document_group_id

        try:
            stored_metadata = json.loads(document.metadata_json or "{}")
            if isinstance(stored_metadata, dict):
                metadata.update(stored_metadata)
        except (TypeError, ValueError):
            pass

        try:
            tags = json.loads(document.tags_json or "[]")
            if isinstance(tags, list):
                metadata["tags"] = [tag for tag in tags if isinstance(tag, str) and tag.strip()]
        except (TypeError, ValueError):
            pass

        return metadata

    @classmethod
    def _matches_filters(cls, document: DocumentRecordModel, filters: dict[str, Any]) -> bool:
        if not filters:
            return True

        metadata = cls._build_metadata(document)
        for key, expected in filters.items():
            if key == "document_group_id":
                if document.document_group_id != expected:
                    return False
                continue
            if key == "document_id":
                if document.document_id != expected:
                    return False
                continue
            if metadata.get(key) != expected:
                return False
        return True
