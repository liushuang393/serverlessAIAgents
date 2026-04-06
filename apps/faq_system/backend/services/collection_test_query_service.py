"""Collection test-query 用の unified retrieval service."""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Callable

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery, RetrievedDocument
from apps.faq_system.backend.mcp.backends.document_preview import DocumentPreviewBackend
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from shared.services.rag_service import RAGConfig, RAGDocument, RAGService


if TYPE_CHECKING:
    from apps.faq_system.backend.mcp.backends.base import RetrievalBackend
    from shared.rag.document_manager import DocumentManager


_MAX_GROUP_EXPANSION_DOCS = 3


class CollectionTestQueryService:
    """`/test-query` の retrieval / related expansion / answer synthesis を統合する."""

    def __init__(
        self,
        *,
        collection_name: str,
        rag_config: RAGConfig,
        document_manager: DocumentManager,
        vector_backend_factory: Callable[[], RetrievalBackend] | None = None,
        preview_backend_factory: Callable[[], RetrievalBackend] | None = None,
        rag_service_factory: Callable[[RAGConfig], Any] | None = None,
    ) -> None:
        self._collection_name = collection_name
        self._rag_config = rag_config
        self._document_manager = document_manager
        self._vector_backend_factory = vector_backend_factory or self._default_vector_backend_factory
        self._preview_backend_factory = preview_backend_factory or self._default_preview_backend_factory
        self._rag_service_factory = rag_service_factory or (lambda config: RAGService(config))
        self._vector_backend: RetrievalBackend | None = None
        self._preview_backend: RetrievalBackend | None = None
        self._answer_service: Any = None

    async def run_test_query(
        self,
        *,
        query: str,
        top_k: int,
        expand_related: bool = True,
    ) -> dict[str, Any]:
        primary_documents, used_lexical_fallback = await self._retrieve_primary_documents(query=query, top_k=top_k)
        related_documents = (
            await self._expand_related_documents(query=query, top_k=top_k, primary_documents=primary_documents)
            if expand_related and primary_documents
            else []
        )

        merged_documents = self._to_rag_documents(primary_documents + related_documents)
        answer, answer_mode = await self._generate_answer(query=query, documents=merged_documents)
        diagnostics = {
            "retrieval_method": self._normalized_retrieval_method,
            "used_lexical_fallback": used_lexical_fallback,
            "expanded_related_count": len(related_documents),
            "answer_mode": answer_mode,
            "primary_sources": [doc.source for doc in primary_documents],
            "related_sources": [doc.source for doc in related_documents],
        }

        return {
            "answer": answer,
            "documents": [self._serialize_document(doc) for doc in primary_documents],
            "related_documents": [self._serialize_document(doc) for doc in related_documents],
            "query": query,
            "total": len(primary_documents) + len(related_documents),
            "diagnostics": diagnostics,
        }

    async def cleanup(self) -> None:
        if self._vector_backend is not None:
            await self._vector_backend.cleanup()
        if self._preview_backend is not None:
            await self._preview_backend.cleanup()
        if self._answer_service is not None and hasattr(self._answer_service, "stop"):
            await self._answer_service.stop()

    @property
    def _normalized_retrieval_method(self) -> str:
        method = getattr(self._rag_config, "retrieval_method", "semantic") or "semantic"
        return str(method).lower()

    def _default_vector_backend_factory(self) -> RetrievalBackend:
        return VectorStoreBackend(
            collection=self._collection_name,
            chunk_strategy=self._rag_config.chunk_strategy.value,
            reranker=self._rag_config.reranker.value,
            top_k=self._rag_config.top_k,
            min_similarity=self._rag_config.min_similarity,
            retrieval_method=self._normalized_retrieval_method,
        )

    def _default_preview_backend_factory(self) -> RetrievalBackend:
        return DocumentPreviewBackend(
            collection=self._collection_name,
            document_manager=self._document_manager,
            chunk_strategy=self._rag_config.chunk_strategy.value,
            chunk_size=self._rag_config.chunk_size,
            chunk_overlap=self._rag_config.chunk_overlap,
            top_k=self._rag_config.top_k,
        )

    async def _get_vector_backend(self) -> RetrievalBackend:
        if self._vector_backend is None:
            self._vector_backend = self._vector_backend_factory()
            await self._vector_backend.initialize()
        return self._vector_backend

    async def _get_preview_backend(self) -> RetrievalBackend:
        if self._preview_backend is None:
            self._preview_backend = self._preview_backend_factory()
            await self._preview_backend.initialize()
        return self._preview_backend

    async def _get_answer_service(self) -> Any:
        if self._answer_service is None:
            self._answer_service = self._rag_service_factory(self._rag_config)
            if hasattr(self._answer_service, "start"):
                await self._answer_service.start()
        return self._answer_service

    async def _retrieve_primary_documents(
        self,
        *,
        query: str,
        top_k: int,
    ) -> tuple[list[RetrievedDocument], bool]:
        retrieval_query = RetrievalQuery(query=query, top_k=top_k)
        method = self._normalized_retrieval_method

        vector_documents: list[RetrievedDocument] = []
        preview_documents: list[RetrievedDocument] = []

        if method in {"semantic", "hybrid", "multi_query"}:
            vector_backend = await self._get_vector_backend()
            vector_documents = (await vector_backend.retrieve(retrieval_query)).documents

        if method in {"keyword", "hybrid", "multi_query"}:
            preview_backend = await self._get_preview_backend()
            preview_documents = (await preview_backend.retrieve(retrieval_query)).documents

        if method == "keyword":
            return self._merge_documents(preview_documents, [], top_k=top_k), bool(preview_documents)

        if method in {"hybrid", "multi_query"}:
            merged_documents = self._merge_documents(vector_documents, preview_documents, top_k=top_k)
            used_lexical_fallback = bool(preview_documents) and (
                not vector_documents
                or self._is_low_confidence(vector_documents, top_k)
                or any(self._doc_identity(doc) not in {self._doc_identity(item) for item in vector_documents} for doc in preview_documents)
            )
            return merged_documents, used_lexical_fallback

        return self._merge_documents(vector_documents, [], top_k=top_k), False

    async def _expand_related_documents(
        self,
        *,
        query: str,
        top_k: int,
        primary_documents: list[RetrievedDocument],
    ) -> list[RetrievedDocument]:
        primary_ids = {self._doc_identity(doc) for doc in primary_documents}
        group_ids = {
            doc.metadata.get("document_group_id")
            for doc in primary_documents
            if isinstance(doc.metadata.get("document_group_id"), str)
        }
        if not group_ids:
            return []

        related_documents: list[RetrievedDocument] = []
        for group_id in group_ids:
            query_with_filter = RetrievalQuery(
                query=query,
                top_k=max(top_k, _MAX_GROUP_EXPANSION_DOCS) + len(primary_documents),
                filters={"document_group_id": group_id},
            )

            vector_candidates: list[RetrievedDocument] = []
            preview_candidates: list[RetrievedDocument] = []

            if self._normalized_retrieval_method != "keyword":
                vector_candidates = (await (await self._get_vector_backend()).retrieve(query_with_filter)).documents
            if self._normalized_retrieval_method in {"keyword", "hybrid", "multi_query"}:
                preview_candidates = (await (await self._get_preview_backend()).retrieve(query_with_filter)).documents

            for candidate in self._merge_documents(vector_candidates, preview_candidates, top_k=_MAX_GROUP_EXPANSION_DOCS * 2):
                identity = self._doc_identity(candidate)
                if identity in primary_ids or identity in {self._doc_identity(doc) for doc in related_documents}:
                    continue
                candidate.metadata["expanded_from_group"] = group_id
                related_documents.append(candidate)
                if len(related_documents) >= _MAX_GROUP_EXPANSION_DOCS:
                    return related_documents

        if len(related_documents) < _MAX_GROUP_EXPANSION_DOCS:
            sibling_previews = await self._load_group_sibling_previews(group_ids=group_ids, seen_ids=primary_ids | {self._doc_identity(doc) for doc in related_documents})
            related_documents.extend(sibling_previews[: max(_MAX_GROUP_EXPANSION_DOCS - len(related_documents), 0)])

        return related_documents[:_MAX_GROUP_EXPANSION_DOCS]

    async def _load_group_sibling_previews(
        self,
        *,
        group_ids: set[str],
        seen_ids: set[str],
    ) -> list[RetrievedDocument]:
        sibling_documents = await self._document_manager.list_documents(self._collection_name, limit=1000)
        previews: list[RetrievedDocument] = []
        for sibling in sibling_documents:
            if sibling.document_group_id not in group_ids:
                continue
            if sibling.document_id in seen_ids:
                continue

            chunks = await self._document_manager.preview_chunks(
                sibling.document_id,
                chunk_strategy=self._rag_config.chunk_strategy.value,
                chunk_size=self._rag_config.chunk_size,
                chunk_overlap=self._rag_config.chunk_overlap,
            )
            if not chunks:
                continue

            previews.append(
                RetrievedDocument(
                    doc_id=sibling.document_id,
                    content=str(chunks[0].get("content") or ""),
                    score=0.0,
                    source=sibling.filename,
                    metadata={
                        "document_id": sibling.document_id,
                        "document_group_id": sibling.document_group_id,
                        "retrieval_backend": "preview",
                        "expanded_from_group": sibling.document_group_id,
                    },
                )
            )
        return previews

    async def _generate_answer(
        self,
        *,
        query: str,
        documents: list[RAGDocument],
    ) -> tuple[str, str]:
        if not documents:
            return "関連する情報が見つかりませんでした。", "no_context"

        answer_service = await self._get_answer_service()
        context = answer_service._build_context(documents)
        answer_mode = "default"
        question_for_generation = query

        if self._is_policy_conflict_query(query) and self._context_has_policy_conflict_rule(context):
            answer_mode = "policy_conflict_guided"
            question_for_generation = (
                f"{query}\n"
                "If the context contains a rule conflict, state explicitly that the latest update notice wins "
                "and cite the effective date used to resolve the conflict."
            )

        answer = await answer_service._generate_answer(question_for_generation, context)
        if answer_mode == "policy_conflict_guided":
            answer = self._ensure_policy_conflict_facts(answer=answer, context=context)

        return answer, answer_mode

    @staticmethod
    def _merge_documents(
        primary: list[RetrievedDocument],
        secondary: list[RetrievedDocument],
        *,
        top_k: int,
    ) -> list[RetrievedDocument]:
        merged: dict[str, RetrievedDocument] = {}

        for document in [*primary, *secondary]:
            identity = CollectionTestQueryService._doc_identity(document)
            existing = merged.get(identity)
            if existing is None:
                merged[identity] = document
                continue

            existing_backend = str(existing.metadata.get("retrieval_backend") or "")
            current_backend = str(document.metadata.get("retrieval_backend") or "")
            if document.score > existing.score or (
                document.score == existing.score and current_backend == "vector" and existing_backend != "vector"
            ):
                merged[identity] = document

        return sorted(merged.values(), key=lambda item: item.score, reverse=True)[:top_k]

    @staticmethod
    def _doc_identity(document: RetrievedDocument) -> str:
        identity = document.metadata.get("document_id") if isinstance(document.metadata, dict) else None
        if isinstance(identity, str) and identity:
            return identity
        return document.doc_id or document.source

    def _to_rag_documents(self, documents: list[RetrievedDocument]) -> list[RAGDocument]:
        return [
            RAGDocument(
                id=document.doc_id,
                content=document.content,
                source=document.source,
                metadata=document.metadata,
                score=document.score,
            )
            for document in documents
        ]

    @staticmethod
    def _serialize_document(document: RetrievedDocument) -> dict[str, Any]:
        return {
            "id": document.doc_id,
            "content": document.content,
            "source": document.source,
            "score": document.score,
            "metadata": document.metadata,
        }

    def _is_low_confidence(self, documents: list[RetrievedDocument], top_k: int) -> bool:
        if not documents:
            return True
        best_score = max(document.score for document in documents)
        return best_score < max(self._rag_config.min_similarity + 0.05, 0.45) or len(documents) < min(top_k, 2)

    @staticmethod
    def _is_policy_conflict_query(query: str) -> bool:
        lowered = query.lower()
        return "conflict" in lowered or "which document" in lowered or "which rule" in lowered

    @staticmethod
    def _context_has_policy_conflict_rule(context: str) -> bool:
        lowered = context.lower()
        return "latest update notice" in lowered or ("effective date" in lowered and "wins" in lowered)

    @classmethod
    def _ensure_policy_conflict_facts(cls, *, answer: str, context: str) -> str:
        normalized = answer.strip() or "Follow the latest update notice."
        lowered = normalized.lower()

        if "latest update notice" not in lowered:
            normalized = f"{normalized} Follow the latest update notice when it conflicts with the baseline policy."

        if "effective date" not in lowered:
            effective_date = cls._extract_effective_date(context)
            if effective_date is not None:
                normalized = f"{normalized} Use the effective date ({effective_date}) to resolve which document applies."
            else:
                normalized = f"{normalized} Use the effective date to resolve which document applies."

        return normalized

    @staticmethod
    def _extract_effective_date(context: str) -> str | None:
        match = re.search(r"effective date[:\s]+([0-9]{4}-[0-9]{2}-[0-9]{2})", context, re.IGNORECASE)
        if match:
            return match.group(1)
        return None
