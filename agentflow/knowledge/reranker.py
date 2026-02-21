"""Reranker モジュール."""

from __future__ import annotations

import logging
import os
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)
DocumentInput = str | dict[str, Any]


class RerankerType(str, Enum):
    COHERE = "cohere"
    CROSS_ENCODER = "cross_encoder"
    BM25 = "bm25"


@dataclass
class RankedDocument:
    content: str
    score: float
    original_index: int
    metadata: dict[str, Any] = field(default_factory=dict)


class BaseReranker(ABC):
    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)

    @property
    @abstractmethod
    def reranker_type(self) -> RerankerType: ...

    @abstractmethod
    async def rerank(
        self,
        query: str,
        documents: list[DocumentInput],
        top_k: int = 5,
    ) -> list[RankedDocument]: ...

    def _normalize_docs(self, documents: list[DocumentInput]) -> list[str]:
        doc_texts: list[str] = []
        for doc in documents:
            if isinstance(doc, str):
                doc_texts.append(doc)
            else:
                content = doc.get("content", doc.get("document", str(doc)))
                doc_texts.append(str(content))
        return doc_texts

    def _get_metadata(self, doc: Any) -> dict[str, Any]:
        if isinstance(doc, dict):
            return {k: v for k, v in doc.items() if k not in ("content", "document")}
        return {}


class CohereReranker(BaseReranker):
    def __init__(self, api_key: str | None = None, model: str = "rerank-multilingual-v3.0") -> None:
        super().__init__()
        self._api_key = api_key or os.getenv("COHERE_API_KEY")
        self._model = model
        self._client: Any = None

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.COHERE

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        if not self._api_key:
            msg = "COHERE_API_KEY is required"
            raise ValueError(msg)
        doc_texts = self._normalize_docs(documents)
        try:
            import cohere

            if self._client is None:
                self._client = cohere.Client(self._api_key)
            response = self._client.rerank(query=query, documents=doc_texts, top_n=top_k, model=self._model)
            results = []
            for result in response.results:
                metadata = self._get_metadata(documents[result.index])
                results.append(
                    RankedDocument(
                        content=doc_texts[result.index],
                        score=result.relevance_score,
                        original_index=result.index,
                        metadata=metadata,
                    )
                )
            return results
        except ImportError:
            msg = "cohere package required: pip install cohere"
            raise ImportError(msg)


class CrossEncoderReranker(BaseReranker):
    def __init__(self, model_name: str = "cross-encoder/ms-marco-MiniLM-L-6-v2") -> None:
        super().__init__()
        self._model_name = model_name
        self._model: Any = None

    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.CROSS_ENCODER

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        doc_texts = self._normalize_docs(documents)
        try:
            from sentence_transformers import CrossEncoder

            if self._model is None:
                self._model = CrossEncoder(self._model_name)
            pairs = [(query, doc) for doc in doc_texts]
            scores = self._model.predict(pairs)
            scored_docs = list(zip(range(len(documents)), doc_texts, scores, documents, strict=False))
            scored_docs.sort(key=lambda x: x[2], reverse=True)
            results = []
            for original_idx, text, score, original_doc in scored_docs[:top_k]:
                metadata = self._get_metadata(original_doc)
                results.append(
                    RankedDocument(
                        content=text,
                        score=float(score),
                        original_index=original_idx,
                        metadata=metadata,
                    )
                )
            return results
        except ImportError:
            msg = "sentence-transformers required: pip install sentence-transformers"
            raise ImportError(msg)


class BM25Reranker(BaseReranker):
    @property
    def reranker_type(self) -> RerankerType:
        return RerankerType.BM25

    async def rerank(self, query: str, documents: list[DocumentInput], top_k: int = 5) -> list[RankedDocument]:
        doc_texts = self._normalize_docs(documents)
        query_terms = set(query.lower().split())
        scores = []
        for doc in doc_texts:
            doc_terms = doc.lower().split()
            doc_term_set = set(doc_terms)
            common_terms = query_terms.intersection(doc_term_set)
            tf = len(common_terms) / max(len(query_terms), 1)
            doc_length_factor = 1.0 / (1.0 + len(doc_terms) / 100)
            score = tf * doc_length_factor
            scores.append(score)
        scored_docs = list(zip(range(len(documents)), doc_texts, scores, documents, strict=False))
        scored_docs.sort(key=lambda x: x[2], reverse=True)
        results = []
        for original_idx, text, score, original_doc in scored_docs[:top_k]:
            metadata = self._get_metadata(original_doc)
            results.append(RankedDocument(content=text, score=score, original_index=original_idx, metadata=metadata))
        return results


def get_reranker(reranker_type: RerankerType | str | None = None) -> BaseReranker:
    if reranker_type is None:
        env_type = os.getenv("RERANKER_TYPE", "").lower()
        if env_type:
            reranker_type = RerankerType(env_type)
        elif os.getenv("COHERE_API_KEY"):
            reranker_type = RerankerType.COHERE
        else:
            try:
                import sentence_transformers

                reranker_type = RerankerType.CROSS_ENCODER
            except ImportError:
                reranker_type = RerankerType.BM25
    if isinstance(reranker_type, str):
        reranker_type = RerankerType(reranker_type)
    rerankers: dict[RerankerType, type[BaseReranker]] = {
        RerankerType.COHERE: CohereReranker,
        RerankerType.CROSS_ENCODER: CrossEncoderReranker,
        RerankerType.BM25: BM25Reranker,
    }
    reranker_cls = rerankers.get(reranker_type or RerankerType.BM25, BM25Reranker)
    try:
        return reranker_cls()
    except Exception as e:
        logger.warning(f"Failed to create reranker: {e}")
        return BM25Reranker()


__all__ = [
    "BM25Reranker",
    "BaseReranker",
    "CohereReranker",
    "CrossEncoderReranker",
    "RankedDocument",
    "RerankerType",
    "get_reranker",
]
