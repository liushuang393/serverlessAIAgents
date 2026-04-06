"""RAG Service - フレームワーク級 RAG サービス.

再利用可能な RAG 機能を提供:
- 複数のチャンキング戦略（recursive, semantic, sentence, token, markdown）
- 複数のリランカー（Cohere, CrossEncoder, BM25）
- 複数のデータソース（ファイル、URL、データベース）

使用例:
    >>> from shared.services.rag_service import RAGService
    >>>
    >>> # Studio/CLI/SDK 全て同一インターフェース
    >>> service = RAGService(
    ...     collection="my_knowledge",
    ...     chunk_strategy="semantic",
    ...     reranker="cohere",
    ... )
    >>> result = await service.execute(
    ...     action="query",
    ...     question="返品ポリシーは何ですか？",
    ... )
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import TYPE_CHECKING, Any

from shared.services.base import (
    ServiceBase,
    ServiceEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


class ChunkStrategy(StrEnum):
    """チャンキング戦略."""

    RECURSIVE = "recursive"
    SEMANTIC = "semantic"
    SENTENCE = "sentence"
    TOKEN = "token"
    MARKDOWN = "markdown"


class RerankerType(StrEnum):
    """リランカータイプ."""

    COHERE = "cohere"
    CROSS_ENCODER = "cross_encoder"
    CROSS_ENCODER_RURI = "cross_encoder_ruri"
    LLM_LISTWISE = "llm_listwise"
    BM25 = "bm25"
    NONE = "none"


@dataclass
class RAGConfig:
    """RAG 設定（Studio UIで設定可能）."""

    collection: str = "default"
    chunk_strategy: ChunkStrategy = ChunkStrategy.RECURSIVE
    chunk_size: int = 1000
    chunk_overlap: int = 200
    retrieval_method: str = "semantic"
    reranker: RerankerType = RerankerType.BM25
    reranker_options: dict[str, Any] = field(default_factory=dict)
    top_k: int = 5
    min_similarity: float = 0.3

    # Studio UI での表示用メタデータ
    @classmethod
    def get_config_fields(cls) -> list[dict[str, Any]]:
        """Studio 設定フィールド定義."""
        return [
            {
                "name": "collection",
                "type": "string",
                "label": "コレクション名",
                "description": "ベクトルDBのコレクション名",
                "required": True,
            },
            {
                "name": "chunk_strategy",
                "type": "select",
                "label": "チャンキング戦略",
                "options": [e.value for e in ChunkStrategy],
                "default": "recursive",
            },
            {
                "name": "chunk_size",
                "type": "number",
                "label": "チャンクサイズ",
                "default": 1000,
                "min": 100,
                "max": 4000,
            },
            {
                "name": "retrieval_method",
                "type": "select",
                "label": "検索手法",
                "options": ["semantic", "keyword", "hybrid", "multi_query"],
                "default": "semantic",
            },
            {
                "name": "reranker",
                "type": "select",
                "label": "リランカー",
                "options": [e.value for e in RerankerType],
                "default": "bm25",
            },
            {
                "name": "top_k",
                "type": "number",
                "label": "上位K件",
                "default": 5,
                "min": 1,
                "max": 20,
            },
        ]


@dataclass
class RAGDocument:
    """RAG ドキュメント."""

    id: str
    content: str
    source: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)
    score: float = 0.0


@dataclass
class RAGResult:
    """RAG 結果."""

    documents: list[RAGDocument]
    answer: str = ""
    query: str = ""


# =============================================================================
# RAG Service 実装
# =============================================================================


class RAGService(ServiceBase[dict[str, Any]]):
    """RAG Service - 框架級可复用服務.

    Studio/CLI/SDK/API 全てで同一インターフェース。

    Actions:
    - query: 質問に回答（検索 + LLM生成）
    - search: 検索のみ
    - add_document: ドキュメント追加
    - add_file: ファイル追加
    """

    def __init__(self, config: RAGConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or RAGConfig()
        self._llm: Any = None
        self._embedding: Any = None
        self._vectordb: Any = None
        self._chunker: Any = None
        self._reranker: Any = None
        self._started = False

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return

        from infrastructure.llm.providers import get_embedding, get_llm, get_vectordb
        from shared.rag.chunking import ChunkConfig, get_chunker
        from shared.rag.reranker import get_reranker

        self._llm = get_llm(temperature=0.1)
        self._embedding = get_embedding()
        self._vectordb = get_vectordb(collection=self._config.collection)

        # connect_or_warn が使えるなら使う（グレースフルデグラデーション）
        connect_fn = getattr(self._vectordb, "connect_or_warn", None)
        if callable(connect_fn):
            vectordb_available = await connect_fn()
            if not vectordb_available:
                self._logger.warning(
                    "VectorDB 未接続のため RAGService は検索機能なしで起動します: collection=%s",
                    self._config.collection,
                )
        else:
            await self._vectordb.connect()

        self._chunker = get_chunker(
            self._config.chunk_strategy.value,
            ChunkConfig(
                chunk_size=self._config.chunk_size,
                chunk_overlap=self._config.chunk_overlap,
            ),
        )

        if self._config.reranker != RerankerType.NONE:
            self._reranker = get_reranker(
                self._config.reranker.value,
                options=self._config.reranker_options,
            )

        self._started = True
        self._logger.info(f"RAGService started: {self._config.collection}")

    async def stop(self) -> None:
        """サービス停止."""
        if self._vectordb:
            await self._vectordb.disconnect()
        self._started = False

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        action = kwargs.get("action", "query")

        if not self._started:
            await self.start()

        if action == "query":
            async for event in self._do_query(execution_id, **kwargs):
                yield event
        elif action == "search":
            async for event in self._do_search(execution_id, **kwargs):
                yield event
        elif action == "add_document":
            async for event in self._do_add_document(execution_id, **kwargs):
                yield event
        elif action == "add_file":
            async for event in self._do_add_file(execution_id, **kwargs):
                yield event
        else:
            yield self._emit_error(execution_id, "invalid_action", f"Unknown action: {action}")

    async def _do_query(
        self,
        execution_id: str,
        question: str = "",
        filters: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """検索 + LLM回答生成."""
        start_time = time.time()

        yield self._emit_progress(execution_id, 10, "ナレッジベースを検索中...", phase="search")

        documents = await self._search_internal(question, filters)

        yield self._emit_progress(execution_id, 50, f"{len(documents)}件のドキュメントを発見", phase="search")

        if not documents:
            yield self._emit_result(
                execution_id,
                {
                    "answer": "関連する情報が見つかりませんでした。",
                    "documents": [],
                    "query": question,
                },
                (time.time() - start_time) * 1000,
            )
            return

        yield self._emit_progress(execution_id, 60, "回答を生成中...", phase="generate")

        context = self._build_context(documents)
        answer = await self._generate_answer(question, context)

        yield self._emit_progress(execution_id, 100, "完了", phase="complete")

        yield self._emit_result(
            execution_id,
            {
                "answer": answer,
                "documents": [
                    {
                        "id": d.id,
                        "content": d.content[:500],
                        "source": d.source,
                        "score": d.score,
                        "metadata": d.metadata,
                    }
                    for d in documents
                ],
                "query": question,
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_search(
        self,
        execution_id: str,
        query: str = "",
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """検索のみ."""
        start_time = time.time()

        yield self._emit_progress(execution_id, 20, "検索中...", phase="search")

        documents = await self._search_internal(query, filters, top_k)

        yield self._emit_result(
            execution_id,
            {
                "documents": [
                    {
                        "id": d.id,
                        "content": d.content,
                        "source": d.source,
                        "score": d.score,
                        "metadata": d.metadata,
                    }
                    for d in documents
                ],
                "query": query,
                "count": len(documents),
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_add_document(
        self,
        execution_id: str,
        content: str = "",
        source: str = "",
        metadata: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """ドキュメント追加."""
        start_time = time.time()

        yield self._emit_progress(execution_id, 20, "ドキュメントをチャンク化中...", phase="chunk")

        chunks = await self._chunker.chunk(content, metadata)

        yield self._emit_progress(execution_id, 50, f"{len(chunks)}チャンクを埋め込み中...", phase="embed")

        contents = [c.content for c in chunks]
        ids = [c.id for c in chunks]
        embeddings = await self._embedding.embed_batch(contents)

        metadatas = [
            {"source": source, "chunk_index": c.index, **(c.metadata or {}), **(metadata or {})} for c in chunks
        ]

        yield self._emit_progress(execution_id, 80, "保存中...", phase="store")

        await self._vectordb.add_documents(
            documents=contents,
            ids=ids,
            embeddings=embeddings,
            metadatas=metadatas,
        )

        yield self._emit_result(
            execution_id,
            {
                "ids": ids,
                "count": len(ids),
                "source": source,
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_add_file(
        self,
        execution_id: str,
        file_path: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """ファイル追加."""
        from shared.rag.document_loader import UniversalLoader

        start_time = time.time()
        path = Path(file_path)

        yield self._emit_progress(execution_id, 10, f"{path.name}を読み込み中...", phase="load")

        loader = UniversalLoader()
        doc_chunks = await loader.load(path)

        yield self._emit_progress(execution_id, 30, f"{len(doc_chunks)}チャンクを処理中...", phase="process")

        all_ids = []
        total = len(doc_chunks)

        for i, chunk in enumerate(doc_chunks):
            progress = 30 + (60 * (i + 1) / total)
            yield self._emit_progress(execution_id, progress, f"チャンク {i + 1}/{total} を処理中...", phase="process")

            contents = [chunk.content]
            ids = [chunk.id]
            embeddings = await self._embedding.embed_batch(contents)

            await self._vectordb.add_documents(
                documents=contents,
                ids=ids,
                embeddings=embeddings,
                metadatas=[chunk.metadata],
            )
            all_ids.extend(ids)

        yield self._emit_result(
            execution_id,
            {
                "ids": all_ids,
                "count": len(all_ids),
                "file": str(path),
            },
            (time.time() - start_time) * 1000,
        )

    async def _search_internal(
        self,
        query: str,
        filters: dict[str, Any] | None = None,
        top_k: int | None = None,
    ) -> list[RAGDocument]:
        """内部検索ロジック."""
        top_k = top_k or self._config.top_k
        search_k = top_k
        if self._reranker:
            try:
                recommended_k = int(self._reranker.recommended_search_k(top_k))
                search_k = max(top_k, recommended_k)
            except Exception as exc:
                self._logger.warning("Failed to get recommended search_k from reranker: %s", exc)
                search_k = top_k * 3

        query_embedding = await self._embedding.embed_text(query)

        results = await self._vectordb.similarity_search(
            query=query,
            query_embedding=query_embedding,
            k=search_k,
            filter=filters,
        )

        documents = []
        for r in results:
            raw_score = r.get("score")
            if isinstance(raw_score, int | float):
                similarity = float(raw_score)
            else:
                similarity = 1.0 - float(r.get("distance", 1.0))
            if similarity >= self._config.min_similarity:
                documents.append(
                    RAGDocument(
                        id=r.get("id", ""),
                        content=r.get("document", ""),
                        source=r.get("metadata", {}).get("source", ""),
                        metadata=r.get("metadata", {}),
                        score=similarity,
                    )
                )

        if self._reranker and len(documents) > top_k:
            try:
                rerank_inputs = [
                    {
                        **d.metadata,
                        "id": d.id,
                        "content": d.content,
                        "source": d.source,
                    }
                    for d in documents
                ]
                ranked = await self._reranker.rerank(query, rerank_inputs, top_k)
                documents = [documents[r.original_index] for r in ranked if r.original_index < len(documents)]
                for i, ranked_doc in enumerate(ranked):
                    if i >= len(documents):
                        break
                    documents[i].score = ranked_doc.score
            except Exception as exc:
                self._logger.warning("Rerank failed, fallback to vector order: %s", exc)

        return documents[:top_k]

    def _build_context(self, documents: list[RAGDocument]) -> str:
        """コンテキスト構築."""
        parts = []
        for i, doc in enumerate(documents, 1):
            parts.append(f"[{i}] {doc.content}")
        return "\n\n".join(parts)

    async def _generate_answer(self, question: str, context: str) -> str:
        """LLM回答生成."""
        messages = [
            {
                "role": "system",
                "content": "提供されたコンテキストに基づいて回答してください。ソースを引用する際は[1]、[2]などの形式を使用してください。コンテキストに関連情報がない場合は、正直にその旨を伝えてください。",
            },
            {"role": "user", "content": f"参考情報:\n{context}\n\n質問: {question}"},
        ]
        response = await self._llm.chat(messages)
        content = response.get("content")
        return content if isinstance(content, str) else ""

    # =========================================================================
    # Studio 統合用メソッド
    # =========================================================================

    @classmethod
    def get_node_definition(cls) -> dict[str, Any]:
        """Studio ノード定義."""
        return {
            "type": "rag",
            "label": "RAG検索",
            "category": "knowledge",
            "icon": "search",
            "description": "ナレッジベースをRAGで検索",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問"},
                {"name": "filters", "type": "object", "label": "フィルター", "optional": True},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "documents", "type": "array", "label": "ドキュメント"},
            ],
            "config": RAGConfig.get_config_fields(),
        }


__all__ = [
    "ChunkStrategy",
    "RAGConfig",
    "RAGDocument",
    "RAGResult",
    "RAGService",
    "RerankerType",
]
