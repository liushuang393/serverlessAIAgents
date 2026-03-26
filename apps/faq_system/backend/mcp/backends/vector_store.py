"""VectorStoreBackend - 既存 RAGService をラップするバックエンド.

shared/services/rag_service.py の RAGService を内部で利用し、
統一 RetrievalBackend インターフェースを提供。

使用例:
    >>> backend = VectorStoreBackend(collection="faq_knowledge")
    >>> result = await backend.retrieve(RetrievalQuery(query="返品ポリシー"))
"""

from __future__ import annotations

import logging
import uuid
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


class VectorStoreBackend(RetrievalBackend):
    """VectorStore バックエンド（既存 RAGService 統合）.

    shared/services/rag_service.py を内部で利用。
    Agent は直接 RAGService を呼ばず、このバックエンドを通じてアクセス。
    """

    def __init__(
        self,
        collection: str = "default",
        chunk_strategy: str = "recursive",
        reranker: str = "bm25",
        top_k: int = 5,
    ) -> None:
        """初期化.

        Args:
            collection: ベクトルDBコレクション名
            chunk_strategy: チャンキング戦略
            reranker: リランカー種別
            top_k: デフォルト上位K件
        """
        super().__init__(backend_type=BackendType.VECTOR_STORE, name=f"vector:{collection}")
        self._collection = collection
        self._chunk_strategy = chunk_strategy
        self._reranker = reranker
        self._default_top_k = top_k
        self._rag_service: Any = None  # 遅延初期化（RAGService 型は動的 import）
        self._started = False

    async def initialize(self) -> None:
        """RAGService を初期化."""
        if self._started:
            return
        try:
            from shared.services.rag_service import ChunkStrategy, RAGConfig, RAGService, RerankerType

            chunk_strategy = ChunkStrategy(self._chunk_strategy)
            reranker = RerankerType(self._reranker)
            config = RAGConfig(
                collection=self._collection,
                chunk_strategy=chunk_strategy,
                reranker=reranker,
                top_k=self._default_top_k,
            )
            self._rag_service = RAGService(config)
            await self._rag_service.start()
            self._started = True
            self._logger.info("VectorStoreBackend 初期化完了: collection=%s", self._collection)
        except Exception:
            self._logger.exception("VectorStoreBackend 初期化失敗")
            raise

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """ベクトル検索を実行.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果
        """
        if not self._started:
            await self.initialize()

        if self._rag_service is None:
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.VECTOR_STORE,
                metadata={"error": "RAGService 未初期化"},
            )

        # RAGService の search アクションを呼び出し
        top_k = query.options.get("top_k", query.top_k)
        rag_result = await self._rag_service.execute(
            action="search",
            question=query.query,
            top_k=top_k,
        )

        if not rag_result.success:
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.VECTOR_STORE,
                metadata={"error": rag_result.error_message or "検索失敗"},
            )

        # RAGService の結果を統一型に変換
        documents = self._convert_rag_documents(rag_result.data)
        return RetrievalResult(
            documents=documents,
            query=query.query,
            total_found=len(documents),
            backend_type=BackendType.VECTOR_STORE,
            metadata={"collection": self._collection},
        )

    async def health_check(self) -> bool:
        """ヘルスチェック."""
        return self._started and self._rag_service is not None

    async def cleanup(self) -> None:
        """クリーンアップ."""
        if self._rag_service is not None and self._started:
            await self._rag_service.stop()
            self._started = False

    def _convert_rag_documents(self, data: dict[str, Any] | None) -> list[RetrievedDocument]:
        """RAGService の結果を RetrievedDocument に変換."""
        if data is None:
            return []
        docs_data = data.get("documents", [])
        documents: list[RetrievedDocument] = []
        for doc in docs_data:
            if not isinstance(doc, dict):
                continue
            documents.append(
                RetrievedDocument(
                    doc_id=str(doc.get("id", uuid.uuid4().hex[:8])),
                    content=str(doc.get("content", "")),
                    score=float(doc.get("score", 0.0)),
                    source=str(doc.get("source", "")),
                    metadata=doc.get("metadata", {}),
                )
            )
        return documents
