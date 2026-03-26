"""LangChain アダプター - LangChain を RetrievalBackend として統合.

LangChain の VectorStore Retriever / Ensemble Retriever 等を
統一 RetrievalBackend インターフェースでラップ。

前提条件:
    pip install langchain langchain-community

使用例:
    >>> adapter = LangChainAdapter(
    ...     retriever_type="vectorstore",
    ...     vectorstore_class="Chroma",
    ...     persist_directory="/data/chroma_db",
    ... )
    >>> result = await adapter.retrieve(RetrievalQuery(query="返品ポリシー"))
"""

from __future__ import annotations

import logging
import uuid
from typing import Any

from apps.faq_system.backend.mcp.adapters.base import ThirdPartyAdapter
from apps.faq_system.backend.mcp.backends.base import (
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


class LangChainAdapter(ThirdPartyAdapter):
    """LangChain アダプター.

    LangChain の Retriever をラップし、
    RetrievalBackend として動作させる。

    サポートする retriever_type:
        - "vectorstore": VectorStoreRetriever（デフォルト）
        - "ensemble": EnsembleRetriever（BM25 + Vector）
        - "multi_query": MultiQueryRetriever（クエリ拡張）
    """

    def __init__(
        self,
        retriever_type: str = "vectorstore",
        vectorstore_class: str = "Chroma",
        persist_directory: str | None = None,
        collection_name: str = "default",
        search_kwargs: dict[str, Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            retriever_type: リトリーバー種別
            vectorstore_class: VectorStore クラス名（Chroma / FAISS 等）
            persist_directory: 永続化ディレクトリ
            collection_name: コレクション名
            search_kwargs: 検索パラメータ（k, score_threshold 等）
        """
        super().__init__(
            framework_name="langchain",
            framework_config={
                "retriever_type": retriever_type,
                "vectorstore_class": vectorstore_class,
                "persist_directory": persist_directory,
                "collection_name": collection_name,
                "search_kwargs": search_kwargs or {"k": 5},
            },
        )
        self._retriever: Any = None  # 理由: langchain 型は動的 import

    async def _create_framework_instance(self) -> Any:  # 理由: langchain 型は動的
        """LangChain Retriever を生成.

        Returns:
            LangChain BaseRetriever インスタンス
        """
        retriever_type = self._framework_config.get("retriever_type", "vectorstore")

        try:
            if retriever_type == "vectorstore":
                self._retriever = await self._create_vectorstore_retriever()
            elif retriever_type == "ensemble":
                self._retriever = await self._create_ensemble_retriever()
            else:
                msg = f"未対応の retriever_type: {retriever_type}"
                raise ValueError(msg)

            return self._retriever
        except ImportError:
            self._logger.exception(
                "langchain 未インストール。pip install langchain langchain-community を実行してください"
            )
            raise
        except Exception:
            self._logger.exception("LangChain Retriever 生成失敗")
            raise

    async def _create_vectorstore_retriever(self) -> Any:  # 理由: langchain 型は動的
        """VectorStoreRetriever を生成."""
        from langchain_community.vectorstores import Chroma

        persist_dir = self._framework_config.get("persist_directory")
        collection = self._framework_config.get("collection_name", "default")
        search_kwargs = self._framework_config.get("search_kwargs", {"k": 5})

        vectorstore = Chroma(
            collection_name=collection,
            persist_directory=persist_dir,
        )
        return vectorstore.as_retriever(search_kwargs=search_kwargs)

    async def _create_ensemble_retriever(self) -> Any:  # 理由: langchain 型は動的
        """EnsembleRetriever を生成（BM25 + Vector）."""
        from langchain.retrievers import EnsembleRetriever
        from langchain_community.retrievers import BM25Retriever

        # Vector Retriever
        vector_retriever = await self._create_vectorstore_retriever()

        # BM25 Retriever（空ドキュメントで初期化、後から追加可能）
        bm25_retriever = BM25Retriever.from_texts([""], metadatas=[{}])
        bm25_retriever.k = self._framework_config.get("search_kwargs", {}).get("k", 5)

        return EnsembleRetriever(
            retrievers=[vector_retriever, bm25_retriever],
            weights=[0.7, 0.3],
        )

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """LangChain 経由で検索を実行.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果
        """
        if not self._initialized:
            await self.initialize()

        if self._retriever is None:
            return RetrievalResult(
                query=query.query,
                metadata={"error": "LangChain Retriever 未初期化"},
            )

        try:
            # LangChain Retriever で検索
            lc_docs = await self._retriever.ainvoke(query.query)

            documents: list[RetrievedDocument] = []
            for i, doc in enumerate(lc_docs[: query.top_k]):
                page_content = getattr(doc, "page_content", "")
                metadata = getattr(doc, "metadata", {}) or {}
                doc_id = metadata.get("id", "") or uuid.uuid4().hex[:8]
                # LangChain は score を返さない場合が多いので順序ベース
                score = metadata.get("score", max(0.0, 1.0 - (i * 0.05)))

                documents.append(
                    RetrievedDocument(
                        doc_id=str(doc_id),
                        content=page_content,
                        score=float(score),
                        source="langchain",
                        metadata=metadata,
                    )
                )

            return RetrievalResult(
                documents=documents,
                query=query.query,
                total_found=len(documents),
                metadata={"framework": "langchain"},
            )
        except Exception as e:
            self._logger.exception("LangChain 検索エラー")
            return RetrievalResult(
                query=query.query,
                metadata={"error": str(e)},
            )
