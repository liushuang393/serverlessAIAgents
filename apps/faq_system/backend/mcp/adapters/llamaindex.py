"""LlamaIndex アダプター - LlamaIndex を RetrievalBackend として統合.

LlamaIndex の VectorStoreIndex / KnowledgeGraphIndex 等を
統一 RetrievalBackend インターフェースでラップ。

前提条件:
    pip install llama-index llama-index-vector-stores-chroma

使用例:
    >>> adapter = LlamaIndexAdapter(
    ...     index_type="vector",
    ...     persist_dir="/data/llama_index",
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


class LlamaIndexAdapter(ThirdPartyAdapter):
    """LlamaIndex アダプター.

    LlamaIndex の Index → QueryEngine をラップし、
    RetrievalBackend として動作させる。

    サポートする index_type:
        - "vector": VectorStoreIndex（デフォルト）
        - "summary": SummaryIndex
        - "keyword": KeywordTableIndex
    """

    def __init__(
        self,
        index_type: str = "vector",
        persist_dir: str | None = None,
        chunk_size: int = 512,
        chunk_overlap: int = 50,
        similarity_top_k: int = 5,
        embed_model: str = "default",
    ) -> None:
        """初期化.

        Args:
            index_type: インデックス種別（vector / summary / keyword）
            persist_dir: インデックスの永続化ディレクトリ
            chunk_size: チャンクサイズ
            chunk_overlap: チャンクオーバーラップ
            similarity_top_k: 類似度上位K件
            embed_model: 埋め込みモデル名
        """
        super().__init__(
            framework_name="llamaindex",
            framework_config={
                "index_type": index_type,
                "persist_dir": persist_dir,
                "chunk_size": chunk_size,
                "chunk_overlap": chunk_overlap,
                "similarity_top_k": similarity_top_k,
                "embed_model": embed_model,
            },
        )
        self._query_engine: Any = None  # 理由: llama_index 型は動的 import

    async def _create_framework_instance(self) -> Any:  # 理由: llama_index 型は動的
        """LlamaIndex インデックスとクエリエンジンを生成.

        Returns:
            LlamaIndex QueryEngine インスタンス
        """
        try:
            from llama_index.core import Settings, StorageContext, VectorStoreIndex, load_index_from_storage

            persist_dir = self._framework_config.get("persist_dir")
            chunk_size = self._framework_config.get("chunk_size", 512)

            Settings.chunk_size = chunk_size
            Settings.chunk_overlap = self._framework_config.get("chunk_overlap", 50)

            if persist_dir:
                # 既存インデックスをロード
                storage_context = StorageContext.from_defaults(persist_dir=persist_dir)
                index = load_index_from_storage(storage_context)
            else:
                # 空のインデックスを生成（後からドキュメント追加可能）
                index = VectorStoreIndex([])

            similarity_top_k = self._framework_config.get("similarity_top_k", 5)
            self._query_engine = index.as_query_engine(similarity_top_k=similarity_top_k)
            return index

        except ImportError:
            self._logger.error(
                "llama-index 未インストール。pip install llama-index を実行してください"
            )
            raise
        except Exception:
            self._logger.exception("LlamaIndex インデックス生成失敗")
            raise

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """LlamaIndex 経由で検索を実行.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果
        """
        if not self._initialized:
            await self.initialize()

        if self._query_engine is None:
            return RetrievalResult(
                query=query.query,
                metadata={"error": "LlamaIndex QueryEngine 未初期化"},
            )

        try:
            # LlamaIndex のクエリエンジンで検索
            response = self._query_engine.query(query.query)

            documents: list[RetrievedDocument] = []

            # ソースノードを RetrievedDocument に変換
            source_nodes = getattr(response, "source_nodes", [])
            for i, node in enumerate(source_nodes[: query.top_k]):
                score = getattr(node, "score", 0.0) or 0.0
                node_content = getattr(node, "text", "") or str(getattr(node, "node", ""))
                node_id = getattr(node, "node_id", "") or uuid.uuid4().hex[:8]
                metadata = getattr(node, "metadata", {}) or {}

                documents.append(
                    RetrievedDocument(
                        doc_id=str(node_id),
                        content=node_content,
                        score=float(score),
                        source="llamaindex",
                        metadata=metadata,
                    )
                )

            return RetrievalResult(
                documents=documents,
                query=query.query,
                total_found=len(documents),
                metadata={"framework": "llamaindex", "response_text": str(response)},
            )
        except Exception as e:
            self._logger.exception("LlamaIndex 検索エラー")
            return RetrievalResult(
                query=query.query,
                metadata={"error": str(e)},
            )

