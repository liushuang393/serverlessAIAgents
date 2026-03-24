"""knowledge_search MCP Tool - 伝統的RAG検索.

ベクトルDB を用いた検索 + リランク + LLM 回答生成。
従来の RAGService をラップした統一ツール。

使用例:
    >>> tool = KnowledgeSearchTool(collection="faq_knowledge")
    >>> response = await tool.execute(request)
"""

from __future__ import annotations

import logging
from typing import Any

from kernel.protocols.mcp_tool import MCPTool, MCPToolRequest, MCPToolResponse

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline


logger = logging.getLogger(__name__)


class KnowledgeSearchTool(MCPTool):
    """伝統的RAG検索 MCP ツール.

    Input:
        - query: 検索クエリ（必須）
        - top_k: 上位K件（デフォルト: 5）
        - collection: コレクション名（デフォルト: 設定値）
        - filters: メタデータフィルタ

    Output:
        - documents: 検索結果ドキュメントリスト
        - total_found: 合計件数
        - query: 実行クエリ
    """

    def __init__(
        self,
        collection: str = "faq_knowledge",
        chunk_strategy: str = "recursive",
        reranker: str = "bm25",
        top_k: int = 5,
    ) -> None:
        """初期化.

        Args:
            collection: デフォルトコレクション名
            chunk_strategy: チャンキング戦略
            reranker: リランカー種別
            top_k: デフォルト上位K件
        """
        super().__init__(tool_name="knowledge_search", version="1.0.0")
        self._default_collection = collection
        self._chunk_strategy = chunk_strategy
        self._reranker = reranker
        self._default_top_k = top_k
        self._pipeline: RetrievalPipeline | None = None

    async def _ensure_pipeline(self, collection: str | None = None) -> RetrievalPipeline:
        """パイプラインを遅延初期化."""
        col = collection or self._default_collection
        if self._pipeline is None:
            backend = VectorStoreBackend(
                collection=col,
                chunk_strategy=self._chunk_strategy,
                reranker=self._reranker,
                top_k=self._default_top_k,
            )
            self._pipeline = RetrievalPipeline(name="knowledge_search")
            self._pipeline.add_backend(backend, priority=0)
            await self._pipeline.initialize_all()
        return self._pipeline

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """MCP リクエストを処理.

        Args:
            request: MCP ツールリクエスト

        Returns:
            MCP ツールレスポンス
        """
        input_data = request.input
        query_text = input_data.get("query", "")
        if not query_text:
            return MCPToolResponse(
                success=False,
                errors=["query パラメータは必須です"],
            )

        top_k = int(input_data.get("top_k", self._default_top_k))
        collection = input_data.get("collection")
        filters = input_data.get("filters", {})

        try:
            pipeline = await self._ensure_pipeline(collection)
            query = RetrievalQuery(
                query=query_text,
                top_k=top_k,
                filters=filters if isinstance(filters, dict) else {},
            )
            result = await pipeline.execute(query)

            return MCPToolResponse(
                success=True,
                output={
                    "documents": [
                        {
                            "doc_id": doc.doc_id,
                            "content": doc.content,
                            "score": doc.score,
                            "source": doc.source,
                            "metadata": doc.metadata,
                        }
                        for doc in result.documents
                    ],
                    "total_found": result.total_found,
                    "query": result.query,
                },
                metadata=result.metadata,
            )
        except Exception as e:
            logger.exception("knowledge_search エラー")
            return MCPToolResponse(
                success=False,
                errors=[str(e)],
            )

    async def cleanup(self) -> None:
        """クリーンアップ."""
        if self._pipeline is not None:
            await self._pipeline.cleanup_all()

