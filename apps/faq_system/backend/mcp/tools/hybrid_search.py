"""hybrid_search MCP Tool - ハイブリッド検索.

VectorStore + FileSystem の両方を使ったハイブリッド検索。
シナリオに応じて単体・組み合わせを自動選択。

使用例:
    >>> tool = HybridSearchTool(collection="faq_knowledge", search_dirs=["/data"])
    >>> response = await tool.execute(request)
"""

from __future__ import annotations

import logging
from typing import Any

from kernel.protocols.mcp_tool import MCPTool, MCPToolRequest, MCPToolResponse

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery
from apps.faq_system.backend.mcp.backends.file_system import FileSystemBackend
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline


logger = logging.getLogger(__name__)


class HybridSearchTool(MCPTool):
    """ハイブリッド検索 MCP ツール.

    VectorStore と FileSystem の両方から検索し、
    結果をスコア順でマージして返す。

    Input:
        - query: 検索クエリ（必須）
        - top_k: 上位K件（デフォルト: 5）
        - collection: コレクション名
        - search_dirs: 追加検索ディレクトリ
        - mode: "auto" | "vector_only" | "file_only" | "both"

    Output:
        - documents: 検索結果ドキュメントリスト
        - total_found: 合計件数
        - query: 実行クエリ
        - backends_used: 使用されたバックエンド一覧
    """

    def __init__(
        self,
        collection: str = "faq_knowledge",
        chunk_strategy: str = "recursive",
        reranker: str = "bm25",
        search_dirs: list[str] | None = None,
        top_k: int = 5,
    ) -> None:
        """初期化.

        Args:
            collection: ベクトルDBコレクション名
            chunk_strategy: チャンキング戦略
            reranker: リランカー種別
            search_dirs: 検索ディレクトリ一覧
            top_k: デフォルト上位K件
        """
        super().__init__(tool_name="hybrid_search", version="1.0.0")
        self._collection = collection
        self._chunk_strategy = chunk_strategy
        self._reranker = reranker
        self._search_dirs = search_dirs or []
        self._default_top_k = top_k
        self._pipeline: RetrievalPipeline | None = None

    async def _ensure_pipeline(self) -> RetrievalPipeline:
        """パイプラインを遅延初期化."""
        if self._pipeline is None:
            self._pipeline = RetrievalPipeline(name="hybrid_search")

            # VectorStore バックエンド（優先度高）
            vector_backend = VectorStoreBackend(
                collection=self._collection,
                chunk_strategy=self._chunk_strategy,
                reranker=self._reranker,
                top_k=self._default_top_k,
            )
            self._pipeline.add_backend(vector_backend, priority=0)

            # FileSystem バックエンド（優先度低）
            if self._search_dirs:
                file_backend = FileSystemBackend(search_dirs=self._search_dirs)
                self._pipeline.add_backend(file_backend, priority=1)

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
            return MCPToolResponse(success=False, errors=["query パラメータは必須です"])

        top_k = int(input_data.get("top_k", self._default_top_k))
        extra_dirs = input_data.get("search_dirs", [])

        try:
            pipeline = await self._ensure_pipeline()
            query = RetrievalQuery(
                query=query_text,
                top_k=top_k,
                options={"search_dirs": extra_dirs} if extra_dirs else {},
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
                    "backends_used": result.metadata.get("backends_used", []),
                },
                metadata=result.metadata,
            )
        except Exception as e:
            logger.exception("hybrid_search エラー")
            return MCPToolResponse(success=False, errors=[str(e)])

    async def cleanup(self) -> None:
        """クリーンアップ."""
        if self._pipeline is not None:
            await self._pipeline.cleanup_all()

