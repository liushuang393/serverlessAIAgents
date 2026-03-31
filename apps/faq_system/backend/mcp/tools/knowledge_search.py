"""knowledge_search MCP Tool - 伝統的RAG検索.

ベクトルDB を用いた検索 + リランク + LLM 回答生成。
従来の RAGService をラップした統一ツール。
ドキュメントグループ関連展開によるコンテキスト拡張をサポート。

使用例:
    >>> tool = KnowledgeSearchTool(collection="faq_knowledge")
    >>> response = await tool.execute(request)
"""

from __future__ import annotations

import logging
from typing import Any

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery, RetrievedDocument
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline
from kernel.protocols.mcp_tool import MCPTool, MCPToolRequest, MCPToolResponse


logger = logging.getLogger(__name__)

# 関連ドキュメント展開の上限
_MAX_GROUP_EXPANSION_DOCS = 3


class KnowledgeSearchTool(MCPTool):
    """伝統的RAG検索 MCP ツール.

    Input:
        - query: 検索クエリ（必須）
        - top_k: 上位K件（デフォルト: 5）
        - collection: コレクション名（デフォルト: 設定値）
        - filters: メタデータフィルタ
        - expand_related: 関連ドキュメント展開を有効にするか（デフォルト: True）

    Output:
        - documents: 検索結果ドキュメントリスト
        - related_documents: 関連展開されたドキュメントリスト
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
        expand_related = bool(input_data.get("expand_related", True))

        try:
            pipeline = await self._ensure_pipeline(collection)
            query = RetrievalQuery(
                query=query_text,
                top_k=top_k,
                filters=filters if isinstance(filters, dict) else {},
            )
            result = await pipeline.execute(query)

            doc_dicts = [
                {
                    "doc_id": doc.doc_id,
                    "content": doc.content,
                    "score": doc.score,
                    "source": doc.source,
                    "metadata": doc.metadata,
                }
                for doc in result.documents
            ]

            # 関連ドキュメント展開
            related_dicts: list[dict[str, Any]] = []
            if expand_related and result.documents:
                related_docs = await self._expand_related_documents(
                    result.documents, pipeline, query_text, top_k,
                )
                related_dicts = [
                    {
                        "doc_id": doc.doc_id,
                        "content": doc.content,
                        "score": doc.score,
                        "source": doc.source,
                        "metadata": doc.metadata,
                        "relation": "group_expansion",
                    }
                    for doc in related_docs
                ]

            return MCPToolResponse(
                success=True,
                output={
                    "documents": doc_dicts,
                    "related_documents": related_dicts,
                    "total_found": result.total_found + len(related_dicts),
                    "query": result.query,
                },
                metadata={
                    **result.metadata,
                    "expanded_related": len(related_dicts),
                },
            )
        except Exception as e:
            logger.exception("knowledge_search エラー")
            return MCPToolResponse(
                success=False,
                errors=[str(e)],
            )

    async def _expand_related_documents(
        self,
        primary_docs: list[RetrievedDocument],
        pipeline: RetrievalPipeline,
        query_text: str,
        primary_top_k: int,
    ) -> list[RetrievedDocument]:
        """プライマリ検索結果からグループ関連ドキュメントを追加取得.

        同じ document_group_id を持つドキュメントのチャンクを追加で検索する。
        プライマリ結果に既に含まれるドキュメントは除外する。

        Args:
            primary_docs: プライマリ検索結果
            pipeline: 検索パイプライン
            query_text: 検索クエリ
            primary_top_k: プライマリの top_k

        Returns:
            追加の関連ドキュメントリスト
        """
        # プライマリ結果からグループ ID を収集
        group_ids: set[str] = set()
        primary_doc_ids: set[str] = set()

        for doc in primary_docs:
            primary_doc_ids.add(doc.doc_id)
            gid = doc.metadata.get("document_group_id")
            if gid and isinstance(gid, str):
                group_ids.add(gid)

        if not group_ids:
            return []

        # 各グループ ID で追加検索
        related: list[RetrievedDocument] = []
        for gid in group_ids:
            try:
                group_query = RetrievalQuery(
                    query=query_text,
                    top_k=_MAX_GROUP_EXPANSION_DOCS + primary_top_k,
                    filters={"document_group_id": gid},
                )
                group_result = await pipeline.execute(group_query)
                for doc in group_result.documents:
                    if doc.doc_id not in primary_doc_ids and len(related) < _MAX_GROUP_EXPANSION_DOCS:
                        doc.metadata["expanded_from_group"] = gid
                        related.append(doc)
                        primary_doc_ids.add(doc.doc_id)
            except Exception:
                logger.debug("グループ展開失敗: group_id=%s", gid, exc_info=True)
                continue

        return related

    async def cleanup(self) -> None:
        """クリーンアップ."""
        if self._pipeline is not None:
            await self._pipeline.cleanup_all()
