"""file_search MCP Tool - ファイルシステム検索.

指定ディレクトリ配下のファイルを全文検索する。
ベクトルDB を使わないシンプルなファイル検索ルート。

使用例:
    >>> tool = FileSearchTool(search_dirs=["/data/docs"])
    >>> response = await tool.execute(request)
"""

from __future__ import annotations

import logging

from apps.faq_system.backend.mcp.backends.base import RetrievalQuery
from apps.faq_system.backend.mcp.backends.file_system import FileSystemBackend
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline
from kernel.protocols.mcp_tool import MCPTool, MCPToolRequest, MCPToolResponse


logger = logging.getLogger(__name__)


class FileSearchTool(MCPTool):
    """ファイルシステム検索 MCP ツール.

    Input:
        - query: 検索クエリ（必須）
        - top_k: 上位K件（デフォルト: 5）
        - search_dirs: 検索ディレクトリ一覧（オプション、動的追加用）

    Output:
        - documents: 検索結果ドキュメントリスト
        - total_found: 合計件数
        - query: 実行クエリ
    """

    def __init__(
        self,
        search_dirs: list[str] | None = None,
        max_file_size_bytes: int = 1_000_000,
    ) -> None:
        """初期化.

        Args:
            search_dirs: デフォルト検索ディレクトリ一覧
            max_file_size_bytes: 読み込み上限ファイルサイズ
        """
        super().__init__(tool_name="file_search", version="1.0.0")
        self._search_dirs = search_dirs or []
        self._max_file_size = max_file_size_bytes
        self._pipeline: RetrievalPipeline | None = None

    async def _ensure_pipeline(self) -> RetrievalPipeline:
        """パイプラインを遅延初期化."""
        if self._pipeline is None:
            backend = FileSystemBackend(
                search_dirs=self._search_dirs,
                max_file_size_bytes=self._max_file_size,
            )
            self._pipeline = RetrievalPipeline(name="file_search")
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

        top_k = int(input_data.get("top_k", 5))
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
                },
                metadata=result.metadata,
            )
        except Exception as e:
            logger.exception("file_search エラー")
            return MCPToolResponse(
                success=False,
                errors=[str(e)],
            )

    async def cleanup(self) -> None:
        """クリーンアップ."""
        if self._pipeline is not None:
            await self._pipeline.cleanup_all()
