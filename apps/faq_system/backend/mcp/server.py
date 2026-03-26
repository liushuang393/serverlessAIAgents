"""FAQ MCP Server - 検索ツールのディスパッチ・権限分離.

Agent は MCP Server を通じて検索ツールを呼び出す。
MCP Server はツール登録・権限チェック・ログ記録を担当。

使用例:
    >>> server = FAQMCPServer.create_default(collection="faq_knowledge")
    >>> result = await server.call_tool("knowledge_search", {"query": "返品ポリシー"})
"""

from __future__ import annotations

import logging
from typing import Any

from apps.faq_system.backend.mcp.tools.file_search import FileSearchTool
from apps.faq_system.backend.mcp.tools.hybrid_search import HybridSearchTool
from apps.faq_system.backend.mcp.tools.knowledge_search import KnowledgeSearchTool
from kernel.protocols.mcp_tool import MCPTool, MCPToolRequest, MCPToolResponse


logger = logging.getLogger(__name__)


class FAQMCPServer:
    """FAQ MCP Server.

    検索ツールの登録・ディスパッチ・権限分離を行う。
    Agent はこのサーバーを通じて検索を実行する。
    """

    def __init__(self) -> None:
        """初期化."""
        self._tools: dict[str, MCPTool] = {}
        self._logger = logging.getLogger(__name__)

    def register_tool(self, tool: MCPTool) -> None:
        """ツールを登録.

        Args:
            tool: MCP ツール
        """
        self._tools[tool.tool_name] = tool
        self._logger.info("MCP ツール登録: %s (v%s)", tool.tool_name, tool.version)

    def list_tools(self) -> list[dict[str, str]]:
        """登録済みツール一覧を返す.

        Returns:
            ツール名とバージョンの一覧
        """
        return [{"name": t.tool_name, "version": t.version} for t in self._tools.values()]

    async def call_tool(
        self,
        tool_name: str,
        input_data: dict[str, Any],
    ) -> MCPToolResponse:
        """ツールを呼び出す.

        Args:
            tool_name: ツール名
            input_data: 入力データ

        Returns:
            MCP ツールレスポンス
        """
        tool = self._tools.get(tool_name)
        if tool is None:
            return MCPToolResponse(
                success=False,
                errors=[f"ツール '{tool_name}' が見つかりません。登録済み: {list(self._tools.keys())}"],
            )

        request = MCPToolRequest(
            tool=tool_name,
            input=input_data,
        )

        self._logger.debug("MCP ツール呼び出し: %s", tool_name)
        return await tool.execute(request)

    async def cleanup(self) -> None:
        """全ツールをクリーンアップ."""
        for tool in self._tools.values():
            cleanup_fn = getattr(tool, "cleanup", None)
            if callable(cleanup_fn):
                await cleanup_fn()

    @classmethod
    def create_default(
        cls,
        collection: str = "faq_knowledge",
        chunk_strategy: str = "recursive",
        reranker: str = "bm25",
        top_k: int = 5,
        search_dirs: list[str] | None = None,
    ) -> FAQMCPServer:
        """デフォルト構成で MCP Server を生成.

        Args:
            collection: ベクトルDBコレクション名
            chunk_strategy: チャンキング戦略
            reranker: リランカー種別
            top_k: デフォルト上位K件
            search_dirs: 検索ディレクトリ一覧

        Returns:
            設定済み FAQMCPServer
        """
        server = cls()

        # 伝統的RAG検索ツール
        server.register_tool(
            KnowledgeSearchTool(
                collection=collection,
                chunk_strategy=chunk_strategy,
                reranker=reranker,
                top_k=top_k,
            )
        )

        # ファイルシステム検索ツール
        server.register_tool(
            FileSearchTool(
                search_dirs=search_dirs or [],
            )
        )

        # ハイブリッド検索ツール
        server.register_tool(
            HybridSearchTool(
                collection=collection,
                chunk_strategy=chunk_strategy,
                reranker=reranker,
                search_dirs=search_dirs or [],
                top_k=top_k,
            )
        )

        return server
