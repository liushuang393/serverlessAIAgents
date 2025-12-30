# -*- coding: utf-8 -*-
"""MCP Client for Code Migration Assistant.

このモジュールはMCP工具を呼び出すためのクライアントを提供します。

主な機能:
    - MCP工具の登録と管理
    - MCP工具の呼び出し
    - エラーハンドリング
"""

from typing import Any

from apps.code_migration_assistant.mcp_tools.base import MCPTool, MCPToolRequest, MCPToolResponse


class MCPClient:
    """MCP Client.

    MCP工具を呼び出すためのクライアント。

    使用例:
        ```python
        client = MCPClient()
        client.register_tool("cobol_parser", COBOLParser())
        client.register_tool("java_generator", JavaGenerator())

        request = MCPToolRequest(
            tool="cobol_parser",
            version="1.0.0",
            input={"cobol_code": "..."},
        )

        response = await client.call_tool(request)
        ```
    """

    def __init__(self) -> None:
        """MCPClientを初期化."""
        self.tools: dict[str, MCPTool] = {}

    def register_tool(self, tool_name: str, tool: MCPTool) -> None:
        """MCP工具を登録.

        Args:
            tool_name: 工具名
            tool: MCP工具インスタンス
        """
        self.tools[tool_name] = tool

    def unregister_tool(self, tool_name: str) -> None:
        """MCP工具を登録解除.

        Args:
            tool_name: 工具名
        """
        if tool_name in self.tools:
            del self.tools[tool_name]

    def list_tools(self) -> list[str]:
        """登録されているMCP工具のリストを取得.

        Returns:
            工具名リスト
        """
        return list(self.tools.keys())

    async def call_tool(self, request: MCPToolRequest) -> MCPToolResponse:
        """MCP工具を呼び出す.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス
        """
        tool_name = request.tool

        # 工具が登録されているかチェック
        if tool_name not in self.tools:
            return MCPToolResponse(
                success=False,
                errors=[f"Tool '{tool_name}' is not registered"],
            )

        # 工具を取得
        tool = self.tools[tool_name]

        # 工具を実行
        try:
            response = await tool.execute(request)
            return response

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Tool execution failed: {str(e)}"],
            )

    async def call_tool_by_name(
        self,
        tool_name: str,
        input_data: dict[str, Any],
        version: str = "1.0.0",
    ) -> MCPToolResponse:
        """工具名で MCP工具を呼び出す（簡易版）.

        Args:
            tool_name: 工具名
            input_data: 入力データ
            version: 工具バージョン

        Returns:
            MCP工具レスポンス
        """
        request = MCPToolRequest(
            tool=tool_name,
            version=version,
            input=input_data,
        )

        return await self.call_tool(request)

    def get_tool(self, tool_name: str) -> MCPTool | None:
        """MCP工具を取得.

        Args:
            tool_name: 工具名

        Returns:
            MCP工具インスタンス（存在しない場合はNone）
        """
        return self.tools.get(tool_name)

    def has_tool(self, tool_name: str) -> bool:
        """MCP工具が登録されているかチェック.

        Args:
            tool_name: 工具名

        Returns:
            登録されている場合True
        """
        return tool_name in self.tools

