"""MCP Tool 基底クラスとクライアント.

このモジュールは MCP (Model Context Protocol) 工具の基底クラスと
クライアントを提供します。
code_migration_assistant の mcp_tools から汎用化したものです。

使用例:
    >>> from agentflow.protocols.mcp_tool import MCPTool, MCPToolClient
    >>>
    >>> # 工具を定義
    >>> class MyTool(MCPTool):
    ...     async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
    ...         # 実装
    ...         return MCPToolResponse(success=True, output={...})
    >>>
    >>> # クライアントを作成
    >>> client = MCPToolClient()
    >>> client.register_tool("my_tool", MyTool("my_tool"))
    >>> response = await client.call_tool_by_name("my_tool", {"input": "..."})
"""

from abc import ABC, abstractmethod
from typing import Any

from pydantic import BaseModel, Field


class MCPToolRequest(BaseModel):
    """MCP 工具リクエスト.

    Attributes:
        tool: 工具名称
        version: 工具バージョン
        input: 工具特定の入力パラメータ
    """

    tool: str = Field(..., description="工具名称")
    version: str = Field(default="1.0.0", description="工具バージョン")
    input: dict[str, Any] = Field(..., description="工具特定の入力パラメータ")


class MCPToolResponse(BaseModel):
    """MCP 工具レスポンス.

    Attributes:
        success: 実行成功フラグ
        output: 工具特定の出力データ
        metadata: 実行メタデータ（実行時間、工具バージョンなど）
        errors: エラーリスト
    """

    success: bool = Field(..., description="実行成功フラグ")
    output: dict[str, Any] | None = Field(default=None, description="工具特定の出力データ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="実行メタデータ")
    errors: list[str] = Field(default_factory=list, description="エラーリスト")


class MCPTool(ABC):
    """MCP 工具基底クラス.

    全ての MCP 工具はこのクラスを継承し、handle_request メソッドを実装する必要があります。

    Example:
        >>> class MyTool(MCPTool):
        ...     async def handle_request(
        ...         self, request: MCPToolRequest
        ...     ) -> MCPToolResponse:
        ...         # 実装
        ...         return MCPToolResponse(success=True, output={...})
    """

    def __init__(self, tool_name: str, version: str = "1.0.0") -> None:
        """MCP 工具を初期化.

        Args:
            tool_name: 工具名称
            version: 工具バージョン
        """
        self.tool_name = tool_name
        self.version = version

    @abstractmethod
    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """MCP 工具リクエストを処理.

        Args:
            request: MCP 工具リクエスト

        Returns:
            MCP 工具レスポンス

        Raises:
            NotImplementedError: サブクラスで実装する必要があります
        """
        raise NotImplementedError

    def validate_request(self, request: MCPToolRequest) -> tuple[bool, str | None]:
        """リクエストを検証.

        Args:
            request: MCP 工具リクエスト

        Returns:
            (検証成功フラグ, エラーメッセージ)
        """
        if request.tool != self.tool_name:
            return (
                False,
                f"Tool name mismatch: expected {self.tool_name}, got {request.tool}",
            )

        if not request.input:
            return False, "Input is required"

        return True, None

    async def execute(self, request: MCPToolRequest) -> MCPToolResponse:
        """リクエストを実行（検証付き）.

        Args:
            request: MCP 工具リクエスト

        Returns:
            MCP 工具レスポンス
        """
        # リクエスト検証
        is_valid, error_msg = self.validate_request(request)
        if not is_valid:
            return MCPToolResponse(
                success=False,
                errors=[error_msg or "Validation failed"],
                metadata={"tool_name": self.tool_name, "tool_version": self.version},
            )

        try:
            # リクエスト処理
            response = await self.handle_request(request)

            # メタデータを追加
            response.metadata.update(
                {
                    "tool_name": self.tool_name,
                    "tool_version": self.version,
                }
            )

            return response

        except Exception as e:
            # エラーハンドリング
            return MCPToolResponse(
                success=False,
                errors=[f"Tool execution failed: {e!s}"],
                metadata={"tool_name": self.tool_name, "tool_version": self.version},
            )


class MCPToolClient:
    """MCP 工具クライアント.

    MCP 工具を呼び出すためのクライアント。

    使用例:
        >>> client = MCPToolClient()
        >>> client.register_tool("cobol_parser", COBOLParser())
        >>> client.register_tool("java_generator", JavaGenerator())
        >>>
        >>> response = await client.call_tool_by_name("cobol_parser", {"cobol_code": "..."})
    """

    def __init__(self) -> None:
        """MCPToolClient を初期化."""
        self.tools: dict[str, MCPTool] = {}

    def register_tool(self, tool_name: str, tool: MCPTool) -> None:
        """MCP 工具を登録.

        Args:
            tool_name: 工具名
            tool: MCP 工具インスタンス
        """
        self.tools[tool_name] = tool

    def unregister_tool(self, tool_name: str) -> None:
        """MCP 工具を登録解除.

        Args:
            tool_name: 工具名
        """
        if tool_name in self.tools:
            del self.tools[tool_name]

    def list_tools(self) -> list[str]:
        """登録されている MCP 工具のリストを取得.

        Returns:
            工具名リスト
        """
        return list(self.tools.keys())

    async def call_tool(self, request: MCPToolRequest) -> MCPToolResponse:
        """MCP 工具を呼び出す.

        Args:
            request: MCP 工具リクエスト

        Returns:
            MCP 工具レスポンス
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
            return await tool.execute(request)

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Tool execution failed: {e!s}"],
            )

    async def call_tool_by_name(
        self,
        tool_name: str,
        input_data: dict[str, Any],
        version: str = "1.0.0",
    ) -> MCPToolResponse:
        """工具名で MCP 工具を呼び出す（簡易版）.

        Args:
            tool_name: 工具名
            input_data: 入力データ
            version: 工具バージョン

        Returns:
            MCP 工具レスポンス
        """
        request = MCPToolRequest(
            tool=tool_name,
            version=version,
            input=input_data,
        )

        return await self.call_tool(request)

    def get_tool(self, tool_name: str) -> MCPTool | None:
        """MCP 工具を取得.

        Args:
            tool_name: 工具名

        Returns:
            MCP 工具インスタンス（存在しない場合は None）
        """
        return self.tools.get(tool_name)

    def has_tool(self, tool_name: str) -> bool:
        """MCP 工具が登録されているかチェック.

        Args:
            tool_name: 工具名

        Returns:
            登録されている場合 True
        """
        return tool_name in self.tools


# ============================================================
# 後方互換エイリアス
# ============================================================

# MCPClient として使用可能（後方互換）
MCPClient = MCPToolClient
