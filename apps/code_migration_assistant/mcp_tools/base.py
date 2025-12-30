# -*- coding: utf-8 -*-
"""MCP Tool基底クラス.

このモジュールはMCP工具の基底クラスと共通データ構造を提供します。
"""

from abc import ABC, abstractmethod
from typing import Any

from pydantic import BaseModel, Field


class MCPToolRequest(BaseModel):
    """MCP工具リクエスト.

    Attributes:
        tool: 工具名称
        version: 工具版本
        input: 工具特定の入力パラメータ
    """

    tool: str = Field(..., description="工具名称")
    version: str = Field(default="1.0.0", description="工具版本")
    input: dict[str, Any] = Field(..., description="工具特定の入力パラメータ")


class MCPToolResponse(BaseModel):
    """MCP工具レスポンス.

    Attributes:
        success: 実行成功フラグ
        output: 工具特定の出力データ
        metadata: 実行メタデータ（実行時間、工具版本など）
        errors: エラーリスト
    """

    success: bool = Field(..., description="実行成功フラグ")
    output: dict[str, Any] | None = Field(default=None, description="工具特定の出力データ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="実行メタデータ")
    errors: list[str] = Field(default_factory=list, description="エラーリスト")


class MCPTool(ABC):
    """MCP工具基底クラス.

    全てのMCP工具はこのクラスを継承し、handle_requestメソッドを実装する必要があります。

    Example:
        >>> class MyTool(MCPTool):
        ...     async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        ...         # 実装
        ...         return MCPToolResponse(success=True, output={...})
    """

    def __init__(self, tool_name: str, version: str = "1.0.0") -> None:
        """MCP工具を初期化.

        Args:
            tool_name: 工具名称
            version: 工具版本
        """
        self.tool_name = tool_name
        self.version = version

    @abstractmethod
    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """MCP工具リクエストを処理.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス

        Raises:
            NotImplementedError: サブクラスで実装する必要があります
        """
        raise NotImplementedError

    def validate_request(self, request: MCPToolRequest) -> tuple[bool, str | None]:
        """リクエストを検証.

        Args:
            request: MCP工具リクエスト

        Returns:
            (検証成功フラグ, エラーメッセージ)
        """
        if request.tool != self.tool_name:
            return False, f"Tool name mismatch: expected {self.tool_name}, got {request.tool}"

        if not request.input:
            return False, "Input is required"

        return True, None

    async def execute(self, request: MCPToolRequest) -> MCPToolResponse:
        """リクエストを実行（検証付き）.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス
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
                errors=[f"Tool execution failed: {str(e)}"],
                metadata={"tool_name": self.tool_name, "tool_version": self.version},
            )

