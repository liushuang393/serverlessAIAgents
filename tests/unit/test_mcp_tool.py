"""MCP Tool 単体テスト.

agentflow/protocols/mcp_tool.py のテスト。
"""

import pytest

from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)


class EchoTool(MCPTool):
    """テスト用エコー工具."""

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """リクエスト処理."""
        return MCPToolResponse(
            success=True,
            output={"echo": request.input.get("message", "empty")},
        )


class FailingTool(MCPTool):
    """テスト用失敗工具."""

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """常に失敗."""
        msg = "Tool failed"
        raise ValueError(msg)


class TestMCPToolRequest:
    """MCPToolRequest のテスト."""

    def test_create_request(self) -> None:
        """リクエスト作成のテスト."""
        request = MCPToolRequest(
            tool="test_tool",
            version="1.0.0",
            input={"key": "value"},
        )

        assert request.tool == "test_tool"
        assert request.version == "1.0.0"
        assert request.input == {"key": "value"}

    def test_default_version(self) -> None:
        """デフォルトバージョンのテスト."""
        request = MCPToolRequest(tool="test", input={})

        assert request.version == "1.0.0"


class TestMCPToolResponse:
    """MCPToolResponse のテスト."""

    def test_success_response(self) -> None:
        """成功レスポンスのテスト."""
        response = MCPToolResponse(
            success=True,
            output={"result": "ok"},
        )

        assert response.success is True
        assert response.output == {"result": "ok"}
        assert response.errors == []

    def test_failure_response(self) -> None:
        """失敗レスポンスのテスト."""
        response = MCPToolResponse(
            success=False,
            errors=["Error 1", "Error 2"],
        )

        assert response.success is False
        assert response.output is None
        assert len(response.errors) == 2


class TestMCPTool:
    """MCPTool のテスト."""

    def test_tool_creation(self) -> None:
        """工具作成のテスト."""
        tool = EchoTool("echo_tool", "2.0.0")

        assert tool.tool_name == "echo_tool"
        assert tool.version == "2.0.0"

    def test_validate_request_success(self) -> None:
        """リクエスト検証成功のテスト."""
        tool = EchoTool("echo")
        request = MCPToolRequest(tool="echo", input={"msg": "hi"})

        is_valid, error = tool.validate_request(request)

        assert is_valid is True
        assert error is None

    def test_validate_request_tool_mismatch(self) -> None:
        """工具名不一致のテスト."""
        tool = EchoTool("echo")
        request = MCPToolRequest(tool="other", input={"msg": "hi"})

        is_valid, error = tool.validate_request(request)

        assert is_valid is False
        assert "mismatch" in error.lower()

    def test_validate_request_empty_input(self) -> None:
        """空入力のテスト."""
        tool = EchoTool("echo")
        request = MCPToolRequest(tool="echo", input={})

        is_valid, error = tool.validate_request(request)

        assert is_valid is False
        assert "required" in error.lower()

    @pytest.mark.asyncio
    async def test_execute_success(self) -> None:
        """実行成功のテスト."""
        tool = EchoTool("echo")
        request = MCPToolRequest(tool="echo", input={"message": "hello"})

        response = await tool.execute(request)

        assert response.success is True
        assert response.output == {"echo": "hello"}
        assert response.metadata["tool_name"] == "echo"

    @pytest.mark.asyncio
    async def test_execute_validation_failure(self) -> None:
        """検証失敗のテスト."""
        tool = EchoTool("echo")
        request = MCPToolRequest(tool="wrong", input={"message": "hello"})

        response = await tool.execute(request)

        assert response.success is False
        assert len(response.errors) > 0

    @pytest.mark.asyncio
    async def test_execute_exception_handling(self) -> None:
        """例外処理のテスト."""
        tool = FailingTool("failing")
        request = MCPToolRequest(tool="failing", input={"x": 1})

        response = await tool.execute(request)

        assert response.success is False
        assert "failed" in response.errors[0].lower()


class TestMCPToolClient:
    """MCPToolClient のテスト."""

    def test_client_creation(self) -> None:
        """クライアント作成のテスト."""
        client = MCPToolClient()

        assert client.tools == {}
        assert client.list_tools() == []

    def test_register_tool(self) -> None:
        """工具登録のテスト."""
        client = MCPToolClient()
        tool = EchoTool("echo")

        client.register_tool("echo", tool)

        assert "echo" in client.list_tools()
        assert client.has_tool("echo") is True
        assert client.get_tool("echo") is tool

    def test_unregister_tool(self) -> None:
        """工具登録解除のテスト."""
        client = MCPToolClient()
        tool = EchoTool("echo")
        client.register_tool("echo", tool)

        client.unregister_tool("echo")

        assert client.has_tool("echo") is False

    def test_get_nonexistent_tool(self) -> None:
        """存在しない工具取得のテスト."""
        client = MCPToolClient()

        assert client.get_tool("nonexistent") is None
        assert client.has_tool("nonexistent") is False

    @pytest.mark.asyncio
    async def test_call_tool_success(self) -> None:
        """工具呼び出し成功のテスト."""
        client = MCPToolClient()
        client.register_tool("echo", EchoTool("echo"))

        request = MCPToolRequest(tool="echo", input={"message": "test"})
        response = await client.call_tool(request)

        assert response.success is True
        assert response.output == {"echo": "test"}

    @pytest.mark.asyncio
    async def test_call_tool_not_registered(self) -> None:
        """未登録工具呼び出しのテスト."""
        client = MCPToolClient()

        request = MCPToolRequest(tool="unknown", input={"x": 1})
        response = await client.call_tool(request)

        assert response.success is False
        assert "not registered" in response.errors[0].lower()

    @pytest.mark.asyncio
    async def test_call_tool_by_name(self) -> None:
        """名前による工具呼び出しのテスト."""
        client = MCPToolClient()
        client.register_tool("echo", EchoTool("echo"))

        response = await client.call_tool_by_name("echo", {"message": "hi"})

        assert response.success is True
        assert response.output == {"echo": "hi"}

    @pytest.mark.asyncio
    async def test_call_tool_by_name_with_version(self) -> None:
        """バージョン指定の呼び出しテスト."""
        client = MCPToolClient()
        client.register_tool("echo", EchoTool("echo", "2.0.0"))

        response = await client.call_tool_by_name("echo", {"message": "hi"}, "2.0.0")

        assert response.success is True
