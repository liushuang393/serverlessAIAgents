"""
Toolコンポーネントの単体テスト
"""

import asyncio

import pytest

from ai_blocks.core.tool import ToolDefinition, ToolManager, ToolResult, tool


class TestToolDecorator:
    """toolデコレータのテスト"""

    def test_basic_decoration(self):
        """基本的なデコレーションのテスト"""

        @tool(name="test_tool", description="テスト用ツール")
        def test_function(x: int, y: int = 10) -> int:
            return x + y

        assert hasattr(test_function, "_tool_definition")
        tool_def = test_function._tool_definition

        assert tool_def.name == "test_tool"
        assert tool_def.description == "テスト用ツール"
        # デコレーターが関数を変更するため、関数名で比較
        assert tool_def.function.__name__ == test_function.__name__

    def test_auto_name_generation(self):
        """自動名前生成のテスト"""

        @tool(description="自動名前テスト")
        def auto_name_function():
            return "test"

        tool_def = auto_name_function._tool_definition
        assert tool_def.name == "auto_name_function"

    def test_parameter_inference(self):
        """パラメータ推論のテスト"""

        @tool()
        def param_test(required_param: str, optional_param: int = 42) -> str:
            return f"{required_param}: {optional_param}"

        tool_def = param_test._tool_definition
        params = tool_def.parameters

        assert "required_param" in params
        assert params["required_param"]["required"] is True

        assert "optional_param" in params
        assert params["optional_param"]["required"] is False
        assert params["optional_param"]["default"] == 42


class TestToolManager:
    """ToolManagerのテスト"""

    def setup_method(self):
        """各テストメソッドの前に実行される"""
        self.tool_manager = ToolManager(timeout=5.0)

    @pytest.mark.asyncio
    async def test_builtin_tools(self):
        """組み込みツールのテスト"""
        tools = self.tool_manager.get_available_tools()
        tool_names = [tool.name for tool in tools]

        # 組み込みツールが登録されているかチェック
        assert "echo" in tool_names
        assert "add" in tool_names
        assert "multiply" in tool_names
        assert "get_current_time" in tool_names

    @pytest.mark.asyncio
    async def test_echo_tool(self):
        """echoツールのテスト"""
        result = await self.tool_manager.execute("echo", {"text": "Hello, World!"})

        assert result.success is True
        assert result.result == "Hello, World!"
        assert result.error_message is None
        assert result.execution_time > 0

    @pytest.mark.asyncio
    async def test_add_tool(self):
        """addツールのテスト"""
        result = await self.tool_manager.execute("add", {"a": 5, "b": 3})

        assert result.success is True
        assert result.result == 8
        assert result.error_message is None

    @pytest.mark.asyncio
    async def test_multiply_tool(self):
        """multiplyツールのテスト"""
        result = await self.tool_manager.execute("multiply", {"a": 4, "b": 7})

        assert result.success is True
        assert result.result == 28
        assert result.error_message is None

    @pytest.mark.asyncio
    async def test_get_current_time_tool(self):
        """get_current_timeツールのテスト"""
        result = await self.tool_manager.execute("get_current_time", {})

        assert result.success is True
        assert isinstance(result.result, str)
        assert "T" in result.result  # ISO形式の時刻文字列

    @pytest.mark.asyncio
    async def test_nonexistent_tool(self):
        """存在しないツールのテスト"""
        result = await self.tool_manager.execute("nonexistent_tool", {})

        assert result.success is False
        assert result.result is None
        assert result.error_message is not None
        assert "見つかりません" in result.error_message

    @pytest.mark.asyncio
    async def test_register_function(self):
        """関数登録のテスト"""

        def custom_tool(message: str) -> str:
            return f"Custom: {message}"

        self.tool_manager.register_function(
            custom_tool, name="custom", description="カスタムツール"
        )

        # 登録されたかチェック
        tools = self.tool_manager.get_available_tools()
        tool_names = [tool.name for tool in tools]
        assert "custom" in tool_names

        # 実行テスト
        result = await self.tool_manager.execute("custom", {"message": "test"})
        assert result.success is True
        assert result.result == "Custom: test"

    @pytest.mark.asyncio
    async def test_register_tool_definition(self):
        """ツール定義登録のテスト"""

        def test_func(x: int) -> int:
            return x * 2

        tool_def = ToolDefinition(
            name="double",
            description="数値を2倍にする",
            parameters={"x": {"type": "int", "required": True}},
            function=test_func,
        )

        self.tool_manager.register_tool(tool_def)

        # 実行テスト
        result = await self.tool_manager.execute("double", {"x": 5})
        assert result.success is True
        assert result.result == 10

    @pytest.mark.asyncio
    async def test_unregister_tool(self):
        """ツール登録解除のテスト"""

        # カスタムツールを登録
        def temp_tool() -> str:
            return "temp"

        self.tool_manager.register_function(temp_tool, name="temp")

        # 登録されているかチェック
        tools_before = self.tool_manager.get_available_tools()
        tool_names_before = [tool.name for tool in tools_before]
        assert "temp" in tool_names_before

        # 登録解除
        result = self.tool_manager.unregister_tool("temp")
        assert result is True

        # 解除されているかチェック
        tools_after = self.tool_manager.get_available_tools()
        tool_names_after = [tool.name for tool in tools_after]
        assert "temp" not in tool_names_after

        # 存在しないツールの解除
        result = self.tool_manager.unregister_tool("nonexistent")
        assert result is False

    @pytest.mark.asyncio
    async def test_parameter_validation(self):
        """パラメータ検証のテスト"""

        def strict_tool(required_param: str, optional_param: int = 10) -> str:
            return f"{required_param}: {optional_param}"

        self.tool_manager.register_function(
            strict_tool,
            parameters={
                "required_param": {"type": "str", "required": True},
                "optional_param": {"type": "int", "required": False, "default": 10},
            },
        )

        # 必須パラメータありの正常実行
        result = await self.tool_manager.execute(
            "strict_tool", {"required_param": "test"}
        )
        assert result.success is True
        assert result.result == "test: 10"

        # 必須パラメータなしのエラー
        result = await self.tool_manager.execute("strict_tool", {})
        assert result.success is False
        assert result.error_message is not None
        assert "必須パラメータ" in result.error_message

    @pytest.mark.asyncio
    async def test_async_tool(self):
        """非同期ツールのテスト"""

        async def async_tool(delay: float = 0.1) -> str:
            await asyncio.sleep(delay)
            return "async_result"

        self.tool_manager.register_function(async_tool)

        result = await self.tool_manager.execute("async_tool", {"delay": 0.05})
        assert result.success is True
        assert result.result == "async_result"
        assert result.execution_time >= 0.05

    @pytest.mark.asyncio
    async def test_tool_timeout(self):
        """ツールタイムアウトのテスト"""
        tool_manager = ToolManager(timeout=0.1)  # 短いタイムアウト

        def slow_tool() -> str:
            import time

            time.sleep(0.2)  # タイムアウトより長い処理
            return "slow_result"

        tool_manager.register_function(slow_tool)

        result = await tool_manager.execute("slow_tool", {})
        assert result.success is False
        assert result.error_message is not None
        assert "タイムアウト" in result.error_message

    @pytest.mark.asyncio
    async def test_tool_exception(self):
        """ツール例外のテスト"""

        def error_tool() -> str:
            raise ValueError("テストエラー")

        self.tool_manager.register_function(error_tool)

        result = await self.tool_manager.execute("error_tool", {})
        assert result.success is False
        assert result.error_message is not None
        assert "テストエラー" in result.error_message


class TestToolResult:
    """ToolResultのテスト"""

    def test_successful_result(self):
        """成功結果のテスト"""
        result = ToolResult(
            success=True, result="test_result", error_message=None, execution_time=0.5
        )

        assert result.success is True
        assert result.result == "test_result"
        assert result.error_message is None
        assert result.execution_time == 0.5

    def test_failed_result(self):
        """失敗結果のテスト"""
        result = ToolResult(
            success=False, result=None, error_message="エラーが発生しました", execution_time=0.1
        )

        assert result.success is False
        assert result.result is None
        assert result.error_message == "エラーが発生しました"
        assert result.execution_time == 0.1


if __name__ == "__main__":
    pytest.main([__file__])
