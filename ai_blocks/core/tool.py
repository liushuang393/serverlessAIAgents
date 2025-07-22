"""
Tool（ツール）コンポーネント

このモジュールは、外部API/関数実行のための抽象インターフェースと
具体的な実装を提供します。
"""

import asyncio
import inspect
import time
from abc import ABC, abstractmethod
from functools import wraps
from typing import Any, Callable, Dict, List, Optional

from ..config import get_settings
from ..utils.logging import get_logger
from .models import ToolDefinition, ToolResult

logger = get_logger(__name__)


class ToolInterface(ABC):
    """外部API/関数実行のための抽象インターフェース"""

    @abstractmethod
    async def execute(self, tool_name: str, parameters: Dict[str, Any]) -> ToolResult:
        """
        ツールを実行する

        Args:
            tool_name: 実行するツール名
            parameters: ツールに渡すパラメータ

        Returns:
            ToolResult: 実行結果
        """
        pass

    @abstractmethod
    def get_available_tools(self) -> List[ToolDefinition]:
        """
        利用可能なツール一覧を取得する

        Returns:
            List[ToolDefinition]: ツール定義のリスト
        """
        pass

    @abstractmethod
    def register_tool(self, tool: ToolDefinition) -> None:
        """
        新しいツールを登録する

        Args:
            tool: 登録するツール定義
        """
        pass

    @abstractmethod
    def unregister_tool(self, tool_name: str) -> bool:
        """
        ツールの登録を解除する

        Args:
            tool_name: 解除するツール名

        Returns:
            bool: 解除が成功したかどうか
        """
        pass


def tool(
    name: Optional[str] = None,
    description: str = "",
    parameters: Optional[Dict[str, Any]] = None,
):
    """
    関数をツールとして登録するためのデコレータ

    Args:
        name: ツール名（Noneの場合は関数名を使用）
        description: ツールの説明
        parameters: パラメータ定義
    """

    def decorator(func: Callable) -> Callable:
        # ツール名を設定
        tool_name = name or func.__name__

        # パラメータ定義を自動生成（指定されていない場合）
        if parameters is None:
            sig = inspect.signature(func)
            auto_parameters = {}
            for param_name, param in sig.parameters.items():
                param_info = {
                    "type": str(param.annotation)
                    if param.annotation != inspect.Parameter.empty
                    else "Any",
                    "required": param.default == inspect.Parameter.empty,
                }
                if param.default != inspect.Parameter.empty:
                    param_info["default"] = param.default
                auto_parameters[param_name] = param_info
        else:
            auto_parameters = parameters

        # ツール定義を作成
        tool_def = ToolDefinition(
            name=tool_name,
            description=description or func.__doc__ or "",
            parameters=auto_parameters,
            function=func,
        )

        # 関数にツール定義を付加
        func._tool_definition = tool_def  # type: ignore

        @wraps(func)
        async def wrapper(*args, **kwargs):
            if asyncio.iscoroutinefunction(func):
                return await func(*args, **kwargs)
            else:
                return func(*args, **kwargs)

        wrapper._tool_definition = tool_def  # type: ignore
        return wrapper

    return decorator


class ToolManager(ToolInterface):
    """ツール管理の具体実装"""

    def __init__(self, timeout: float = None):
        """
        ツールマネージャーを初期化する

        Args:
            timeout: ツール実行のタイムアウト時間（秒）
        """
        settings = get_settings()
        self._timeout = timeout or settings.tool_timeout
        self._tools: Dict[str, ToolDefinition] = {}

        # 基本的なツールを登録
        self._register_builtin_tools()

        logger.info(f"ツールマネージャーを初期化しました（タイムアウト: {self._timeout}秒）")

    async def execute(self, tool_name: str, parameters: Dict[str, Any]) -> ToolResult:
        """
        ツールを実行する

        Args:
            tool_name: 実行するツール名
            parameters: ツールに渡すパラメータ

        Returns:
            ToolResult: 実行結果
        """
        start_time = time.time()

        # ツールが存在するかチェック
        if tool_name not in self._tools:
            return ToolResult(
                success=False,
                result=None,
                error_message=f"ツール '{tool_name}' が見つかりません",
                execution_time=time.time() - start_time,
            )

        tool_def = self._tools[tool_name]

        try:
            # パラメータを検証
            validated_params = self._validate_parameters(tool_def, parameters)

            # ツール関数が存在するかチェック
            if tool_def.function is None:
                raise ValueError(f"ツール '{tool_name}' の関数が定義されていません")

            # ツールを実行
            if asyncio.iscoroutinefunction(tool_def.function):
                result = await asyncio.wait_for(
                    tool_def.function(**validated_params), timeout=self._timeout
                )
            else:
                result = await asyncio.wait_for(
                    asyncio.to_thread(tool_def.function, **validated_params),
                    timeout=self._timeout,
                )

            execution_time = time.time() - start_time

            logger.debug(f"ツール '{tool_name}' を実行しました（実行時間: {execution_time:.2f}秒）")

            return ToolResult(
                success=True,
                result=result,
                error_message=None,
                execution_time=execution_time,
            )

        except asyncio.TimeoutError:
            execution_time = time.time() - start_time
            error_msg = f"ツール '{tool_name}' の実行がタイムアウトしました（{self._timeout}秒）"
            logger.warning(error_msg)

            return ToolResult(
                success=False,
                result=None,
                error_message=error_msg,
                execution_time=execution_time,
            )

        except Exception as e:
            execution_time = time.time() - start_time
            error_msg = f"ツール '{tool_name}' の実行中にエラーが発生しました: {str(e)}"
            logger.error(error_msg)

            return ToolResult(
                success=False,
                result=None,
                error_message=error_msg,
                execution_time=execution_time,
            )

    def get_available_tools(self) -> List[ToolDefinition]:
        """
        利用可能なツール一覧を取得する

        Returns:
            List[ToolDefinition]: ツール定義のリスト
        """
        return list(self._tools.values())

    def register_tool(self, tool: ToolDefinition) -> None:
        """
        新しいツールを登録する

        Args:
            tool: 登録するツール定義
        """
        self._tools[tool.name] = tool
        logger.info(f"ツール '{tool.name}' を登録しました")

    def register_function(
        self,
        func: Callable,
        name: Optional[str] = None,
        description: str = "",
        parameters: Optional[Dict[str, Any]] = None,
    ) -> None:
        """
        関数をツールとして登録する

        Args:
            func: 登録する関数
            name: ツール名（Noneの場合は関数名を使用）
            description: ツールの説明
            parameters: パラメータ定義
        """
        tool_name = name or func.__name__

        # パラメータ定義を自動生成（指定されていない場合）
        if parameters is None:
            sig = inspect.signature(func)
            auto_parameters = {}
            for param_name, param in sig.parameters.items():
                param_info = {
                    "type": str(param.annotation)
                    if param.annotation != inspect.Parameter.empty
                    else "Any",
                    "required": param.default == inspect.Parameter.empty,
                }
                if param.default != inspect.Parameter.empty:
                    param_info["default"] = param.default
                auto_parameters[param_name] = param_info
        else:
            auto_parameters = parameters

        tool_def = ToolDefinition(
            name=tool_name,
            description=description or func.__doc__ or "",
            parameters=auto_parameters,
            function=func,
        )

        self.register_tool(tool_def)

    def unregister_tool(self, tool_name: str) -> bool:
        """
        ツールの登録を解除する

        Args:
            tool_name: 解除するツール名

        Returns:
            bool: 解除が成功したかどうか
        """
        if tool_name in self._tools:
            del self._tools[tool_name]
            logger.info(f"ツール '{tool_name}' の登録を解除しました")
            return True
        return False

    def _validate_parameters(
        self, tool_def: ToolDefinition, parameters: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        パラメータを検証する

        Args:
            tool_def: ツール定義
            parameters: 検証するパラメータ

        Returns:
            Dict[str, Any]: 検証済みパラメータ

        Raises:
            ValueError: パラメータが無効な場合
        """
        validated = {}

        # 必須パラメータのチェック
        for param_name, param_info in tool_def.parameters.items():
            if param_info.get("required", False) and param_name not in parameters:
                raise ValueError(f"必須パラメータ '{param_name}' が指定されていません")

            if param_name in parameters:
                validated[param_name] = parameters[param_name]
            elif "default" in param_info:
                validated[param_name] = param_info["default"]

        return validated

    def _register_builtin_tools(self) -> None:
        """組み込みツールを登録する"""

        @tool(name="echo", description="入力された文字列をそのまま返す")
        def echo_tool(text: str) -> str:
            """入力された文字列をそのまま返すツール"""
            return text

        @tool(name="add", description="2つの数値を足し算する")
        def add_tool(a: float, b: float) -> float:
            """2つの数値を足し算するツール"""
            return a + b

        @tool(name="multiply", description="2つの数値を掛け算する")
        def multiply_tool(a: float, b: float) -> float:
            """2つの数値を掛け算するツール"""
            return a * b

        @tool(name="get_current_time", description="現在の時刻を取得する")
        def get_current_time_tool() -> str:
            """現在の時刻を取得するツール"""
            from datetime import datetime

            return datetime.now().isoformat()

        # ツールを登録
        for func in [echo_tool, add_tool, multiply_tool, get_current_time_tool]:
            if hasattr(func, "_tool_definition"):
                self.register_tool(func._tool_definition)
