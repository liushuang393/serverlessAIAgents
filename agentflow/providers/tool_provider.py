# -*- coding: utf-8 -*-
"""Tool Provider - 統一ツールアクセスインターフェース.

このモジュールは、MCP/Method/Skills の統一アクセスを提供します。
@tool デコレータで登録されたツールを自動発見し、統一インターフェースで呼び出し。

使用例:
    >>> tools = ToolProvider.discover()  # @tool を自動発見
    >>> result = await tools.call("search_database", query="test")
    >>> # ツール一覧
    >>> for tool in tools.list_tools():
    ...     print(tool.name, tool.description)
"""

import functools
import inspect
import logging
from typing import Any, Callable, TypeVar

from pydantic import BaseModel, Field


# ツールレジストリ（グローバル）
_tool_registry: dict[str, "RegisteredTool"] = {}


class RegisteredTool(BaseModel):
    """登録されたツール情報.

    Args:
        name: ツール名
        description: 説明
        func: 実行関数
        provider_type: プロバイダータイプ (method/mcp/skill)
        parameters: パラメータスキーマ
    """

    name: str = Field(..., description="ツール名")
    description: str = Field(default="", description="説明")
    func: Any = Field(..., description="実行関数")
    provider_type: str = Field(default="method", description="プロバイダータイプ")
    parameters: dict[str, Any] = Field(default_factory=dict, description="パラメータ")

    model_config = {"arbitrary_types_allowed": True}


T = TypeVar("T", bound=Callable[..., Any])


def tool(
    name: str | None = None,
    description: str | None = None,
    provider: str = "method",
    cache: bool = False,
    ttl: int = 3600,
) -> Callable[[T], T]:
    """ツール登録デコレータ.

    @tool で修飾されたメソッドは自動的にToolProviderに登録されます。

    Args:
        name: ツール名（省略時は関数名）
        description: 説明（省略時はdocstring）
        provider: プロバイダータイプ (method/mcp/skill)
        cache: キャッシュ有効化
        ttl: キャッシュTTL秒数

    Returns:
        デコレートされた関数

    使用例:
        >>> @tool
        ... def search_products(query: str) -> list[dict]:
        ...     '''商品を検索'''
        ...     return db.search(query)
        >>> @tool(name="complex_query", provider="mcp")
        ... def query(sql: str) -> list:
        ...     pass
    """

    def decorator(func: T) -> T:
        tool_name = name or func.__name__
        tool_desc = description or (func.__doc__ or "").strip().split("\n")[0]

        # パラメータスキーマを抽出
        sig = inspect.signature(func)
        parameters: dict[str, Any] = {}
        for param_name, param in sig.parameters.items():
            if param_name == "self":
                continue
            param_info: dict[str, Any] = {"type": "string"}  # デフォルト
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_info["type"] = "integer"
                elif param.annotation == float:
                    param_info["type"] = "number"
                elif param.annotation == bool:
                    param_info["type"] = "boolean"
                elif param.annotation == list:
                    param_info["type"] = "array"
                elif param.annotation == dict:
                    param_info["type"] = "object"
            if param.default != inspect.Parameter.empty:
                param_info["default"] = param.default
            parameters[param_name] = param_info

        # レジストリに登録
        registered = RegisteredTool(
            name=tool_name,
            description=tool_desc,
            func=func,
            provider_type=provider,
            parameters=parameters,
        )
        _tool_registry[tool_name] = registered

        # キャッシュラッパー（必要に応じて）
        if cache:
            # 簡易キャッシュ実装
            _cache: dict[str, Any] = {}

            @functools.wraps(func)
            async def cached_wrapper(*args: Any, **kwargs: Any) -> Any:
                cache_key = f"{tool_name}:{args}:{kwargs}"
                if cache_key in _cache:
                    return _cache[cache_key]
                result = await func(*args, **kwargs) if inspect.iscoroutinefunction(func) else func(*args, **kwargs)
                _cache[cache_key] = result
                return result

            return cached_wrapper  # type: ignore

        return func

    return decorator


class ToolProvider:
    """ツール統一プロバイダー.

    @tool で登録されたツールを自動発見し、統一インターフェースで呼び出し。

    使用例:
        >>> tools = ToolProvider.discover()
        >>> result = await tools.call("search_database", query="test")
    """

    _default_instance: "ToolProvider | None" = None

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(__name__)
        self._tools = dict(_tool_registry)  # コピー

    @classmethod
    def discover(cls) -> "ToolProvider":
        """@tool を自動発見してインスタンスを作成.

        Returns:
            ToolProvider: 発見されたツールを含むインスタンス
        """
        if cls._default_instance is None:
            cls._default_instance = cls()
        else:
            # レジストリを再読み込み
            cls._default_instance._tools = dict(_tool_registry)
        return cls._default_instance

    @classmethod
    def register(cls, registered_tool: RegisteredTool) -> None:
        """ツールを手動登録.

        Args:
            registered_tool: 登録するツール
        """
        _tool_registry[registered_tool.name] = registered_tool

    def list_tools(self) -> list[RegisteredTool]:
        """登録されたツール一覧を取得.

        Returns:
            ツールリスト
        """
        return list(self._tools.values())

    def get_tool(self, name: str) -> RegisteredTool | None:
        """指定名のツールを取得.

        Args:
            name: ツール名

        Returns:
            ツール情報、または None
        """
        return self._tools.get(name)

    async def call(self, name: str, **kwargs: Any) -> Any:
        """ツールを呼び出し.

        Args:
            name: ツール名
            **kwargs: パラメータ

        Returns:
            ツールの戻り値

        Raises:
            ValueError: ツールが見つからない場合
        """
        tool_info = self._tools.get(name)
        if tool_info is None:
            msg = f"Tool not found: {name}"
            raise ValueError(msg)

        func = tool_info.func

        # 非同期関数の場合
        if inspect.iscoroutinefunction(func):
            return await func(**kwargs)
        else:
            return func(**kwargs)

    def to_openai_tools(self) -> list[dict[str, Any]]:
        """OpenAI Function Calling形式に変換.

        Returns:
            OpenAI tools形式のリスト
        """
        result = []
        for tool_info in self._tools.values():
            result.append({
                "type": "function",
                "function": {
                    "name": tool_info.name,
                    "description": tool_info.description,
                    "parameters": {
                        "type": "object",
                        "properties": tool_info.parameters,
                    },
                },
            })
        return result

    def to_mcp_tools(self) -> list[dict[str, Any]]:
        """MCP Tools形式に変換.

        Returns:
            MCP tools形式のリスト
        """
        result = []
        for tool_info in self._tools.values():
            result.append({
                "name": tool_info.name,
                "description": tool_info.description,
                "inputSchema": {
                    "type": "object",
                    "properties": tool_info.parameters,
                },
            })
        return result

