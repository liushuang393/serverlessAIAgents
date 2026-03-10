"""ToolCapableAgent - Tool Use (MCP/Function Calling) 対応 Agent 基底クラス.

ResilientAgent を継承し、ツール実行能力を追加する。

使用例:
    >>> from agentflow.core.tool_agent import ToolCapableAgent
    >>> from agentflow.core.tool_definition import ToolDefinition, ToolSource
    >>>
    >>> class MyToolAgent(ToolCapableAgent[MyInput, MyOutput]):
    ...     name = "MyToolAgent"
    ...     tools = [
    ...         ToolDefinition(
    ...             uri="tool://builtin/search",
    ...             name="search",
    ...             description="検索ツール",
    ...             source=ToolSource.BUILTIN,
    ...             input_schema={
    ...                 "type": "object",
    ...                 "properties": {"query": {"type": "string"}},
    ...             },
    ...         ),
    ...     ]
    ...
    ...     async def process(self, input_data: MyInput) -> MyOutput:
    ...         result = await self.call_tool("search", {"query": "..."})
    ...         return MyOutput(answer=result)
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, ClassVar

from agentflow.core.resilient_agent import InputT, OutputT, ResilientAgent
from agentflow.core.tool_binding import BoundTools, ToolExecutor


if TYPE_CHECKING:
    from agentflow.core.tool_definition import ToolDefinition


logger = logging.getLogger(__name__)


class ToolCapableAgent(ResilientAgent[InputT, OutputT]):
    """Tool Use (MCP/Function Calling) をサポートする Agent 基底クラス.

    ResilientAgent の全機能（リトライ、タイムアウト、型安全）に加え、
    ツール定義・実行の基盤を提供する。

    クラス属性:
        tools: このAgentが使用可能なツール定義のリスト

    既存の ToolBinder / BoundTools / ToolExecutor インフラと統合し、
    ツールの宣言・検索・実行を簡潔に行えるようにする。
    """

    tools: ClassVar[list[ToolDefinition]] = []

    def __init__(self, **kwargs: Any) -> None:
        """初期化.

        Args:
            **kwargs: ResilientAgent への引数
        """
        super().__init__(**kwargs)
        # クラス属性 tools から BoundTools / ToolExecutor を構築
        self._bound_tools = BoundTools(list(self.tools))
        self._tool_executor = ToolExecutor(list(self.tools))

    async def call_tool(self, name: str, args: dict[str, Any]) -> Any:
        """ツールを名前で呼び出す.

        Args:
            name: ツール名
            args: ツール引数

        Returns:
            ツール実行結果

        Raises:
            ValueError: 指定名のツールが見つからない場合
        """
        tool = self._find_tool(name)
        if tool is None:
            available = [t.name for t in self.tools]
            msg = f"ツール '{name}' が見つかりません。利用可能: {available}"
            raise ValueError(msg)
        logger.info("ツール実行: %s (args=%s)", name, list(args.keys()))
        return await self._execute_tool(tool, args)

    async def _execute_tool(
        self,
        tool: ToolDefinition,
        args: dict[str, Any],
    ) -> Any:
        """ツールを実行する（サブクラスでオーバーライド可能）.

        デフォルト実装は ToolExecutor にハンドラが登録されていれば
        それを使用する。なければ NotImplementedError を送出する。

        Args:
            tool: ツール定義
            args: ツール引数

        Returns:
            ツール実行結果

        Raises:
            NotImplementedError: ハンドラ未登録の場合
        """
        # ToolExecutor にハンドラが登録済みなら委譲
        if self._tool_executor.has_tool(tool.uri):
            try:
                return await self._tool_executor.execute(tool.uri, **args)
            except ValueError:
                # ハンドラ未登録の場合は下のフォールバックへ
                pass

        msg = (
            f"ツール '{tool.name}' (uri={tool.uri}) にハンドラがありません。"
            "register_tool_handler() でハンドラを登録するか、"
            "サブクラスで _execute_tool をオーバーライドしてください。"
        )
        raise NotImplementedError(msg)

    def register_tool_handler(
        self,
        name: str,
        handler: Any,
    ) -> None:
        """ツール名に対する実行ハンドラを登録.

        Args:
            name: ツール名
            handler: async callable なハンドラ関数

        Raises:
            ValueError: 指定名のツールが見つからない場合
        """
        tool = self._find_tool(name)
        if tool is None:
            available = [t.name for t in self.tools]
            msg = f"ツール '{name}' が見つかりません。利用可能: {available}"
            raise ValueError(msg)
        self._tool_executor.register_handler(tool.uri, handler)

    def get_tool_schemas(self) -> list[dict[str, Any]]:
        """全ツールのスキーマを取得（LLM function calling 用）.

        ToolDefinition.to_mcp() を利用して MCP 互換形式で返す。

        Returns:
            OpenAI function calling 互換のスキーマリスト
        """
        schemas: list[dict[str, Any]] = []
        for tool in self.tools:
            mcp = tool.to_mcp()
            schema: dict[str, Any] = {
                "type": "function",
                "function": {
                    "name": mcp["name"],
                    "description": mcp.get("description", ""),
                },
            }
            if mcp.get("inputSchema"):
                schema["function"]["parameters"] = mcp["inputSchema"]
            schemas.append(schema)
        return schemas

    def _find_tool(self, name: str) -> ToolDefinition | None:
        """名前でツールを検索.

        Args:
            name: ツール名

        Returns:
            ToolDefinition または None
        """
        return next((t for t in self.tools if t.name == name), None)


__all__ = [
    "ToolCapableAgent",
]
