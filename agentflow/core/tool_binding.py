"""ツールバインディングインターフェース.

ランタイムでAgentにツールをアタッチする機能を提供するモジュール。

設計原則:
- 高度な抽象化: ツールソースに依存しない統一バインディング
- 低結合: Agent実装に依存しない
- 拡張性: 新しいバインディング方法の追加が容易

使用例:
    >>> # ツールをバインド
    >>> binder = ToolBinder(tool_registry)
    >>> bound_agent = await binder.bind(agent, tool_uris=["tool://builtin/search"])
    >>>
    >>> # 能力ベースでバインド
    >>> bound_agent = await binder.bind_for_capability(agent, capability)
    >>>
    >>> # バインドされたツールをMCP形式で取得
    >>> mcp_tools = bound_agent._tools.to_mcp_format()
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Iterator

    from agentflow.core.capability_spec import AgentCapabilitySpec
    from agentflow.core.tool_definition import ToolDefinition
    from agentflow.core.tool_registry import ToolRegistry


class BoundTools:
    """バインドされたツールのコンテナ.

    Agentにバインドされたツールを保持し、
    MCP形式への変換やツールの取得を提供。

    Attributes:
        _tools: URI → ToolDefinition のマッピング
    """

    def __init__(self, tools: list[ToolDefinition]) -> None:
        """ツールリストで初期化.

        Args:
            tools: バインドするツールのリスト
        """
        self._tools = {t.uri: t for t in tools}

    def __len__(self) -> int:
        """バインドされたツール数を返す."""
        return len(self._tools)

    def __iter__(self) -> Iterator[ToolDefinition]:
        """ツールをイテレート."""
        return iter(self._tools.values())

    def get(self, uri: str) -> ToolDefinition | None:
        """URIでツールを取得.

        Args:
            uri: ツールURI

        Returns:
            ToolDefinition または見つからない場合None
        """
        return self._tools.get(uri)

    def list_uris(self) -> list[str]:
        """バインドされたツールのURIリストを取得.

        Returns:
            ツールURIのリスト
        """
        return list(self._tools.keys())

    def to_mcp_format(self) -> list[dict[str, Any]]:
        """バインドされた全ツールをMCP形式に変換.

        LLMに渡すためのMCP互換形式に変換。

        Returns:
            MCP形式のツール定義リスト
        """
        return [t.to_mcp() for t in self._tools.values()]


class ToolExecutor:
    """バインドされたツールのエグゼキュータ.

    ツールの実行を管理。実際の実行はハンドラを通じて行う。

    Attributes:
        _tools: URI → ToolDefinition のマッピング
        _handlers: URI → ハンドラ関数 のマッピング
    """

    def __init__(self, tools: list[ToolDefinition]) -> None:
        """ツールリストで初期化.

        Args:
            tools: 実行可能なツールのリスト
        """
        self._tools = {t.uri: t for t in tools}
        self._handlers: dict[str, Any] = {}

    def register_handler(self, uri: str, handler: Any) -> None:
        """ツールURIの実行ハンドラを登録.

        Args:
            uri: ツールURI
            handler: 実行ハンドラ（async callable）
        """
        self._handlers[uri] = handler

    async def execute(self, uri: str, **kwargs: Any) -> Any:
        """URIでツールを実行.

        Args:
            uri: ツールURI
            **kwargs: ツール入力パラメータ

        Returns:
            ツール実行結果

        Raises:
            ValueError: ツールが見つからない or ハンドラがない場合
        """
        if uri not in self._tools:
            msg = f"ツールがバインドされていません: {uri}"
            raise ValueError(msg)

        handler = self._handlers.get(uri)
        if not handler:
            msg = f"ツールのハンドラがありません: {uri}"
            raise ValueError(msg)

        return await handler(**kwargs)

    def has_tool(self, uri: str) -> bool:
        """ツールがバインドされているかチェック.

        Args:
            uri: ツールURI

        Returns:
            バインドされている場合True
        """
        return uri in self._tools


class ToolBinder:
    """ツールバインダー.

    レジストリからツールを解決してAgentインスタンスにバインド。

    主な機能:
    - URIベースのバインディング
    - 能力ベースのバインディング
    - ツール存在検証

    Attributes:
        _registry: ツールを解決するToolRegistry
    """

    def __init__(self, registry: ToolRegistry) -> None:
        """ToolRegistryで初期化.

        Args:
            registry: ツールを解決するToolRegistry
        """
        self._registry = registry

    async def bind(
        self,
        agent: Any,
        tool_uris: list[str] | None = None,
    ) -> Any:
        """指定されたツールをAgentにバインド.

        存在しないツールはスキップされ、警告なしで処理される。

        Args:
            agent: ツールをバインドするAgentインスタンス
            tool_uris: バインドするツールURIのリスト

        Returns:
            ツールがバインドされたAgent
        """
        tools = []

        for uri in tool_uris or []:
            tool = self._registry.get(uri)
            if tool:
                tools.append(tool)

        agent._tools = BoundTools(tools)
        agent._tool_executor = self._create_executor(tools)

        return agent

    async def bind_for_capability(
        self,
        agent: Any,
        capability: AgentCapabilitySpec,
    ) -> Any:
        """能力が必要とするツールをすべてバインド.

        AgentCapabilitySpecで宣言されたrequired_toolsを解決してバインド。

        Args:
            agent: Agentインスタンス
            capability: ツール要件を宣言した能力

        Returns:
            ツールがバインドされたAgent
        """
        return await self.bind(agent, capability.required_tools)

    def _create_executor(self, tools: list[ToolDefinition]) -> ToolExecutor:
        """バインドされたツールのエグゼキュータを作成.

        Args:
            tools: ツール定義のリスト

        Returns:
            ToolExecutorインスタンス
        """
        return ToolExecutor(tools)


__all__ = [
    "BoundTools",
    "ToolBinder",
    "ToolExecutor",
]
