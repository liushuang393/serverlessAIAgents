# -*- coding: utf-8 -*-
"""統一ツールプロバイダー - Skills/MCP/内蔵ツールの統合管理.

三種類のツールを統一インターフェースで管理:
- Skills: SKILL.md形式の固化された指示
- MCP Tools: 外部MCPサーバーのツール
- Builtin Tools: フレームワーク内蔵ツール

設計原則:
- 単一インターフェース: 呼び出し側はツール種別を意識しない
- 自動ルーティング: URIスキームで適切なプロバイダーにルーティング
- 統一結果形式: 全ツールが同じ形式で結果を返す

使用例:
    >>> from agentflow.providers.unified_tool import UnifiedToolProvider
    >>>
    >>> provider = UnifiedToolProvider()
    >>> await provider.initialize()
    >>>
    >>> # Skills経由
    >>> result = await provider.call("skill://web_scraper", {"url": "..."})
    >>>
    >>> # MCP経由
    >>> result = await provider.call("mcp://browser/scrape", {"url": "..."})
    >>>
    >>> # 内蔵ツール
    >>> result = await provider.call("builtin://file_reader", {"path": "..."})
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field


class ToolType(str, Enum):
    """ツール種別."""

    SKILL = "skill"        # Skills (SKILL.md)
    MCP = "mcp"            # MCP外部ツール
    BUILTIN = "builtin"    # 内蔵ツール
    CUSTOM = "custom"      # カスタムツール


class ToolStatus(str, Enum):
    """ツール実行状態."""

    SUCCESS = "success"
    FAILED = "failed"
    TIMEOUT = "timeout"
    NOT_FOUND = "not_found"
    PERMISSION_DENIED = "permission_denied"


@dataclass
class ToolResult:
    """統一ツール実行結果.

    全てのツール種別が同じ形式で結果を返す。

    Attributes:
        success: 成功したかどうか
        status: 実行状態
        tool_uri: ツールURI
        tool_type: ツール種別
        output: 出力データ
        error: エラーメッセージ
        duration_ms: 実行時間（ミリ秒）
        metadata: メタデータ
        timestamp: 実行時刻
    """

    success: bool
    status: ToolStatus
    tool_uri: str
    tool_type: ToolType
    output: Any = None
    error: str | None = None
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "status": self.status.value,
            "tool_uri": self.tool_uri,
            "tool_type": self.tool_type.value,
            "output": self.output,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "metadata": self.metadata,
            "timestamp": self.timestamp.isoformat(),
        }


class ToolDefinition(BaseModel):
    """ツール定義.

    Attributes:
        uri: ツールURI（例: skill://web_scraper, mcp://browser/scrape）
        name: ツール名
        description: 説明
        tool_type: ツール種別
        parameters: パラメータスキーマ
        required_permissions: 必要な権限
        tags: タグ
    """

    uri: str = Field(default="", description="ツールURI")
    name: str = Field(default="", description="ツール名")
    description: str = Field(default="", description="説明")
    tool_type: ToolType = Field(default=ToolType.BUILTIN)
    parameters: dict[str, Any] = Field(default_factory=dict)
    required_permissions: list[str] = Field(default_factory=list)
    tags: list[str] = Field(default_factory=list)
    enabled: bool = Field(default=True)


class ToolProvider(ABC):
    """ツールプロバイダー基底クラス."""

    @abstractmethod
    async def call(
        self,
        tool_name: str,
        params: dict[str, Any],
    ) -> ToolResult:
        """ツールを呼び出す."""
        ...

    @abstractmethod
    def list_tools(self) -> list[ToolDefinition]:
        """利用可能なツール一覧を取得."""
        ...

    @abstractmethod
    def get_tool(self, tool_name: str) -> ToolDefinition | None:
        """ツール定義を取得."""
        ...


class SkillToolProvider(ToolProvider):
    """Skillsツールプロバイダー.

    SKILL.md形式のスキルをツールとして提供。
    """

    def __init__(self) -> None:
        """初期化."""
        self._skills: dict[str, ToolDefinition] = {}
        self._engine: Any = None
        self._logger = logging.getLogger(__name__)

    async def initialize(self) -> None:
        """初期化（SkillEngineをロード）."""
        try:
            from agentflow.skills.engine import SkillEngine
            self._engine = SkillEngine(auto_learn=False)

            # スキルをツール定義として登録
            for skill in self._engine.list_skills():
                tool_def = ToolDefinition(
                    uri=f"skill://{skill.name}",
                    name=skill.name,
                    description=skill.description,
                    tool_type=ToolType.SKILL,
                    parameters={"query": {"type": "string"}},
                    tags=skill.tags if hasattr(skill, "tags") else [],
                )
                self._skills[skill.name] = tool_def

            self._logger.info(f"Skillsプロバイダー初期化完了: {len(self._skills)}件")
        except Exception as e:
            self._logger.warning(f"Skillsプロバイダー初期化失敗: {e}")

    async def call(
        self,
        tool_name: str,
        params: dict[str, Any],
    ) -> ToolResult:
        """Skillを実行."""
        start_time = datetime.now()

        if not self._engine:
            return ToolResult(
                success=False,
                status=ToolStatus.FAILED,
                tool_uri=f"skill://{tool_name}",
                tool_type=ToolType.SKILL,
                error="SkillEngineが初期化されていません",
            )

        try:
            query = params.get("query", "")
            result = await self._engine.resolve(query)

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            return ToolResult(
                success=True,
                status=ToolStatus.SUCCESS,
                tool_uri=f"skill://{tool_name}",
                tool_type=ToolType.SKILL,
                output={
                    "skill_name": result.skill.name,
                    "instructions": result.instructions,
                    "matched": result.matched,
                    "generated": result.generated,
                },
                duration_ms=duration_ms,
            )
        except Exception as e:
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            return ToolResult(
                success=False,
                status=ToolStatus.FAILED,
                tool_uri=f"skill://{tool_name}",
                tool_type=ToolType.SKILL,
                error=str(e),
                duration_ms=duration_ms,
            )

    def list_tools(self) -> list[ToolDefinition]:
        """利用可能なスキル一覧."""
        return list(self._skills.values())

    def get_tool(self, tool_name: str) -> ToolDefinition | None:
        """スキル定義を取得."""
        return self._skills.get(tool_name)


class MCPToolProvider(ToolProvider):
    """MCPツールプロバイダー.

    MCPサーバーのツールを提供。
    """

    def __init__(self) -> None:
        """初期化."""
        self._client: Any = None
        self._tools: dict[str, ToolDefinition] = {}
        self._logger = logging.getLogger(__name__)

    async def initialize(self, config: Any = None) -> None:
        """初期化（MCPClientを接続）."""
        try:
            from agentflow.protocols.mcp_client import MCPClient
            from agentflow.protocols.mcp_config import MCPConfig

            if config:
                self._client = MCPClient(config)
            else:
                # デフォルト設定
                self._client = MCPClient(MCPConfig(servers=[]))

            await self._client.connect()

            # ツールをツール定義として登録
            for tool_uri in self._client.list_tools():
                tool_info = self._client.get_tool_info(tool_uri)
                if tool_info:
                    tool_def = ToolDefinition(
                        uri=tool_uri,
                        name=tool_info.get("name", ""),
                        description=tool_info.get("description", ""),
                        tool_type=ToolType.MCP,
                        parameters=tool_info.get("input_schema", {}),
                    )
                    self._tools[tool_uri] = tool_def

            self._logger.info(f"MCPプロバイダー初期化完了: {len(self._tools)}件")
        except Exception as e:
            self._logger.warning(f"MCPプロバイダー初期化失敗: {e}")

    async def call(
        self,
        tool_name: str,
        params: dict[str, Any],
    ) -> ToolResult:
        """MCPツールを呼び出す."""
        start_time = datetime.now()
        tool_uri = tool_name if tool_name.startswith("mcp://") else f"mcp://{tool_name}"

        if not self._client:
            return ToolResult(
                success=False,
                status=ToolStatus.FAILED,
                tool_uri=tool_uri,
                tool_type=ToolType.MCP,
                error="MCPClientが初期化されていません",
            )

        try:
            result = await self._client.call_tool(tool_uri, params)
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            if result.get("success"):
                return ToolResult(
                    success=True,
                    status=ToolStatus.SUCCESS,
                    tool_uri=tool_uri,
                    tool_type=ToolType.MCP,
                    output=result.get("result"),
                    duration_ms=duration_ms,
                )
            else:
                return ToolResult(
                    success=False,
                    status=ToolStatus.FAILED,
                    tool_uri=tool_uri,
                    tool_type=ToolType.MCP,
                    error=result.get("error", "不明なエラー"),
                    duration_ms=duration_ms,
                )
        except Exception as e:
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            return ToolResult(
                success=False,
                status=ToolStatus.FAILED,
                tool_uri=tool_uri,
                tool_type=ToolType.MCP,
                error=str(e),
                duration_ms=duration_ms,
            )

    def list_tools(self) -> list[ToolDefinition]:
        """利用可能なMCPツール一覧."""
        return list(self._tools.values())

    def get_tool(self, tool_name: str) -> ToolDefinition | None:
        """MCPツール定義を取得."""
        tool_uri = tool_name if tool_name.startswith("mcp://") else f"mcp://{tool_name}"
        return self._tools.get(tool_uri)


class BuiltinToolProvider(ToolProvider):
    """内蔵ツールプロバイダー.

    フレームワーク内蔵の基本ツールを提供。
    """

    def __init__(self) -> None:
        """初期化."""
        self._tools: dict[str, ToolDefinition] = {}
        self._handlers: dict[str, Callable[..., Any]] = {}
        self._logger = logging.getLogger(__name__)

        # デフォルトツールを登録
        self._register_default_tools()

    def _register_default_tools(self) -> None:
        """デフォルトツールを登録."""
        # ファイル読み取り
        self.register_tool(
            ToolDefinition(
                uri="builtin://file_reader",
                name="file_reader",
                description="ファイルの内容を読み取る",
                tool_type=ToolType.BUILTIN,
                parameters={
                    "path": {"type": "string", "description": "ファイルパス"},
                    "encoding": {"type": "string", "default": "utf-8"},
                },
            ),
            self._read_file,
        )

        # JSON解析
        self.register_tool(
            ToolDefinition(
                uri="builtin://json_parser",
                name="json_parser",
                description="JSON文字列を解析する",
                tool_type=ToolType.BUILTIN,
                parameters={
                    "json_string": {"type": "string", "description": "JSON文字列"},
                },
            ),
            self._parse_json,
        )

        # 計算
        self.register_tool(
            ToolDefinition(
                uri="builtin://calculator",
                name="calculator",
                description="数式を計算する",
                tool_type=ToolType.BUILTIN,
                parameters={
                    "expression": {"type": "string", "description": "計算式"},
                },
            ),
            self._calculate,
        )

        # 日時
        self.register_tool(
            ToolDefinition(
                uri="builtin://datetime",
                name="datetime",
                description="現在日時を取得する",
                tool_type=ToolType.BUILTIN,
                parameters={
                    "format": {"type": "string", "default": "%Y-%m-%d %H:%M:%S"},
                },
            ),
            self._get_datetime,
        )

    def register_tool(
        self,
        definition: ToolDefinition,
        handler: Callable[..., Any],
    ) -> None:
        """ツールを登録.

        Args:
            definition: ツール定義
            handler: ハンドラー関数
        """
        self._tools[definition.name] = definition
        self._handlers[definition.name] = handler
        self._logger.debug(f"内蔵ツール登録: {definition.name}")

    async def call(
        self,
        tool_name: str,
        params: dict[str, Any],
    ) -> ToolResult:
        """内蔵ツールを呼び出す."""
        start_time = datetime.now()
        tool_uri = f"builtin://{tool_name}"

        if tool_name not in self._handlers:
            return ToolResult(
                success=False,
                status=ToolStatus.NOT_FOUND,
                tool_uri=tool_uri,
                tool_type=ToolType.BUILTIN,
                error=f"ツール '{tool_name}' が見つかりません",
            )

        try:
            handler = self._handlers[tool_name]
            result = await handler(params) if callable(handler) else handler(params)
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            return ToolResult(
                success=True,
                status=ToolStatus.SUCCESS,
                tool_uri=tool_uri,
                tool_type=ToolType.BUILTIN,
                output=result,
                duration_ms=duration_ms,
            )
        except Exception as e:
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            return ToolResult(
                success=False,
                status=ToolStatus.FAILED,
                tool_uri=tool_uri,
                tool_type=ToolType.BUILTIN,
                error=str(e),
                duration_ms=duration_ms,
            )

    def list_tools(self) -> list[ToolDefinition]:
        """利用可能な内蔵ツール一覧."""
        return list(self._tools.values())

    def get_tool(self, tool_name: str) -> ToolDefinition | None:
        """内蔵ツール定義を取得."""
        return self._tools.get(tool_name)

    # === 内蔵ツールハンドラー ===

    async def _read_file(self, params: dict[str, Any]) -> dict[str, Any]:
        """ファイル読み取りハンドラー."""
        path = params.get("path", "")
        encoding = params.get("encoding", "utf-8")

        with open(path, encoding=encoding) as f:
            content = f.read()

        return {"content": content, "path": path, "size": len(content)}

    async def _parse_json(self, params: dict[str, Any]) -> dict[str, Any]:
        """JSON解析ハンドラー."""
        import json
        json_string = params.get("json_string", "{}")
        return json.loads(json_string)

    async def _calculate(self, params: dict[str, Any]) -> dict[str, Any]:
        """計算ハンドラー."""
        expression = params.get("expression", "0")
        # 安全な計算のみ許可
        allowed_chars = set("0123456789+-*/().% ")
        if not all(c in allowed_chars for c in expression):
            raise ValueError("不正な文字が含まれています")
        result = eval(expression)  # noqa: S307
        return {"expression": expression, "result": result}

    async def _get_datetime(self, params: dict[str, Any]) -> dict[str, Any]:
        """日時取得ハンドラー."""
        fmt = params.get("format", "%Y-%m-%d %H:%M:%S")
        now = datetime.now()
        return {
            "formatted": now.strftime(fmt),
            "iso": now.isoformat(),
            "timestamp": now.timestamp(),
        }


class UnifiedToolProvider:
    """統一ツールプロバイダー.

    Skills/MCP/内蔵ツールを統一インターフェースで管理。
    URIスキームで自動ルーティング。

    URI形式:
    - skill://skill_name - Skillsツール
    - mcp://server/tool - MCPツール
    - builtin://tool_name - 内蔵ツール

    Example:
        >>> provider = UnifiedToolProvider()
        >>> await provider.initialize()
        >>>
        >>> # Skills
        >>> result = await provider.call("skill://web_scraper", {"query": "..."})
        >>>
        >>> # MCP
        >>> result = await provider.call("mcp://browser/scrape", {"url": "..."})
        >>>
        >>> # 内蔵
        >>> result = await provider.call("builtin://calculator", {"expression": "1+1"})
    """

    def __init__(self) -> None:
        """初期化."""
        self._skill_provider = SkillToolProvider()
        self._mcp_provider = MCPToolProvider()
        self._builtin_provider = BuiltinToolProvider()
        self._custom_providers: dict[str, ToolProvider] = {}
        self._initialized = False
        self._logger = logging.getLogger(__name__)

    async def initialize(self, mcp_config: Any = None) -> None:
        """全プロバイダーを初期化.

        Args:
            mcp_config: MCP設定（オプション）
        """
        self._logger.info("統一ツールプロバイダーを初期化中...")

        # 各プロバイダーを初期化
        await self._skill_provider.initialize()
        await self._mcp_provider.initialize(mcp_config)
        # builtin_providerは初期化不要

        self._initialized = True
        self._logger.info("統一ツールプロバイダー初期化完了")

    async def call(
        self,
        tool_uri: str,
        params: dict[str, Any],
    ) -> ToolResult:
        """ツールを呼び出す.

        URIスキームで自動ルーティング。

        Args:
            tool_uri: ツールURI（例: skill://name, mcp://server/tool）
            params: パラメータ

        Returns:
            ToolResult
        """
        start_time = datetime.now()

        # URIをパース
        tool_type, tool_name = self._parse_uri(tool_uri)

        self._logger.debug(f"ツール呼び出し: {tool_uri} (type={tool_type})")

        # 適切なプロバイダーにルーティング
        if tool_type == ToolType.SKILL:
            return await self._skill_provider.call(tool_name, params)
        elif tool_type == ToolType.MCP:
            return await self._mcp_provider.call(tool_name, params)
        elif tool_type == ToolType.BUILTIN:
            return await self._builtin_provider.call(tool_name, params)
        elif tool_type == ToolType.CUSTOM and tool_name in self._custom_providers:
            provider = self._custom_providers[tool_name.split("/")[0]]
            return await provider.call(tool_name, params)
        else:
            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            return ToolResult(
                success=False,
                status=ToolStatus.NOT_FOUND,
                tool_uri=tool_uri,
                tool_type=tool_type,
                error=f"不明なツール種別またはツール: {tool_uri}",
                duration_ms=duration_ms,
            )

    def _parse_uri(self, uri: str) -> tuple[ToolType, str]:
        """URIをパースしてツール種別と名前を取得.

        Args:
            uri: ツールURI

        Returns:
            (ToolType, tool_name)
        """
        if uri.startswith("skill://"):
            return ToolType.SKILL, uri[8:]
        elif uri.startswith("mcp://"):
            return ToolType.MCP, uri  # MCPは完全URIを使用
        elif uri.startswith("builtin://"):
            return ToolType.BUILTIN, uri[10:]
        elif uri.startswith("custom://"):
            return ToolType.CUSTOM, uri[9:]
        else:
            # スキームがない場合はbuiltin扱い
            return ToolType.BUILTIN, uri

    def register_custom_provider(
        self,
        name: str,
        provider: ToolProvider,
    ) -> None:
        """カスタムプロバイダーを登録.

        Args:
            name: プロバイダー名
            provider: プロバイダーインスタンス
        """
        self._custom_providers[name] = provider
        self._logger.info(f"カスタムプロバイダー登録: {name}")

    def list_all_tools(self) -> list[ToolDefinition]:
        """全ての利用可能なツールを一覧取得.

        Returns:
            ツール定義リスト
        """
        all_tools: list[ToolDefinition] = []
        all_tools.extend(self._skill_provider.list_tools())
        all_tools.extend(self._mcp_provider.list_tools())
        all_tools.extend(self._builtin_provider.list_tools())

        for provider in self._custom_providers.values():
            all_tools.extend(provider.list_tools())

        return all_tools

    def get_tool(self, tool_uri: str) -> ToolDefinition | None:
        """ツール定義を取得.

        Args:
            tool_uri: ツールURI

        Returns:
            ToolDefinition or None
        """
        tool_type, tool_name = self._parse_uri(tool_uri)

        if tool_type == ToolType.SKILL:
            return self._skill_provider.get_tool(tool_name)
        elif tool_type == ToolType.MCP:
            return self._mcp_provider.get_tool(tool_name)
        elif tool_type == ToolType.BUILTIN:
            return self._builtin_provider.get_tool(tool_name)
        elif tool_type == ToolType.CUSTOM:
            provider_name = tool_name.split("/")[0]
            if provider_name in self._custom_providers:
                return self._custom_providers[provider_name].get_tool(tool_name)

        return None

    def get_tools_for_llm(self) -> list[dict[str, Any]]:
        """LLM用のツール定義を取得.

        OpenAI Function Calling形式で返す。

        Returns:
            ツール定義リスト
        """
        tools = []
        for tool_def in self.list_all_tools():
            if not tool_def.enabled:
                continue

            tools.append({
                "type": "function",
                "function": {
                    "name": tool_def.uri,
                    "description": tool_def.description,
                    "parameters": tool_def.parameters,
                },
            })

        return tools

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "initialized": self._initialized,
            "skill_tools": len(self._skill_provider.list_tools()),
            "mcp_tools": len(self._mcp_provider.list_tools()),
            "builtin_tools": len(self._builtin_provider.list_tools()),
            "custom_providers": list(self._custom_providers.keys()),
            "total_tools": len(self.list_all_tools()),
        }

    async def close(self) -> None:
        """リソースを解放."""
        # MCPクライアントを切断
        if self._mcp_provider._client:
            await self._mcp_provider._client.disconnect()

        self._initialized = False
        self._logger.info("統一ツールプロバイダーを終了しました")

    async def __aenter__(self) -> "UnifiedToolProvider":
        """非同期コンテキストマネージャー."""
        await self.initialize()
        return self

    async def __aexit__(self, *args: Any) -> None:
        """非同期コンテキストマネージャー終了."""
        await self.close()


# エクスポート
__all__ = [
    "ToolType",
    "ToolStatus",
    "ToolResult",
    "ToolDefinition",
    "ToolProvider",
    "SkillToolProvider",
    "MCPToolProvider",
    "BuiltinToolProvider",
    "UnifiedToolProvider",
]
