"""MCP クライアント実装.

このモジュールは複数の MCP サーバーに接続し、ツールを呼び出すためのクライアントを提供します。
"""

import asyncio
import logging
from typing import Any

from mcp.client.stdio import stdio_client

from agentflow.core.security import AuditLogger, ParameterValidator, ToolWhitelist
from agentflow.protocols.mcp_config import MCPConfig, MCPServerConfig
from mcp import ClientSession, StdioServerParameters


class ToolNotAllowedError(Exception):
    """工具がホワイトリストにない場合のエラー."""


class ToolValidationError(Exception):
    """工具パラメータ検証エラー."""


class MCPClient:
    """複数の MCP サーバーを管理するクライアント（セキュリティ強化版）.

    このクラスは複数の MCP サーバーに接続し、ツールの呼び出しを管理します。
    業界最佳実践に基づいた以下のセキュリティ機能を提供:
    - ツールホワイトリスト検証
    - 審計ログ記録
    - パラメータ検証
    - リトライとタイムアウト制御

    Example:
        >>> config = MCPConfig(servers=[...])
        >>> client = MCPClient(config)
        >>> await client.connect()
        >>> result = await client.call_tool("mcp://server/tool", {"arg": "value"})
    """

    def __init__(
        self,
        config: MCPConfig,
        *,
        logger: logging.Logger | None = None,
        whitelist: ToolWhitelist | None = None,
        audit_logger: AuditLogger | None = None,
        validator: ParameterValidator | None = None,
        max_retries: int = 3,
        timeout: float = 30.0,
        enable_security: bool = True,
    ) -> None:
        """MCP クライアントを初期化.

        Args:
            config: MCP 設定
            logger: ロガーインスタンス (オプション)
            whitelist: ツールホワイトリスト (オプション、None の場合はデフォルト)
            audit_logger: 審計ログ記録器 (オプション、None の場合はデフォルト)
            validator: パラメータ検証器 (オプション、None の場合はデフォルト)
            max_retries: 最大リトライ回数 (デフォルト: 3)
            timeout: タイムアウト時間（秒）(デフォルト: 30.0)
            enable_security: セキュリティ機能を有効にするか (デフォルト: True)
        """
        self._config = config
        self._logger = logger or logging.getLogger(__name__)
        self._sessions: dict[str, ClientSession] = {}
        self._tools: dict[str, dict[str, Any]] = {}
        self._contexts: dict[str, Any] = {}  # stdio_client コンテキストを保持

        # セキュリティコンポーネント
        self._enable_security = enable_security
        self._whitelist = whitelist or ToolWhitelist()
        self._audit_logger = audit_logger or AuditLogger(logger=self._logger)
        self._validator = validator or ParameterValidator()
        self._max_retries = max_retries
        self._timeout = timeout

    async def connect(self) -> None:
        """すべての有効な MCP サーバーに接続.

        Raises:
            RuntimeError: サーバー接続に失敗した場合
        """
        self._logger.info(f"Connecting to {len(self._config.servers)} MCP servers")

        for server_config in self._config.servers:
            if not server_config.enabled:
                self._logger.debug(f"Skipping disabled server: {server_config.name}")
                continue

            try:
                await self._connect_server(server_config)
            except Exception:
                self._logger.exception(f"Failed to connect to server {server_config.name}")
                # サーバー接続失敗は致命的ではない
                continue

        self._logger.info(f"Connected to {len(self._sessions)} MCP servers")

    async def _connect_server(self, config: MCPServerConfig) -> None:
        """単一の MCP サーバーに接続.

        Args:
            config: サーバー設定

        Raises:
            Exception: 接続に失敗した場合
        """
        self._logger.debug(f"Connecting to server: {config.name}")

        # StdioServerParameters を作成
        server_params = StdioServerParameters(
            command=config.command,
            args=config.args,
            env=config.env if config.env else None,
        )

        # stdio クライアントを使用して接続
        # コンテキストマネージャーを保持して接続を維持
        context = stdio_client(server_params)
        read, write = await context.__aenter__()

        session = ClientSession(read, write)

        # セッションを初期化
        await session.initialize()

        # セッションとコンテキストを保存
        self._sessions[config.name] = session
        self._contexts[config.name] = context

        # ツールリストを取得
        tools_result = await session.list_tools()
        for tool in tools_result.tools:
            tool_uri = f"mcp://{config.name}/{tool.name}"
            self._tools[tool_uri] = {
                "name": tool.name,
                "description": tool.description,
                "input_schema": tool.inputSchema,
                "server": config.name,
            }

        self._logger.info(f"Connected to {config.name}, loaded {len(tools_result.tools)} tools")

    async def disconnect(self) -> None:
        """すべての MCP サーバーから切断."""
        self._logger.info("Disconnecting from all MCP servers")

        for server_name in self._sessions:
            try:
                # コンテキストマネージャーを終了
                if server_name in self._contexts:
                    context = self._contexts[server_name]
                    await context.__aexit__(None, None, None)
                self._logger.debug(f"Disconnected from server: {server_name}")
            except Exception:
                self._logger.exception(f"Error disconnecting from {server_name}")

        self._sessions.clear()
        self._tools.clear()
        self._contexts.clear()
        self._logger.info("Disconnected from all servers")

    def get_tool_definitions(self) -> list[dict[str, Any]]:
        """LLM 用のツール定義を取得.

        Returns:
            ツール定義のリスト
        """
        return [
            {
                "type": "function",
                "function": {
                    "name": tool_uri,
                    "description": tool_info["description"] or "",
                    "parameters": tool_info["input_schema"],
                },
            }
            for tool_uri, tool_info in self._tools.items()
        ]

    async def call_tool(
        self,
        tool_uri: str,
        arguments: dict[str, Any],
        user_id: str = "system",
    ) -> dict[str, Any]:
        """ツールを呼び出す（セキュリティチェック付き）.

        実行フロー:
        1. ホワイトリストチェック
        2. パラメータ検証
        3. リトライ付き実行
        4. 審計ログ記録

        Args:
            tool_uri: ツール URI (例: "mcp://server/tool")
            arguments: ツール引数
            user_id: ユーザー ID（審計ログ用）

        Returns:
            ツール実行結果

        Raises:
            ToolNotAllowedError: ツールがホワイトリストにない場合
            ToolValidationError: パラメータ検証失敗
            ValueError: ツール URI が無効な場合
            RuntimeError: ツール呼び出しに失敗した場合
        """
        # 1. ホワイトリストチェック
        if self._enable_security and not self._whitelist.is_allowed(tool_uri):
            error_msg = f"Tool not in whitelist: {tool_uri}"
            self._audit_logger.log_tool_call(
                user_id=user_id,
                tool_uri=tool_uri,
                parameters=arguments,
                result=None,
                success=False,
                error=error_msg,
            )
            raise ToolNotAllowedError(error_msg)

        # 2. URI をパース
        if not tool_uri.startswith("mcp://"):
            msg = f"Invalid tool URI: {tool_uri}"
            raise ValueError(msg)

        if tool_uri not in self._tools:
            msg = f"Tool not found: {tool_uri}"
            raise ValueError(msg)

        tool_info = self._tools[tool_uri]
        server_name = tool_info["server"]
        tool_name = tool_info["name"]

        # 3. パラメータ検証
        if self._enable_security:
            schema = tool_info.get("input_schema", {})
            is_valid, validation_error = self._validator.validate(schema, arguments)
            if not is_valid:
                self._audit_logger.log_tool_call(
                    user_id=user_id,
                    tool_uri=tool_uri,
                    parameters=arguments,
                    result=None,
                    success=False,
                    error=validation_error,
                )
                raise ToolValidationError(validation_error or "Parameter validation failed")

        # 4. リトライ付き実行
        return await self._call_tool_with_retry(
            tool_uri=tool_uri,
            tool_name=tool_name,
            server_name=server_name,
            arguments=arguments,
            user_id=user_id,
        )

    async def _call_tool_with_retry(
        self,
        tool_uri: str,
        tool_name: str,
        server_name: str,
        arguments: dict[str, Any],
        user_id: str,
    ) -> dict[str, Any]:
        """リトライ付きでツールを呼び出す.

        Args:
            tool_uri: ツール URI
            tool_name: ツール名
            server_name: サーバー名
            arguments: ツール引数
            user_id: ユーザー ID

        Returns:
            ツール実行結果
        """
        if server_name not in self._sessions:
            msg = f"Server not connected: {server_name}"
            raise RuntimeError(msg)

        session = self._sessions[server_name]
        last_error: Exception | None = None

        # 指数バックオフでリトライ
        for attempt in range(self._max_retries):
            try:
                # タイムアウト付きで実行
                async with asyncio.timeout(self._timeout):
                    result = await session.call_tool(tool_name, arguments)

                # 成功時の審計ログ
                self._audit_logger.log_tool_call(
                    user_id=user_id,
                    tool_uri=tool_uri,
                    parameters=arguments,
                    result=result.content,
                    success=True,
                )

                return {
                    "success": True,
                    "result": result.content,
                    "tool": tool_name,
                    "server": server_name,
                }

            except TimeoutError as e:
                last_error = e
                self._logger.warning(f"Tool call timeout (attempt {attempt + 1}/{self._max_retries}): {tool_uri}")
                if attempt < self._max_retries - 1:
                    wait_time = 2**attempt  # 指数バックオフ
                    await asyncio.sleep(wait_time)

            except Exception as e:
                last_error = e
                self._logger.warning(
                    f"Tool call failed (attempt {attempt + 1}/{self._max_retries}): {tool_uri}",
                    exc_info=True,
                )
                if attempt < self._max_retries - 1:
                    wait_time = 2**attempt  # 指数バックオフ
                    await asyncio.sleep(wait_time)

        # すべてのリトライが失敗
        error_msg = f"Tool call failed after {self._max_retries} attempts: {last_error}"
        self._audit_logger.log_tool_call(
            user_id=user_id,
            tool_uri=tool_uri,
            parameters=arguments,
            result=None,
            success=False,
            error=error_msg,
        )

        return {
            "success": False,
            "error": error_msg,
            "tool": tool_name,
            "server": server_name,
        }

    def list_tools(self) -> list[str]:
        """利用可能なツール URI のリストを取得.

        Returns:
            ツール URI のリスト
        """
        return list(self._tools.keys())

    def get_tool_info(self, tool_uri: str) -> dict[str, Any] | None:
        """ツールの詳細情報を取得.

        Args:
            tool_uri: ツール URI

        Returns:
            ツール情報、または存在しない場合は None
        """
        return self._tools.get(tool_uri)

    async def __aenter__(self) -> "MCPClient":
        """非同期コンテキストマネージャーのエントリー."""
        await self.connect()
        return self

    async def __aexit__(self, *args: Any) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.disconnect()
