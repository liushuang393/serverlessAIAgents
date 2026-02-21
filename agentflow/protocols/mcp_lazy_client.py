"""MCP 懒加載クライアント実装.

このモジュールは MCP ツールの懒加載（Lazy Loading）機能を提供します。
Claude Code の MCP Tool Search 機能を参考に、ツール定義を必要時のみロードし、
上下文 token 消費を大幅に削減します。

主要機能:
- ツールインデックス（軽量メタデータ）のみを初期ロード
- MCPSearch による関連ツールの検索
- 必要時のみ完全なツール定義をロード
- セッション内キャッシュで重複ロード回避

使用例:
    >>> config = MCPConfig(servers=[...])
    >>> client = LazyMCPClient(config, lazy_threshold=0.1)
    >>> await client.connect()
    >>> # インデックスのみ取得（軽量）
    >>> index = client.get_tool_index()
    >>> # 必要なツールを検索してロード
    >>> tools = client.search_tools("github issue")
    >>> # 完全なツール定義を取得
    >>> definitions = client.get_loaded_tool_definitions()
"""

import logging
import re
from dataclasses import dataclass, field
from typing import Any

from agentflow.core.security import AuditLogger, ParameterValidator, ToolWhitelist
from agentflow.protocols.mcp_client import MCPClient
from agentflow.protocols.mcp_config import MCPConfig


@dataclass
class ToolIndexEntry:
    """ツールインデックスエントリ（軽量メタデータ）.

    完全なスキーマをロードせず、名前と説明のみ保持。
    検索とフィルタリングに使用。

    Attributes:
        uri: ツール URI (例: "mcp://server/tool_name")
        name: ツール名
        description: ツールの説明（短い）
        server: 所属サーバー名
        keywords: 検索用キーワード（自動生成）
        loaded: 完全定義がロード済みかどうか
    """

    uri: str
    name: str
    description: str
    server: str
    keywords: list[str] = field(default_factory=list)
    loaded: bool = False

    def __post_init__(self) -> None:
        """キーワードを自動生成."""
        if not self.keywords:
            # 説明と名前からキーワードを抽出
            text = f"{self.name} {self.description}".lower()
            # 単語分割（アンダースコア、ハイフン、スペースで分割）
            words = re.split(r"[_\-\s/]+", text)
            self.keywords = [w for w in words if len(w) >= 2]


@dataclass
class ToolSearchResult:
    """ツール検索結果.

    Attributes:
        entries: マッチしたツールインデックスエントリ
        query: 検索クエリ
        loaded_count: ロードされたツール数
    """

    entries: list[ToolIndexEntry]
    query: str
    loaded_count: int = 0


class LazyMCPClient(MCPClient):
    """MCP 懒加載クライアント.

    MCPClient を拡張し、ツール定義の懒加載機能を追加。
    上下文 token 消費を削減するため、以下の戦略を採用:

    1. 初期接続時: インデックス（名前+説明）のみロード
    2. 検索時: 関連ツールのインデックスを返す
    3. 実行時: 必要なツールの完全定義をロード

    Example:
        >>> client = LazyMCPClient(config, lazy_threshold=0.1)
        >>> await client.connect()
        >>> # 検索してロード
        >>> results = client.search_tools("read file")
        >>> client.load_tools([r.uri for r in results.entries[:3]])
        >>> # 実行
        >>> await client.call_tool("mcp://filesystem/read_file", {...})
    """

    # 定数: インデックスサイズの閾値（この比率を超えると懒加載を有効化）
    DEFAULT_LAZY_THRESHOLD = 0.1  # 上下文の10%

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
        lazy_threshold: float = 0.1,
        enable_lazy_loading: bool = True,
    ) -> None:
        """初期化.

        Args:
            config: MCP 設定
            logger: ロガーインスタンス
            whitelist: ツールホワイトリスト
            audit_logger: 審計ログ記録器
            validator: パラメータ検証器
            max_retries: 最大リトライ回数
            timeout: タイムアウト時間（秒）
            enable_security: セキュリティ機能を有効にするか
            lazy_threshold: 懒加載を有効化する閾値（上下文の割合）
            enable_lazy_loading: 懒加載を有効にするか
        """
        super().__init__(
            config,
            logger=logger,
            whitelist=whitelist,
            audit_logger=audit_logger,
            validator=validator,
            max_retries=max_retries,
            timeout=timeout,
            enable_security=enable_security,
        )

        self._lazy_threshold = lazy_threshold
        self._enable_lazy_loading = enable_lazy_loading

        # インデックス（軽量メタデータ）
        self._tool_index: dict[str, ToolIndexEntry] = {}
        # ロード済みツール定義（完全スキーマ）
        self._loaded_tools: dict[str, dict[str, Any]] = {}
        # 検索履歴（デバッグ用）
        self._search_history: list[str] = []

    async def connect(self) -> None:
        """すべての有効な MCP サーバーに接続（懒加載対応）.

        親クラスの connect() を呼び出し、ツールインデックスを構築。
        完全なスキーマは _tools に保存されるが、LLM への送信は遅延。
        """
        await super().connect()

        # インデックスを構築
        self._build_tool_index()

        if self._enable_lazy_loading:
            self._logger.info(f"懒加載モード: {len(self._tool_index)} ツールをインデックス化")

    def _build_tool_index(self) -> None:
        """ツールインデックスを構築（軽量メタデータ抽出）."""
        for tool_uri, tool_info in self._tools.items():
            entry = ToolIndexEntry(
                uri=tool_uri,
                name=tool_info["name"],
                description=tool_info.get("description") or "",
                server=tool_info["server"],
            )
            self._tool_index[tool_uri] = entry

    def get_tool_index(self) -> list[dict[str, str]]:
        """LLM 用のツールインデックス（軽量版）を取得.

        完全なスキーマではなく、名前と説明のみを含む軽量リスト。
        これを system prompt に含めて検索の参考情報とする。

        Returns:
            ツールインデックスのリスト
        """
        return [
            {
                "uri": entry.uri,
                "name": entry.name,
                "description": entry.description[:100] if entry.description else "",
            }
            for entry in self._tool_index.values()
        ]

    def get_tool_index_prompt(self) -> str:
        """LLM 向けのツールインデックスプロンプトを生成.

        Returns:
            ツールインデックスを含むプロンプト文字列
        """
        if not self._tool_index:
            return ""

        lines = [
            "## 利用可能な MCP ツール（概要）",
            "以下のツールが利用可能です。必要なツールがあれば MCPSearch で検索してください。",
            "",
        ]

        for entry in self._tool_index.values():
            desc = (
                entry.description[:80] + "..." if len(entry.description) > 80 else entry.description
            )
            lines.append(f"- **{entry.name}** ({entry.server}): {desc}")

        lines.extend(
            [
                "",
                'ツールを使用する場合は `MCPSearch(query: "キーワード")` または',
                '`MCPSearch(query: "select:tool_name")` で検索してください。',
            ]
        )

        return "\n".join(lines)

    def search_tools(self, query: str) -> ToolSearchResult:
        """ツールを検索.

        Claude Code の MCPSearch 風のインターフェース。
        - 関連ツールを検索
        - 「select:tool_name」形式で直接指定可能

        Args:
            query: 検索クエリ（キーワードまたは select:tool_name）

        Returns:
            ToolSearchResult: 検索結果
        """
        self._search_history.append(query)
        query_lower = query.lower().strip()

        # select:tool_name 形式の直接指定
        if query_lower.startswith("select:"):
            tool_name = query_lower[7:].strip()
            return self._search_by_name(tool_name)

        # キーワード検索
        return self._search_by_keywords(query_lower)

    def _search_by_name(self, tool_name: str) -> ToolSearchResult:
        """ツール名で直接検索.

        Args:
            tool_name: ツール名（部分一致）

        Returns:
            検索結果
        """
        matches: list[ToolIndexEntry] = []

        for entry in self._tool_index.values():
            if tool_name in entry.name.lower() or tool_name in entry.uri.lower():
                matches.append(entry)

        return ToolSearchResult(
            entries=matches,
            query=f"select:{tool_name}",
            loaded_count=sum(1 for e in matches if e.loaded),
        )

    def _search_by_keywords(self, query: str) -> ToolSearchResult:
        """キーワードで検索.

        Args:
            query: 検索キーワード（スペース区切りで複数可）

        Returns:
            検索結果
        """
        keywords = [k.strip() for k in query.split() if k.strip()]
        if not keywords:
            return ToolSearchResult(entries=[], query=query)

        matches: list[tuple[ToolIndexEntry, int]] = []

        for entry in self._tool_index.values():
            score = 0
            for kw in keywords:
                # 名前に含まれる場合は高スコア
                if kw in entry.name.lower():
                    score += 3
                # 説明に含まれる場合
                elif kw in entry.description.lower():
                    score += 2
                # キーワードリストに含まれる場合
                elif any(kw in ek for ek in entry.keywords):
                    score += 1

            if score > 0:
                matches.append((entry, score))

        # スコア順でソート
        matches.sort(key=lambda x: x[1], reverse=True)

        return ToolSearchResult(
            entries=[m[0] for m in matches],
            query=query,
            loaded_count=sum(1 for m in matches if m[0].loaded),
        )

    def load_tools(self, tool_uris: list[str]) -> list[dict[str, Any]]:
        """指定されたツールの完全定義をロード.

        懒加載の核心メソッド。検索後に必要なツールのみをロード。

        Args:
            tool_uris: ロードするツール URI のリスト

        Returns:
            ロードされたツール定義のリスト
        """
        loaded: list[dict[str, Any]] = []

        for uri in tool_uris:
            if uri in self._loaded_tools:
                # 既にロード済み
                loaded.append(self._loaded_tools[uri])
                continue

            if uri not in self._tools:
                self._logger.warning(f"ツールが見つかりません: {uri}")
                continue

            # 完全定義をロード
            tool_info = self._tools[uri]
            definition = {
                "type": "function",
                "function": {
                    "name": uri,
                    "description": tool_info.get("description") or "",
                    "parameters": tool_info.get("input_schema", {}),
                },
            }

            self._loaded_tools[uri] = definition
            loaded.append(definition)

            # インデックスを更新
            if uri in self._tool_index:
                self._tool_index[uri].loaded = True

            self._logger.debug(f"ツール定義をロード: {uri}")

        return loaded

    def load_tool(self, tool_uri: str) -> dict[str, Any] | None:
        """単一ツールの完全定義をロード.

        Args:
            tool_uri: ツール URI

        Returns:
            ツール定義、または見つからない場合は None
        """
        result = self.load_tools([tool_uri])
        return result[0] if result else None

    def get_loaded_tool_definitions(self) -> list[dict[str, Any]]:
        """ロード済みツールの定義を取得（LLM 用）.

        懒加載モードでは、load_tools() でロードされたツールのみ返す。
        これにより上下文 token を大幅に削減。

        Returns:
            ロード済みツール定義のリスト
        """
        return list(self._loaded_tools.values())

    def get_tool_definitions(self) -> list[dict[str, Any]]:
        """LLM 用のツール定義を取得.

        懒加載が有効な場合はロード済みツールのみ返す。
        無効な場合は親クラスの動作（全ツール返却）を維持。

        Returns:
            ツール定義のリスト
        """
        if self._enable_lazy_loading:
            return self.get_loaded_tool_definitions()
        return super().get_tool_definitions()

    async def call_tool(
        self,
        tool_uri: str,
        arguments: dict[str, Any],
        user_id: str = "system",
    ) -> dict[str, Any]:
        """ツールを呼び出す（自動ロード対応）.

        ツールが未ロードの場合、自動的にロードしてから呼び出す。

        Args:
            tool_uri: ツール URI
            arguments: ツール引数
            user_id: ユーザー ID

        Returns:
            ツール実行結果
        """
        # 自動ロード（懒加載モードで未ロードの場合）
        if self._enable_lazy_loading and tool_uri not in self._loaded_tools:
            self.load_tool(tool_uri)

        return await super().call_tool(tool_uri, arguments, user_id)

    def get_stats(self) -> dict[str, Any]:
        """懒加載の統計情報を取得.

        Returns:
            統計情報（ツール数、ロード済み数、検索回数など）
        """
        total_tools = len(self._tool_index)
        loaded_tools = len(self._loaded_tools)
        load_ratio = loaded_tools / total_tools if total_tools > 0 else 0

        return {
            "total_tools": total_tools,
            "loaded_tools": loaded_tools,
            "load_ratio": load_ratio,
            "search_count": len(self._search_history),
            "lazy_loading_enabled": self._enable_lazy_loading,
            "lazy_threshold": self._lazy_threshold,
            # Token 削減量の推定（1ツール定義 ≈ 200 token と仮定）
            "estimated_token_saved": (total_tools - loaded_tools) * 200,
        }

    def clear_loaded_tools(self) -> None:
        """ロード済みツールをクリア（メモリ解放）."""
        self._loaded_tools.clear()
        for entry in self._tool_index.values():
            entry.loaded = False
        self._logger.info("ロード済みツールをクリアしました")

    async def disconnect(self) -> None:
        """切断処理（キャッシュクリア含む）."""
        self._tool_index.clear()
        self._loaded_tools.clear()
        self._search_history.clear()
        await super().disconnect()
