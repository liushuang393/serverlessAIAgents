"""統一ツールレジストリ.

すべてのソースからのツールを一元管理するレジストリモジュール。

設計原則:
- 高度な抽象化: ツールソースに依存しない統一インターフェース
- 高凝集: ツール管理機能のみに責任を持つ
- 拡張性: 新しい検索・フィルタ機能の追加が容易

使用例:
    >>> # ツールを登録
    >>> registry = ToolRegistry()
    >>> registry.register(tool)
    >>>
    >>> # URIで取得
    >>> tool = registry.get("tool://mcp/filesystem/read_file")
    >>>
    >>> # クエリで検索
    >>> results = registry.search("ファイル")
    >>>
    >>> # ソースでフィルタ
    >>> mcp_tools = registry.filter_by_source(ToolSource.MCP)
"""

from __future__ import annotations

import threading
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.core.tool_definition import ToolDefinition, ToolSource


class ToolRegistry:
    """統一ツールレジストリ.

    すべてのソースからのツールに対して統一された発見・検索・管理を提供:
    - @tool デコレータで定義された関数
    - MCPサーバーツール
    - Skills
    - 動的生成ツール

    スレッドセーフな実装。

    Attributes:
        _tools: URI → ToolDefinition のマッピング
        _lock: スレッドセーフティのためのロック
    """

    def __init__(self) -> None:
        """空のレジストリを初期化."""
        self._tools: dict[str, ToolDefinition] = {}
        self._lock = threading.Lock()

    def register(self, tool: ToolDefinition) -> None:
        """ツール定義を登録.

        同じURIのツールが既に存在する場合は置換される。

        Args:
            tool: 登録するツール定義
        """
        with self._lock:
            self._tools[tool.uri] = tool

    def unregister(self, uri: str) -> bool:
        """レジストリからツールを削除.

        Args:
            uri: 削除するツールのURI

        Returns:
            ツールが削除された場合True、見つからない場合False
        """
        with self._lock:
            if uri in self._tools:
                del self._tools[uri]
                return True
            return False

    def get(self, uri: str) -> ToolDefinition | None:
        """URIでツールを取得.

        Args:
            uri: ツールURI

        Returns:
            ToolDefinition または見つからない場合None
        """
        return self._tools.get(uri)

    def list_all(self) -> list[ToolDefinition]:
        """登録された全ツールをリスト.

        Returns:
            全ツール定義のリスト
        """
        return list(self._tools.values())

    def search(self, query: str, limit: int = 10) -> list[ToolDefinition]:
        """クエリ文字列でツールを検索.

        ツール名と説明に対して検索を行い、
        関連性スコアでソートされた結果を返す。

        Args:
            query: 検索クエリ
            limit: 返す最大結果数

        Returns:
            関連性順にソートされたツールリスト
        """
        if not query:
            return self.list_all()[:limit]

        scored = [(tool, tool.matches(query)) for tool in self._tools.values()]
        scored = [(t, s) for t, s in scored if s > 0]
        scored.sort(key=lambda x: x[1], reverse=True)

        return [t for t, _ in scored[:limit]]

    def filter_by_source(self, source: ToolSource) -> list[ToolDefinition]:
        """ソースでツールをフィルタリング.

        Args:
            source: フィルタするToolSource

        Returns:
            指定されたソースからのツールリスト
        """
        return [t for t in self._tools.values() if t.source == source]

    def clear(self) -> None:
        """レジストリを完全にクリア."""
        with self._lock:
            self._tools.clear()

    def __len__(self) -> int:
        """登録ツール数を返す."""
        return len(self._tools)

    def __contains__(self, uri: str) -> bool:
        """ツールURIが登録されているかチェック."""
        return uri in self._tools


# =============================================================================
# グローバルレジストリ（シングルトン）
# =============================================================================

_global_tool_registry: ToolRegistry | None = None
_registry_lock = threading.Lock()


def get_global_tool_registry() -> ToolRegistry:
    """グローバルツールレジストリを取得または作成.

    シングルトンパターンでグローバルレジストリを管理。
    アプリケーション全体で同じレジストリを共有。

    Returns:
        グローバルToolRegistryインスタンス
    """
    global _global_tool_registry
    if _global_tool_registry is None:
        with _registry_lock:
            if _global_tool_registry is None:
                _global_tool_registry = ToolRegistry()
    return _global_tool_registry


def reset_global_tool_registry() -> None:
    """グローバルツールレジストリをリセット.

    主にテスト用。新しい空のレジストリを作成。
    """
    global _global_tool_registry
    with _registry_lock:
        _global_tool_registry = ToolRegistry()


__all__ = [
    "ToolRegistry",
    "get_global_tool_registry",
    "reset_global_tool_registry",
]
