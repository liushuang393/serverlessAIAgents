"""統一ツールカタログマネージャー.

全ツールソース(Skills/MCP/内蔵/OS・Browser)を一元管理し、
Agent に永続的にロードされるツールカタログを提供する。

設計原則:
- 単一エントリポイント: 全ツールソースを1つのカタログに統合
- メタデータ優先: 軽量メタ情報を常時保持、フル定義は遅延読み込み
- LLM最適化: ツール説明をLLM呼び出し率100%を目指して最適化
- シングルトン: プロセスあたり1カタログインスタンス

アーキテクチャ層: Core層(Agent層とProvider層の橋渡し)

使用例:
    >>> catalog = await get_tool_catalog()
    >>> entries = catalog.list_catalog()
    >>> llm_tools = catalog.get_for_llm()
    >>> results = catalog.search("ファイル読み取り")
"""

from __future__ import annotations

import logging
import threading
from dataclasses import dataclass, field
from enum import StrEnum
from typing import Any


class CatalogSource(StrEnum):
    """ツールカタログのソース種別."""

    SKILL = "skill"  # SKILL.md ビルトインスキル
    GATEWAY = "gateway"  # OS/Browser ゲートウェイスキル
    MCP = "mcp"  # MCP サーバーツール
    BUILTIN = "builtin"  # フレームワーク内蔵ツール(calculator等)
    DECORATOR = "decorator"  # @tool デコレータ関数


@dataclass
class CatalogEntry:
    """ツールカタログエントリ(軽量メタ情報).

    全ツールが統一された形式で表現される。

    Attributes:
        uri: 一意識別子(tool://source/name)
        name: ツール名
        description: LLM向け説明(50文字以内推奨)
        source: ツールソース種別
        category: 分類カテゴリ
        tags: 検索用タグ
        available: 現在利用可能か
        input_schema: 入力パラメータのJSONスキーマ（LLM Function Calling用）
    """

    uri: str
    name: str
    description: str
    source: CatalogSource
    category: str = ""
    tags: list[str] = field(default_factory=list)
    available: bool = True
    input_schema: dict[str, Any] = field(
        default_factory=lambda: {
            "type": "object",
            "properties": {},
        }
    )

    def matches(self, query: str) -> float:
        """クエリとのマッチスコアを計算.

        Args:
            query: 検索クエリ

        Returns:
            0.0〜1.0 のマッチスコア
        """
        q = query.lower()
        score = 0.0
        if q in self.name.lower():
            score += 0.5
        if q in self.description.lower():
            score += 0.3
        if any(q in t.lower() for t in self.tags):
            score += 0.2
        return min(score, 1.0)


class ToolCatalogManager:
    """統一ツールカタログマネージャー.

    全ツールソースを集約し、Agent に永続ロードするカタログを提供。

    統合対象:
    - 25+ SKILL.md ビルトインスキル(rag, chatbot, code_analysis 等)
    - 20+ OS/Browser ゲートウェイスキル(read_file, run_command 等)
    - MCP サーバーツール(LazyMCPClient 経由)
    - 内蔵ツール(calculator, file_reader, json_parser, datetime)
    - @tool デコレータ関数

    Example:
        >>> catalog = ToolCatalogManager()
        >>> await catalog.initialize()
        >>> entries = catalog.list_catalog()
        >>> llm_tools = catalog.get_for_llm()
    """

    def __init__(self) -> None:
        """初期化."""
        self._entries: dict[str, CatalogEntry] = {}
        self._initialized = False
        self._lock = threading.Lock()
        self._logger = logging.getLogger(__name__)

    @property
    def initialized(self) -> bool:
        """初期化済みか."""
        return self._initialized

    async def initialize(self) -> None:
        """全ツールソースからカタログを構築.

        初期化順序:
        1. SKILL.md ビルトインスキル
        2. OS/Browser ゲートウェイスキル
        3. 内蔵ツール(calculator等)
        4. @tool デコレータ関数
        5. MCP サーバーツール(設定がある場合)
        """
        if self._initialized:
            return

        self._logger.info("ToolCatalog: 初期化開始...")

        self._load_builtin_skills()
        self._load_gateway_skills()
        self._load_framework_builtins()
        self._load_decorator_tools()

        self._initialized = True
        self._logger.info(
            "ToolCatalog: 初期化完了 - %d 件登録",
            len(self._entries),
        )

    def _load_builtin_skills(self) -> None:
        """SKILL.md ビルトインスキルをロード."""
        try:
            from pathlib import Path

            from kernel.skills.loader import SkillLoader

            loader = SkillLoader()
            import kernel as _af

            builtin_dir = Path(_af.__file__).parent / "skills" / "builtin"
            if not builtin_dir.exists():
                self._logger.warning("ToolCatalog: ビルトインSkillディレクトリ未検出")
                return

            skills = loader.load_directory(builtin_dir, recursive=True)
            for skill in skills:
                meta = skill.metadata
                # スキルメタデータから input_schema を取得
                skill_schema = getattr(meta, "input_schema", None) or {
                    "type": "object",
                    "properties": {
                        "query": {
                            "type": "string",
                            "description": f"{meta.name} への入力",
                        },
                    },
                }
                entry = CatalogEntry(
                    uri=f"tool://skill/{meta.name}",
                    name=meta.name,
                    description=meta.description or f"{meta.name} スキル",
                    source=CatalogSource.SKILL,
                    category="skill",
                    tags=list(meta.tags) if hasattr(meta, "tags") and meta.tags else [],
                    available=True,
                    input_schema=skill_schema,
                )
                self._register(entry)

            self._logger.info("ToolCatalog: Skills %d 件ロード", len(skills))
        except Exception as e:
            self._logger.warning("ToolCatalog: Skills ロード失敗: %s", e)

    def _load_gateway_skills(self) -> None:
        """OS/Browser ゲートウェイスキルのメタ情報を登録.

        SkillGateway の実体を生成せず、メタ情報のみカタログに登録。
        """
        # OS/Browser スキルの静的メタ情報(factory.py の登録内容と同期)
        # 各スキルに適切な input_schema を定義
        _path_schema: dict[str, Any] = {
            "type": "object",
            "properties": {"path": {"type": "string", "description": "ファイルパス"}},
            "required": ["path"],
        }
        _write_schema: dict[str, Any] = {
            "type": "object",
            "properties": {
                "path": {"type": "string", "description": "ファイルパス"},
                "content": {"type": "string", "description": "書き込み内容"},
            },
            "required": ["path", "content"],
        }
        _cmd_schema: dict[str, Any] = {
            "type": "object",
            "properties": {"command": {"type": "string", "description": "実行コマンド"}},
            "required": ["command"],
        }
        _url_schema: dict[str, Any] = {
            "type": "object",
            "properties": {"url": {"type": "string", "description": "URL"}},
            "required": ["url"],
        }
        _selector_schema: dict[str, Any] = {
            "type": "object",
            "properties": {"selector": {"type": "string", "description": "要素セレクタ"}},
            "required": ["selector"],
        }
        _empty_schema: dict[str, Any] = {"type": "object", "properties": {}}

        # Each tuple: (name, description, category, input_schema)
        gateway_skills: list[tuple[str, str, str, dict[str, Any]]] = [
            ("read_file", "ファイルを読み込む", "os_read", _path_schema),
            ("write_file", "ファイルを書き込む", "os_write", _write_schema),
            ("list_files", "ファイル一覧を取得", "os_read", _path_schema),
            (
                "search_files",
                "ファイルを検索",
                "os_read",
                {
                    "type": "object",
                    "properties": {
                        "pattern": {"type": "string", "description": "検索パターン"},
                        "path": {"type": "string", "description": "検索ディレクトリ"},
                    },
                    "required": ["pattern"],
                },
            ),
            ("delete_file", "ファイルを削除", "os_write", _path_schema),
            ("run_command", "コマンドを実行", "os_command", _cmd_schema),
            ("run_background", "バックグラウンド実行", "os_command", _cmd_schema),
            (
                "kill_process",
                "プロセスを終了",
                "os_process",
                {
                    "type": "object",
                    "properties": {"pid": {"type": "integer", "description": "プロセスID"}},
                    "required": ["pid"],
                },
            ),
            ("get_process_list", "プロセス一覧を取得", "os_process", _empty_schema),
            (
                "http_request",
                "HTTPリクエスト送信",
                "os_network",
                {
                    "type": "object",
                    "properties": {
                        "url": {"type": "string", "description": "リクエストURL"},
                        "method": {"type": "string", "description": "HTTPメソッド"},
                    },
                    "required": ["url"],
                },
            ),
            (
                "socket_connect",
                "ソケット接続",
                "os_network",
                {
                    "type": "object",
                    "properties": {
                        "host": {"type": "string", "description": "ホスト"},
                        "port": {"type": "integer", "description": "ポート"},
                    },
                    "required": ["host", "port"],
                },
            ),
            ("get_system_info", "システム情報を取得", "os_system", _empty_schema),
            ("get_cpu_info", "CPU情報を取得", "os_system", _empty_schema),
            ("get_memory_info", "メモリ情報を取得", "os_system", _empty_schema),
            ("get_disk_info", "ディスク情報を取得", "os_system", _empty_schema),
            ("browser_navigate", "ブラウザでURLを開く", "browser", _url_schema),
            ("browser_click", "要素をクリック", "browser", _selector_schema),
            (
                "browser_type",
                "テキストを入力",
                "browser",
                {
                    "type": "object",
                    "properties": {
                        "selector": {"type": "string", "description": "要素セレクタ"},
                        "text": {"type": "string", "description": "入力テキスト"},
                    },
                    "required": ["selector", "text"],
                },
            ),
            ("browser_get_text", "要素のテキストを取得", "browser", _selector_schema),
            ("browser_screenshot", "スクリーンショットを取得", "browser", _empty_schema),
        ]

        for name, desc, category, schema in gateway_skills:
            entry = CatalogEntry(
                uri=f"tool://gateway/{name}",
                name=name,
                description=desc,
                source=CatalogSource.GATEWAY,
                category=category,
                tags=[category.split("_")[0] if "_" in category else category],
                available=True,
                input_schema=schema,
            )
            self._register(entry)

        self._logger.info(
            "ToolCatalog: Gateway スキル %d 件登録",
            len(gateway_skills),
        )

    def _load_framework_builtins(self) -> None:
        """フレームワーク内蔵ツール(calculator, file_reader 等)を登録."""
        # Each tuple: (name, description, input_schema)
        builtins: list[tuple[str, str, dict[str, Any]]] = [
            (
                "file_reader",
                "ファイルの内容を読み取る",
                {
                    "type": "object",
                    "properties": {"path": {"type": "string", "description": "ファイルパス"}},
                    "required": ["path"],
                },
            ),
            (
                "json_parser",
                "JSON文字列を解析する",
                {
                    "type": "object",
                    "properties": {"json_string": {"type": "string", "description": "JSON文字列"}},
                    "required": ["json_string"],
                },
            ),
            (
                "calculator",
                "数式を計算する",
                {
                    "type": "object",
                    "properties": {"expression": {"type": "string", "description": "数式"}},
                    "required": ["expression"],
                },
            ),
            (
                "datetime",
                "現在日時を取得する",
                {
                    "type": "object",
                    "properties": {"format": {"type": "string", "description": "日時フォーマット"}},
                },
            ),
        ]

        for name, desc, schema in builtins:
            entry = CatalogEntry(
                uri=f"tool://builtin/{name}",
                name=name,
                description=desc,
                source=CatalogSource.BUILTIN,
                category="utility",
                tags=["builtin", "utility"],
                available=True,
                input_schema=schema,
            )
            self._register(entry)

        self._logger.info("ToolCatalog: 内蔵ツール %d 件登録", len(builtins))

    def _load_decorator_tools(self) -> None:
        """@tool デコレータで登録されたツールを発見."""
        try:
            from infrastructure.sandbox.tool_provider import _tool_registry

            for name, registered in _tool_registry.items():
                desc = registered.description or ""
                if not desc and callable(registered.func) and registered.func.__doc__:
                    desc = registered.func.__doc__.strip().split("\n")[0]

                # RegisteredTool.parameters からスキーマを取得
                schema = (
                    registered.parameters
                    if registered.parameters
                    else {
                        "type": "object",
                        "properties": {},
                    }
                )
                entry = CatalogEntry(
                    uri=f"tool://decorator/{name}",
                    name=name,
                    description=desc or f"{name} ツール",
                    source=CatalogSource.DECORATOR,
                    category="custom",
                    tags=["custom", "decorator"],
                    available=True,
                    input_schema=schema,
                )
                self._register(entry)

            self._logger.info(
                "ToolCatalog: @tool デコレータ %d 件発見",
                len(_tool_registry),
            )
        except Exception as e:
            self._logger.debug("ToolCatalog: @tool デコレータ発見スキップ: %s", e)

    async def load_mcp_tools(self, mcp_config: Any = None) -> int:
        """MCP サーバーツールをカタログに追加(オプション).

        Args:
            mcp_config: MCPConfig インスタンス

        Returns:
            登録されたMCPツール数
        """
        if mcp_config is None:
            return 0

        count = 0
        try:
            from kernel.protocols.mcp_lazy_client import LazyMCPClient

            client = LazyMCPClient(mcp_config)
            await client.connect()

            index = client.get_tool_index()
            for tool_entry in index:
                t_name = str(tool_entry.get("name", ""))
                t_server = str(tool_entry.get("server", "unknown"))
                t_desc = str(tool_entry.get("description", ""))
                # MCP ツールの inputSchema を取得
                t_schema = tool_entry.get(
                    "inputSchema",
                    {
                        "type": "object",
                        "properties": {},
                    },
                )
                input_schema = t_schema if isinstance(t_schema, dict) else {"type": "object", "properties": {}}
                entry = CatalogEntry(
                    uri=f"tool://mcp/{t_server}/{t_name}",
                    name=t_name,
                    description=t_desc or f"MCP: {t_name}",
                    source=CatalogSource.MCP,
                    category="mcp",
                    tags=["mcp", t_server],
                    available=True,
                    input_schema=input_schema,
                )
                self._register(entry)
                count += 1

            self._logger.info("ToolCatalog: MCP ツール %d 件登録", count)
        except Exception as e:
            self._logger.warning("ToolCatalog: MCP ツールロード失敗: %s", e)

        return count

    # ========== 公開API ==========

    def _register(self, entry: CatalogEntry) -> None:
        """エントリをカタログに登録(重複時は上書き).

        Args:
            entry: カタログエントリ
        """
        with self._lock:
            self._entries[entry.uri] = entry

    def list_catalog(self) -> list[CatalogEntry]:
        """全カタログエントリを取得.

        Returns:
            全エントリのリスト
        """
        return list(self._entries.values())

    def get(self, uri: str) -> CatalogEntry | None:
        """URI でエントリを取得.

        Args:
            uri: ツールURI

        Returns:
            CatalogEntry または None
        """
        return self._entries.get(uri)

    def search(self, query: str, limit: int = 10) -> list[CatalogEntry]:
        """クエリでカタログを検索.

        Args:
            query: 検索クエリ
            limit: 最大結果数

        Returns:
            マッチスコア順のエントリリスト
        """
        if not query.strip():
            return self.list_catalog()[:limit]

        scored = [(e, e.matches(query)) for e in self._entries.values()]
        scored = [(e, s) for e, s in scored if s > 0]
        scored.sort(key=lambda x: x[1], reverse=True)
        return [e for e, _ in scored[:limit]]

    def get_by_source(self, source: CatalogSource) -> list[CatalogEntry]:
        """ソース種別でフィルタ.

        Args:
            source: ツールソース種別

        Returns:
            指定ソースのエントリリスト
        """
        return [e for e in self._entries.values() if e.source == source]

    def get_by_category(self, category: str) -> list[CatalogEntry]:
        """カテゴリでフィルタ.

        Args:
            category: カテゴリ名

        Returns:
            指定カテゴリのエントリリスト
        """
        return [e for e in self._entries.values() if e.category == category]

    def get_for_llm(self) -> list[dict[str, Any]]:
        """LLM 用ツール定義を取得(OpenAI Function Calling 形式).

        各エントリの input_schema を使用してパラメータ定義を生成する。
        input_schema が空の場合はデフォルトの汎用スキーマにフォールバック。

        Returns:
            LLM に渡すツール定義リスト
        """
        tools: list[dict[str, Any]] = []
        for entry in self._entries.values():
            if not entry.available:
                continue
            # input_schema が有効な場合はそのまま使用、空の場合はフォールバック
            params = entry.input_schema
            if not params or not params.get("properties"):
                params = {
                    "type": "object",
                    "properties": {
                        "query": {
                            "type": "string",
                            "description": f"{entry.name} への入力",
                        },
                    },
                }
            tools.append(
                {
                    "type": "function",
                    "function": {
                        "name": entry.name,
                        "description": entry.description,
                        "parameters": params,
                    },
                }
            )
        return tools

    def get_stats(self) -> dict[str, Any]:
        """カタログ統計情報を取得.

        Returns:
            統計情報辞書
        """
        by_source: dict[str, int] = {}
        by_category: dict[str, int] = {}
        for entry in self._entries.values():
            by_source[entry.source.value] = by_source.get(entry.source.value, 0) + 1
            if entry.category:
                by_category[entry.category] = by_category.get(entry.category, 0) + 1

        return {
            "initialized": self._initialized,
            "total": len(self._entries),
            "by_source": by_source,
            "by_category": by_category,
            "available": sum(1 for e in self._entries.values() if e.available),
        }


# ========== シングルトンアクセサ ==========

_global_catalog: ToolCatalogManager | None = None
_catalog_lock = threading.Lock()


async def get_tool_catalog() -> ToolCatalogManager:
    """グローバルツールカタログを取得(シングルトン).

    未初期化の場合は自動初期化する。

    Returns:
        ToolCatalogManager インスタンス
    """
    global _global_catalog
    if _global_catalog is None:
        with _catalog_lock:
            if _global_catalog is None:
                _global_catalog = ToolCatalogManager()
                await _global_catalog.initialize()
    return _global_catalog


def reset_tool_catalog() -> None:
    """グローバルツールカタログをリセット(テスト用)."""
    global _global_catalog
    _global_catalog = None


__all__ = [
    "CatalogEntry",
    "CatalogSource",
    "ToolCatalogManager",
    "get_tool_catalog",
    "reset_tool_catalog",
]
