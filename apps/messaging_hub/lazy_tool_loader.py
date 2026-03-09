"""懒加載ツールローダーモジュール.

Skills と MCP ツールを統合的に管理し、懒加載戦略を適用する。
Claude Code の MCPSearch パターンを参考に、デフォルトで2-3個の
共通ツールのみロードし、残りはインデックス（名前+説明）として
保持。会話コンテキストに応じて必要時にオンデマンドロードする。

使用例:
    >>> loader = LazyToolLoader(mcp_manager, skills_manager)
    >>> index = loader.get_tool_index()
    >>> tools = loader.search_and_load("ファイル操作")
    >>> definitions = loader.get_active_tool_definitions()
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from apps.messaging_hub.mcp_manager import MCPManager
    from apps.messaging_hub.skills_manager import SkillsManager


_logger = logging.getLogger(__name__)

# デフォルトで常時ロードするスキル名
_DEFAULT_SKILLS: tuple[str, ...] = (
    "list_dir",
    "read_file",
    "get_os_info",
)


@dataclass
class ToolIndexEntry:
    """ツールインデックスエントリ（軽量メタデータ）.

    完全なスキーマを保持せず、名前と説明のみ。
    検索・フィルタリングに使用。

    Attributes:
        name: ツール名
        description: ツールの説明
        source: ソース種別 ("skill" | "mcp")
        server: MCP サーバー名（MCP ツールの場合）
        keywords: 検索用キーワード（自動生成）
        loaded: 完全定義がロード済みか
    """

    name: str
    description: str
    source: str
    server: str = ""
    keywords: list[str] = field(default_factory=list)
    loaded: bool = False

    def __post_init__(self) -> None:
        """キーワードを自動生成."""
        if not self.keywords:
            text = f"{self.name} {self.description}".lower()
            words = re.split(r"[_\-\s/]+", text)
            self.keywords = [w for w in words if len(w) >= 2]

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "description": self.description[:100] if self.description else "",
            "source": self.source,
            "server": self.server,
            "loaded": self.loaded,
        }


@dataclass
class ToolSearchResult:
    """ツール検索結果.

    Attributes:
        entries: マッチしたエントリ
        query: 検索クエリ
        loaded_count: ロード済みツール数
    """

    entries: list[ToolIndexEntry]
    query: str
    loaded_count: int = 0


class LazyToolLoader:
    """懒加載ツールローダー.

    Skills + MCP ツールを統合管理し、上下文 token 過負荷を防止。
    デフォルトで2-3個のみロードし、残りはインデックスで保持。

    戦略:
    1. 初期化時: 全ツールのインデックス（名前+説明）を構築
    2. デフォルトツール: 設定に基づき常時ロード
    3. オンデマンド: search_and_load() で必要時にロード
    4. セッションキャッシュ: ロード済みツールをキャッシュ
    """

    def __init__(
        self,
        mcp_manager: MCPManager,
        skills_manager: SkillsManager,
        *,
        default_skills: tuple[str, ...] | None = None,
        default_load_count: int = 3,
    ) -> None:
        """初期化.

        Args:
            mcp_manager: MCP マネージャー
            skills_manager: スキルマネージャー
            default_skills: デフォルトでロードするスキル名
            default_load_count: デフォルトロード数
        """
        self._mcp_manager = mcp_manager
        self._skills_manager = skills_manager
        self._default_skills = default_skills or _DEFAULT_SKILLS
        self._default_load_count = default_load_count

        # インデックス（軽量メタデータ）
        self._tool_index: dict[str, ToolIndexEntry] = {}
        # ロード済みツール（完全定義）
        self._loaded_tools: dict[str, dict[str, Any]] = {}
        # 検索履歴
        self._search_history: list[str] = []

        self._logger = logging.getLogger(__name__)

    async def build_index(self) -> None:
        """全ツールのインデックスを構築.

        Skills と MCP サーバーから軽量メタデータを収集し、
        インデックスに登録する。
        """
        self._tool_index.clear()

        # 1. Skills のインデックス化
        skills = await self._skills_manager.list_available_skills()
        for skill in skills:
            entry = ToolIndexEntry(
                name=skill.name,
                description=skill.description,
                source="skill",
            )
            self._tool_index[skill.name] = entry

        # 2. MCP サーバーのツールをインデックス化
        servers = self._mcp_manager.list_servers()
        for server in servers:
            if not server.get("enabled", False):
                continue
            server_name = server.get("name", "")
            # MCP ツールは「サーバー名::ツール名」形式で登録
            entry = ToolIndexEntry(
                name=f"mcp::{server_name}",
                description=server.get("description", ""),
                source="mcp",
                server=server_name,
            )
            self._tool_index[entry.name] = entry

        # 3. デフォルトツールをロード済みとしてマーク
        for skill_name in self._default_skills:
            if skill_name in self._tool_index:
                self._tool_index[skill_name].loaded = True
                self._loaded_tools[skill_name] = {
                    "name": skill_name,
                    "source": "skill",
                    "loaded": True,
                }

        self._logger.info(
            "ツールインデックス構築完了: 合計=%d, デフォルトロード=%d",
            len(self._tool_index),
            len(self._loaded_tools),
        )

    def get_tool_index(self) -> list[dict[str, Any]]:
        """全ツールのインデックスを取得.

        Returns:
            ツールインデックスのリスト（名前+説明+ソース）
        """
        return [entry.to_dict() for entry in self._tool_index.values()]

    def get_index_for_prompt(self) -> str:
        """LLM プロンプト用の軽量インデックス文字列を生成.

        ロード済みでないツールのみ、名前と説明を一覧にする。

        Returns:
            プロンプト用インデックス文字列
        """
        lines: list[str] = []
        for entry in self._tool_index.values():
            if entry.loaded:
                continue
            desc = entry.description[:80] if entry.description else "説明なし"
            lines.append(f"- {entry.name}: {desc}")

        if not lines:
            return ""

        header = "以下のツールが利用可能です。必要に応じて「tool_search」で検索してロードしてください:\n"
        return header + "\n".join(lines)

    def search_and_load(self, query: str) -> ToolSearchResult:
        """クエリに基づいてツールを検索し、ロードする.

        Args:
            query: 検索クエリ（自然言語）

        Returns:
            検索結果
        """
        self._search_history.append(query)
        query_lower = query.lower()
        query_words = re.split(r"[\s_\-/]+", query_lower)
        query_words = [w for w in query_words if len(w) >= 2]

        matched: list[ToolIndexEntry] = []
        for entry in self._tool_index.values():
            # キーワードマッチ
            score = sum(1 for qw in query_words if any(qw in kw for kw in entry.keywords))
            # 名前の部分一致
            if query_lower in entry.name.lower():
                score += 3
            # 説明の部分一致
            if query_lower in entry.description.lower():
                score += 2

            if score > 0:
                matched.append(entry)

        # マッチしたツールをロード済みにする
        loaded_count = 0
        for entry in matched:
            if not entry.loaded:
                entry.loaded = True
                self._loaded_tools[entry.name] = {
                    "name": entry.name,
                    "source": entry.source,
                    "server": entry.server,
                    "loaded": True,
                }
                loaded_count += 1

        self._logger.info(
            "ツール検索: query=%s, マッチ=%d, 新規ロード=%d",
            query,
            len(matched),
            loaded_count,
        )
        return ToolSearchResult(
            entries=matched,
            query=query,
            loaded_count=loaded_count,
        )

    def get_loaded_tool_names(self) -> list[str]:
        """ロード済みツール名の一覧を取得.

        Returns:
            ロード済みツール名リスト
        """
        return list(self._loaded_tools.keys())

    def get_active_tool_definitions(self) -> list[dict[str, Any]]:
        """現在ロード済みのツール定義を取得.

        Coordinator がプロンプトに注入する用。
        ロード済みツールのみ返す。

        Returns:
            ツール定義リスト
        """
        return list(self._loaded_tools.values())

    def get_stats(self) -> dict[str, Any]:
        """ローダーの統計情報を取得.

        Returns:
            統計情報辞書
        """
        return {
            "total_indexed": len(self._tool_index),
            "loaded_count": len(self._loaded_tools),
            "unloaded_count": len(self._tool_index) - len(self._loaded_tools),
            "search_count": len(self._search_history),
            "default_skills": list(self._default_skills),
            "loaded_tools": self.get_loaded_tool_names(),
        }

    def reset_session(self) -> None:
        """セッションをリセット（ロード済みツールをクリア）.

        デフォルトツール以外のロード状態を解除する。
        """
        self._loaded_tools.clear()
        self._search_history.clear()

        # デフォルトツールを再ロード
        for skill_name in self._default_skills:
            if skill_name in self._tool_index:
                self._tool_index[skill_name].loaded = True
                self._loaded_tools[skill_name] = {
                    "name": skill_name,
                    "source": "skill",
                    "loaded": True,
                }

        # 非デフォルトのロード状態を解除
        for entry in self._tool_index.values():
            if entry.name not in self._loaded_tools:
                entry.loaded = False

        self._logger.info("セッションリセット完了")
