"""Agentレジストリ.

能力ベースのAgent発見とファクトリベースのインスタンス化を提供するモジュール。

設計原則:
- 高度な抽象化: Agent実装に依存しないインターフェース
- 高凝集: Agent管理機能のみに責任を持つ
- 低結合: 具体的なAgent実装を知らない
- 拡張性: 新しい検索・フィルタ機能の追加が容易

使用例:
    >>> # Agentを登録
    >>> registry = AgentRegistry()
    >>> registry.register(
    ...     agent_id="pdf_analyzer",
    ...     capability=capability,
    ...     factory=lambda: PDFAnalyzerAgent(),
    ... )
    >>>
    >>> # 能力でAgent検索
    >>> matches = registry.find_matching(requirement)
    >>>
    >>> # Agentインスタンスを作成
    >>> factory = registry.get_factory("pdf_analyzer")
    >>> agent = factory()
"""

from __future__ import annotations

import threading
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.core.capability_spec import AgentCapabilitySpec, CapabilityRequirement


class AgentEntry:
    """Agentレジストリエントリ.

    登録されたAgent情報を保持。

    Attributes:
        agent_id: Agent ID
        capability: Agent能力仕様
        factory: Agentインスタンス生成ファクトリ
    """

    def __init__(
        self,
        agent_id: str,
        capability: AgentCapabilitySpec,
        factory: Callable[[], Any],
    ) -> None:
        """初期化.

        Args:
            agent_id: Agent ID
            capability: Agent能力仕様
            factory: Agentインスタンス生成ファクトリ
        """
        self.agent_id = agent_id
        self.capability = capability
        self.factory = factory


class AgentRegistry:
    """Agentレジストリ.

    能力ベースの発見とファクトリベースのインスタンス化を提供する中央レジストリ。

    主な機能:
    - Agent登録（能力とファクトリ付き）
    - タグベースの発見
    - 要件マッチング
    - ファクトリベースのインスタンス化

    スレッドセーフな実装。

    Attributes:
        _agents: agent_id → AgentEntry のマッピング
        _lock: スレッドセーフティのためのロック
    """

    def __init__(self) -> None:
        """空のレジストリを初期化."""
        self._agents: dict[str, AgentEntry] = {}
        self._lock = threading.Lock()

    def register(
        self,
        agent_id: str,
        capability: AgentCapabilitySpec,
        factory: Callable[[], Any],
    ) -> None:
        """Agentを能力とファクトリと共に登録.

        Args:
            agent_id: Agentのユニーク識別子
            capability: Agent能力宣言
            factory: Agentインスタンスを作成するCallable
        """
        with self._lock:
            self._agents[agent_id] = AgentEntry(agent_id, capability, factory)

    def unregister(self, agent_id: str) -> bool:
        """Agentをレジストリから削除.

        Args:
            agent_id: 削除するAgent ID

        Returns:
            削除された場合True、見つからない場合False
        """
        with self._lock:
            if agent_id in self._agents:
                del self._agents[agent_id]
                return True
            return False

    def get_factory(self, agent_id: str) -> Callable[[], Any] | None:
        """Agentのファクトリ関数を取得.

        Args:
            agent_id: Agent ID

        Returns:
            ファクトリCallable または見つからない場合None
        """
        entry = self._agents.get(agent_id)
        return entry.factory if entry else None

    def get_capability(self, agent_id: str) -> AgentCapabilitySpec | None:
        """Agentの能力宣言を取得.

        Args:
            agent_id: Agent ID

        Returns:
            AgentCapabilitySpec または見つからない場合None
        """
        entry = self._agents.get(agent_id)
        return entry.capability if entry else None

    def find_by_tags(self, tags: list[str]) -> list[str]:
        """指定されたタグをすべて持つAgentを検索.

        Args:
            tags: マッチするタグ

        Returns:
            マッチするAgent IDのリスト
        """
        results = []
        for agent_id, entry in self._agents.items():
            if all(tag in entry.capability.tags for tag in tags):
                results.append(agent_id)
        return results

    def find_matching(
        self,
        requirement: CapabilityRequirement,
        limit: int = 5,
    ) -> list[tuple[str, float]]:
        """タスク要件にマッチするAgentを検索.

        Args:
            requirement: タスク要件
            limit: 返す最大結果数

        Returns:
            (agent_id, score) タプルのリスト（スコア降順）
        """
        scored = []
        for agent_id, entry in self._agents.items():
            score = entry.capability.matches(requirement)
            if score > 0:
                scored.append((agent_id, score))

        scored.sort(key=lambda x: x[1], reverse=True)
        return scored[:limit]

    def list_all(self) -> list[AgentEntry]:
        """登録された全Agentエントリをリスト.

        Returns:
            AgentEntryのリスト
        """
        return list(self._agents.values())

    def get_all_capabilities(self) -> dict[str, AgentCapabilitySpec]:
        """全Agentの能力を取得.

        Returns:
            agent_id → AgentCapabilitySpec のマッピング
        """
        return {aid: entry.capability for aid, entry in self._agents.items()}

    def clear(self) -> None:
        """レジストリを完全にクリア."""
        with self._lock:
            self._agents.clear()

    def __contains__(self, agent_id: str) -> bool:
        """Agentが登録されているかチェック."""
        return agent_id in self._agents

    def __len__(self) -> int:
        """登録Agent数を返す."""
        return len(self._agents)


# =============================================================================
# グローバルレジストリ（シングルトン）
# =============================================================================

_global_agent_registry: AgentRegistry | None = None
_registry_lock = threading.Lock()


def get_global_agent_registry() -> AgentRegistry:
    """グローバルAgentレジストリを取得または作成.

    シングルトンパターンでグローバルレジストリを管理。
    アプリケーション全体で同じレジストリを共有。

    Returns:
        グローバルAgentRegistryインスタンス
    """
    global _global_agent_registry
    if _global_agent_registry is None:
        with _registry_lock:
            if _global_agent_registry is None:
                _global_agent_registry = AgentRegistry()
    return _global_agent_registry


def reset_global_agent_registry() -> None:
    """グローバルAgentレジストリをリセット.

    主にテスト用。新しい空のレジストリを作成。
    """
    global _global_agent_registry
    with _registry_lock:
        _global_agent_registry = AgentRegistry()


__all__ = [
    "AgentEntry",
    "AgentRegistry",
    "get_global_agent_registry",
    "reset_global_agent_registry",
]
