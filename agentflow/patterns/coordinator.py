"""協調器基類 - Multi-Agent 協調パターン共通インターフェース.

このモジュールは協調器の基本インターフェースを定義します：
- Sequential: 順次実行
- Concurrent: 並行実行
- Supervisor: 監督者パターン
- Hierarchical: 階層パターン

設計原則：
- 簡単：最小限のインターフェース
- 柔軟：具体実装は自由
- 統一：全パターン同じ API
"""

import logging
from abc import ABC, abstractmethod
from enum import Enum
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.core.registry import Registry


class CoordinationPattern(str, Enum):
    """協調パターン種別."""

    SEQUENTIAL = "sequential"
    CONCURRENT = "concurrent"
    SUPERVISOR = "supervisor"
    HIERARCHICAL = "hierarchical"
    HANDOFF = "handoff"


class CoordinatorBase(ABC):
    """協調器基類 - 全パターン共通インターフェース.

    Example:
        >>> class MyCoordinator(CoordinatorBase):
        ...     @property
        ...     def pattern(self) -> CoordinationPattern:
        ...         return CoordinationPattern.SEQUENTIAL
        ...
        ...     async def execute(self, task, **kwargs):
        ...         # 実装
        ...         pass
    """

    def __init__(self, agents: list[AgentBlock] | None = None) -> None:
        """初期化.

        Args:
            agents: 協調対象の Agent リスト
        """
        self._agents = agents or []
        self._logger = logging.getLogger(self.__class__.__name__)

    @property
    @abstractmethod
    def pattern(self) -> CoordinationPattern:
        """協調パターン種別を取得.

        Returns:
            協調パターン
        """
        pass

    @abstractmethod
    async def execute(self, task: str, **kwargs: Any) -> dict[str, Any]:
        """協調タスクを実行.

        Args:
            task: 実行するタスク
            **kwargs: 追加パラメータ

        Returns:
            実行結果
        """
        pass

    def add_agent(self, agent: AgentBlock) -> None:
        """Agent を追加.

        Args:
            agent: 追加する Agent
        """
        self._agents.append(agent)
        self._logger.debug(f"Added agent: {type(agent).__name__}")

    def remove_agent(self, agent: AgentBlock) -> bool:
        """Agent を削除.

        Args:
            agent: 削除する Agent

        Returns:
            削除成功した場合 True
        """
        try:
            self._agents.remove(agent)
            return True
        except ValueError:
            return False

    @property
    def agents(self) -> list[AgentBlock]:
        """Agent リストを取得."""
        return list(self._agents)

    @property
    def agent_count(self) -> int:
        """Agent 数を取得."""
        return len(self._agents)


class CoordinatorRegistry(Registry["CoordinatorBase"]):
    """協調器レジストリ - パターン別の協調器を管理.

    Example:
        >>> registry = CoordinatorRegistry()
        >>> registry.register("supervisor", SupervisorCoordinator())
        >>> coordinator = registry.get("supervisor")
    """

    _instance: "CoordinatorRegistry | None" = None

    def __new__(cls) -> "CoordinatorRegistry":
        """シングルトンパターン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def get_by_pattern(self, pattern: CoordinationPattern) -> "CoordinatorBase | None":
        """パターン種別で協調器を取得.

        Args:
            pattern: 協調パターン

        Returns:
            協調器、存在しない場合 None
        """
        for coordinator in self._items.values():
            if coordinator.pattern == pattern:
                return coordinator
        return None

