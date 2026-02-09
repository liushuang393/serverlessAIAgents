"""Coordinator - Agent協調パターン基底クラス.

Agent協調の基底抽象を提供。
全てのCoordinatorはexecuteメソッドを実装する必要がある。

Example:
    >>> class MyCoordinator(CoordinatorBase):
    ...     @property
    ...     def pattern(self) -> CoordinationPattern:
    ...         return CoordinationPattern.PIPELINE
    ...
    ...     async def execute(self, task: str, **kwargs) -> dict:
    ...         return {"result": f"Processed: {task}"}
"""

from abc import ABC, abstractmethod
from enum import Enum
from typing import Any


class CoordinationPattern(Enum):
    """協調パターン種別."""

    HIERARCHICAL = "hierarchical"  # 階層型（DeepAgent等）
    PEER_TO_PEER = "peer_to_peer"  # P2P型
    BROADCAST = "broadcast"  # ブロードキャスト型
    PIPELINE = "pipeline"  # パイプライン型


class CoordinatorBase(ABC):
    """Agent協調の基底クラス.

    全てのCoordinatorが実装すべきインターフェースを定義。
    executeメソッドが標準の実行エントリーポイント。

    Attributes:
        pattern: 協調パターン種別

    Example:
        >>> coordinator = MyCoordinator()
        >>> result = await coordinator.execute("タスクを実行")
    """

    @property
    @abstractmethod
    def pattern(self) -> CoordinationPattern:
        """協調パターンを返す."""
        ...

    @abstractmethod
    async def execute(self, task: Any, **kwargs: Any) -> Any:
        """タスクを実行.

        全Coordinatorが実装すべきコアメソッド。

        Args:
            task: 実行するタスク（通常は文字列）
            **kwargs: 追加パラメータ

        Returns:
            実行結果（通常はdict）
        """
        ...
