"""SharedContext - Agent間の共有コンテキスト.

Agent間で状態を共有するためのコンテキストクラス。
"""

import logging
from datetime import datetime
from typing import Any


class SharedContext:
    """Agent間の共有コンテキスト.

    責務：
    - Agent間で状態を共有
    - 履歴管理
    - スレッドセーフ
    - 記憶システム統合（オプション）

    Example:
        >>> context = SharedContext()
        >>> context.set("result_a", {"data": "A"})
        >>> result = context.get("result_a")

        # 記憶システムを有効化
        >>> context = SharedContext(enable_memory=True)
        >>> await context.start()
        >>> await context.remember("重要な情報", topic="AI")
        >>> memories = await context.recall(topic="AI")
    """

    def __init__(
        self,
        enable_memory: bool = False,
        enable_vector_search: bool = False,
        embedding_dim: int = 384,
    ) -> None:
        """初期化.

        Args:
            enable_memory: 記憶システムを有効化
            enable_vector_search: ベクトル検索を有効化（enable_memory=Trueの場合のみ）
            embedding_dim: 埋め込みベクトルの次元数
        """
        self._data: dict[str, Any] = {}
        self._history: list[dict[str, Any]] = []
        self._logger = logging.getLogger(__name__)
        self._enable_memory = enable_memory
        self._memory_manager = None

        # 記憶システムを初期化
        if self._enable_memory:
            from agentflow.memory import MemoryManager

            self._memory_manager = MemoryManager(enable_vector_search=enable_vector_search, embedding_dim=embedding_dim)

    def set(self, key: str, value: Any) -> None:
        """値を設定."""
        self._data[key] = value
        self._history.append(
            {
                "action": "set",
                "key": key,
                "value": value,
                "timestamp": datetime.now().isoformat(),
            }
        )
        self._logger.debug(f"SharedContext.set: {key}")

    def get(self, key: str, default: Any = None) -> Any:
        """値を取得."""
        return self._data.get(key, default)

    def get_all(self) -> dict[str, Any]:
        """全ての値を取得."""
        return self._data.copy()

    def get_history(self) -> list[dict[str, Any]]:
        """履歴を取得."""
        return self._history.copy()

    def remove(self, key: str) -> None:
        """値を削除."""
        if key in self._data:
            del self._data[key]
            self._history.append(
                {
                    "action": "remove",
                    "key": key,
                    "timestamp": datetime.now().isoformat(),
                }
            )
            self._logger.debug(f"SharedContext.remove: {key}")

    def clear(self) -> None:
        """クリア."""
        self._data.clear()
        self._history.clear()

    async def start(self) -> None:
        """記憶システムを開始（記憶システムが有効な場合）."""
        if self._memory_manager:
            await self._memory_manager.start()
            self._logger.info("Memory system started in SharedContext")

    async def stop(self) -> None:
        """記憶システムを停止（記憶システムが有効な場合）."""
        if self._memory_manager:
            await self._memory_manager.stop()
            self._logger.info("Memory system stopped in SharedContext")

    async def remember(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> Any:
        """情報を記憶（記憶システムが有効な場合）."""
        if not self._memory_manager:
            msg = "Memory system is not enabled. Set enable_memory=True in constructor."
            raise RuntimeError(msg)
        return await self._memory_manager.remember(text, topic, metadata)

    async def recall(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
        query: str | None = None,
        min_similarity: float = 0.0,
    ) -> list[Any]:
        """記憶を検索（記憶システムが有効な場合）."""
        if not self._memory_manager:
            msg = "Memory system is not enabled. Set enable_memory=True in constructor."
            raise RuntimeError(msg)
        return await self._memory_manager.recall(topic, limit, min_importance, query, min_similarity)

    def get_memory_status(self) -> dict[str, Any] | None:
        """記憶システムの状態を取得."""
        if self._memory_manager:
            return self._memory_manager.get_status()
        return None


__all__ = ["SharedContext"]
