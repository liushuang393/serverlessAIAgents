"""Flow実行クラス.

エンドユーザーが使用するFlowクラス、シンプルな実行インターフェースを提供。

Example:
    >>> flow = create_flow("my-flow").then(MyAgent).build()
    >>> result = await flow.run({"question": "..."})
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)

設計原則:
- シンプルなAPI：run()とrun_stream()の2つのコアメソッド
- 自動管理：Agentの自動初期化とクリーンアップ
- 観測可能：組み込み進捗追跡
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.flow.context import FlowContext
from agentflow.flow.executor import FlowExecutor
from agentflow.flow.types import CleanableAgent, FlowConfig, InitializableAgent


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.flow.graph import FlowGraph


class MemoryAccessor:
    """メモリシステムアクセサー.

    意味的なメモリ操作APIを提供。

    Example:
        >>> flow.memory.remember("key", "value")
        >>> value = flow.memory.recall("key")
    """

    def __init__(self, context: FlowContext) -> None:
        self._context = context

    def remember(self, key: str, value: Any) -> None:
        """値を保存."""
        self._context.set(key, value)

    def recall(self, key: str, default: Any = None) -> Any:
        """値を読み取り."""
        return self._context.get(key, default)

    def forget(self, key: str) -> None:
        """値を削除."""
        self._context.remove(key)


class Flow:
    """Flow実行クラス.

    フローグラフと実行エンジンをカプセル化し、シンプルな実行インターフェースを提供。

    Attributes:
        flow_id: フローID
        name: フロー名
        memory: メモリシステムアクセサー
    """

    def __init__(
        self,
        flow_id: str,
        name: str,
        graph: FlowGraph,
        config: FlowConfig,
    ) -> None:
        """初期化.

        Args:
            flow_id: フローID
            name: フロー名
            graph: フローグラフ
            config: 設定
        """
        self._logger = logging.getLogger("agentflow.flow")
        self.flow_id = flow_id
        self.name = name
        self._graph = graph
        self._config = config

        self._context = FlowContext(flow_id)
        self._executor = FlowExecutor(
            graph,
            enable_progress=config.enable_progress,
            max_revisions=config.max_revisions,
        )
        self._initialized = False

        # メモリアクセサー
        self.memory = MemoryAccessor(self._context)

    async def _ensure_initialized(self) -> None:
        """すべてのAgentが初期化されていることを確認."""
        if self._initialized or not self._config.auto_initialize:
            return

        for node in self._graph:
            agent = getattr(node, "agent", None)
            if agent and isinstance(agent, InitializableAgent):
                await agent.initialize()

        self._initialized = True
        self._logger.debug(f"Flow {self.flow_id} 初期化完了")

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """フローを実行.

        Args:
            inputs: 入力データ

        Returns:
            最終結果

        Example:
            >>> result = await flow.run({"question": "..."})
        """
        await self._ensure_initialized()
        self._context.clear()
        return await self._executor.execute(inputs)

    async def run_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行、イベントを発行.

        Args:
            inputs: 入力データ

        Yields:
            AG-UIプロトコルイベント

        Example:
            >>> async for event in flow.run_stream(inputs):
            ...     if event["type"] == "node_complete":
            ...         print(f"完了: {event['node_name']}")
        """
        await self._ensure_initialized()
        self._context.clear()
        async for event in self._executor.execute_stream(inputs):
            yield event

    async def cleanup(self) -> None:
        """すべてのAgentリソースをクリーンアップ."""
        for node in self._graph:
            agent = getattr(node, "agent", None)
            if agent and isinstance(agent, CleanableAgent):
                await agent.cleanup()
        self._initialized = False
        self._logger.debug(f"Flow {self.flow_id} クリーンアップ完了")

    @property
    def node_count(self) -> int:
        """ノード数."""
        return self._graph.node_count

    @property
    def context(self) -> FlowContext:
        """実行コンテキスト（高度な用法）."""
        return self._context

    def __repr__(self) -> str:
        return f"Flow({self.flow_id!r}, nodes={self.node_count})"


__all__ = ["Flow", "MemoryAccessor"]

