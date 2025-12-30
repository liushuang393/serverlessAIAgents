"""AgentFlow Quick API - 統一入口.

目的: 最小限のコードでAgentFlowを使えるようにする高抽象API
設計: 低耦合・高内聚・対外高抽象

使用例:
    # 最小構成（単一Agent）
    >>> from agentflow import create_flow
    >>> flow = create_flow([MyAgent()])
    >>> result = await flow.run({"question": "..."})
    # → 直接 Agent の出力を返す（ラップしない）

    # 複数Agent順次実行
    >>> flow = create_flow(
    ...     agents=[Agent1(), Agent2(), Agent3()],
    ...     pattern="sequential"
    ... )

    # ストリーム実行（SSE用）
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)

    # 記憶システムアクセス
    >>> flow.memory.remember("key", "value")
    >>> value = flow.memory.recall("key")
"""

from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator
from typing import TYPE_CHECKING, Any, Literal

if TYPE_CHECKING:
    from fastapi import FastAPI

    from agentflow.core.agent_block import AgentBlock


# ============================================================
# Flow 作成 API
# ============================================================


def create_flow(
    agents: list[AgentBlock],
    *,
    pattern: Literal["sequential", "concurrent", "handoff"] = "sequential",
    enable_memory: bool = True,
    name: str | None = None,
    auto_initialize: bool = True,
) -> Flow:
    """Flowを作成する統一入口.

    Args:
        agents: Agentのリスト（単一でも可）
        pattern: 協調パターン（sequential/concurrent/handoff）
        enable_memory: 記憶システムを有効化
        name: Flow名（省略時は自動生成）
        auto_initialize: Agent の initialize() を自動呼び出し

    Returns:
        Flow: 実行可能なFlowインスタンス

    Example:
        >>> flow = create_flow([MyAgent()])
        >>> result = await flow.run({"input": "test"})
        # → Agent の出力がそのまま返る
    """
    from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext

    context = SharedContext(enable_memory=enable_memory)
    coordinator = AgentCoordinator(
        agents=agents,
        pattern=pattern,
        shared_context=context,
    )

    return Flow(
        coordinator=coordinator,
        context=context,
        agents=agents,
        name=name or f"flow-{len(agents)}agents",
        auto_initialize=auto_initialize,
    )


class MemoryAccessor:
    """記憶システムへの簡易アクセサ.

    Example:
        >>> flow.memory.remember("key", "value")
        >>> value = flow.memory.recall("key")
    """

    def __init__(self, context: Any) -> None:
        """初期化."""
        self._context = context

    def remember(self, key: str, value: Any) -> None:
        """値を記憶."""
        self._context.set(key, value)

    def recall(self, key: str, default: Any = None) -> Any:
        """値を想起."""
        return self._context.get(key, default)

    def forget(self, key: str) -> None:
        """値を忘却."""
        self._context.remove(key)

    def clear(self) -> None:
        """全記憶をクリア."""
        self._context.clear()


class Flow:
    """Flow実行クラス.

    内部実装を隠蔽し、シンプルなAPIを提供.

    Example:
        >>> flow = create_flow([MyAgent()])
        >>> result = await flow.run({"task": "..."})
        >>> async for event in flow.run_stream(inputs):
        ...     print(event)
    """

    def __init__(
        self,
        coordinator: Any,
        context: Any,
        agents: list[Any],
        name: str,
        auto_initialize: bool = True,
    ) -> None:
        """初期化.

        Args:
            coordinator: AgentCoordinatorインスタンス
            context: SharedContextインスタンス
            agents: Agentリスト
            name: Flow名
            auto_initialize: 自動初期化フラグ
        """
        self._coordinator = coordinator
        self._context = context
        self._agents = agents
        self._auto_initialize = auto_initialize
        self._initialized = False
        self.name = name
        self.memory = MemoryAccessor(context)

    async def _ensure_initialized(self) -> None:
        """Agent の初期化を保証."""
        if self._initialized or not self._auto_initialize:
            return

        for agent in self._agents:
            if hasattr(agent, "initialize"):
                await agent.initialize()

        self._initialized = True

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Flowを実行.

        Args:
            inputs: 入力データ

        Returns:
            最終Agentの出力（ラップしない）
        """
        await self._ensure_initialized()

        result = await self._coordinator.execute(inputs)

        # 痛点1解決: final_result を直接返す
        if isinstance(result, dict) and "final_result" in result:
            return result["final_result"]
        return result

    async def run_stream(
        self,
        inputs: dict[str, Any],
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリームモードでFlowを実行（SSE用）.

        Args:
            inputs: 入力データ

        Yields:
            イベント dict:
                - type: "node_start" | "node_complete" | "progress" | "result" | "error"
                - node: Agent名（node_start/node_complete時）
                - data: イベントデータ
        """
        await self._ensure_initialized()

        # イベントキュー
        event_queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue()
        done = asyncio.Event()

        async def on_node_start(node_name: str, data: dict[str, Any]) -> None:
            await event_queue.put({
                "type": "node_start",
                "node": node_name,
                "data": data,
            })

        async def on_node_complete(node_name: str, result: dict[str, Any]) -> None:
            await event_queue.put({
                "type": "node_complete",
                "node": node_name,
                "data": result,
            })

        async def execute_flow() -> dict[str, Any]:
            try:
                # コールバック登録
                self._coordinator._on_node_start = on_node_start
                self._coordinator._on_node_complete = on_node_complete
                result = await self._coordinator.execute(inputs)
                await event_queue.put({"type": "result", "data": result})
                return result
            except Exception as e:
                await event_queue.put({"type": "error", "error": str(e)})
                raise
            finally:
                done.set()

        # 並行実行
        task = asyncio.create_task(execute_flow())

        # イベントを yield
        while not done.is_set() or not event_queue.empty():
            try:
                event = await asyncio.wait_for(event_queue.get(), timeout=0.1)
                yield event
            except TimeoutError:
                continue

        # タスク結果を確認（例外があれば再raise）
        await task

    async def cleanup(self) -> None:
        """全Agentのクリーンアップ."""
        for agent in self._agents:
            if hasattr(agent, "cleanup"):
                await agent.cleanup()
        self._initialized = False

    @property
    def agents(self) -> list[Any]:
        """Agentリスト."""
        return self._agents

    # 後方互換性のため残す（非推奨）
    @property
    def coordinator(self) -> Any:
        """内部Coordinatorへのアクセス（上級者向け・非推奨）."""
        return self._coordinator

    @property
    def context(self) -> Any:
        """SharedContextへのアクセス（上級者向け・非推奨）."""
        return self._context


# ============================================================
# 後方互換エイリアス
# ============================================================

# FlowWrapper は Flow にリネーム（後方互換）
FlowWrapper = Flow


# ============================================================
# API Endpoint 作成（非推奨・将来削除予定）
# ============================================================


def create_api_endpoint(
    app: FastAPI,
    flow: Flow,
    *,
    path: str = "/api/task",
    stream_path: str | None = None,
) -> None:
    """FastAPIエンドポイントを自動生成.

    注意: この関数は非推奨です。
    実際のプロジェクトでは手動でエンドポイントを定義することを推奨。

    Args:
        app: FastAPIインスタンス
        flow: Flowインスタンス
        path: RESTエンドポイントパス
        stream_path: SSEエンドポイントパス（省略時は{path}/stream）
    """
    from agentflow.builders.api_factory import APIFactory

    factory = APIFactory(app, flow)
    factory.create_rest_endpoint(path)
    factory.create_sse_endpoint(stream_path or f"{path}/stream")

