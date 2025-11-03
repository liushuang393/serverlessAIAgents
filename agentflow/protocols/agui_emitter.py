"""AG-UI イベントエミッター実装.

このモジュールは AgentFlow のフローイベントを AG-UI プロトコルイベントに変換し、
SSE (Server-Sent Events) ストリームとして配信するエミッターを提供します。
"""

import asyncio
import logging
import time
from collections.abc import AsyncIterator

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.hooks import HookEvent, HookType
from agentflow.protocols.agui_events import (
    AGUIEvent,
    FlowCancelEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    NodeStartEvent,
    ProgressEvent,
)


class AGUIEventEmitter:
    """AG-UI イベントエミッター.

    AgentFlowEngine のフックイベントを AG-UI プロトコルイベントに変換し、
    SSE ストリームとして配信します。

    Example:
        >>> engine = AgentFlowEngine()
        >>> emitter = AGUIEventEmitter(engine)
        >>> await emitter.attach_to_flow("my-flow")
        >>> async for event in emitter.stream_events():
        ...     print(event.event_type, event.data)
    """

    def __init__(
        self,
        engine: AgentFlowEngine,
        *,
        logger: logging.Logger | None = None,
        max_queue_size: int = 1000,
    ) -> None:
        """AG-UI イベントエミッターを初期化.

        Args:
            engine: AgentFlowEngine インスタンス
            logger: ロガーインスタンス (オプション)
            max_queue_size: イベントキューの最大サイズ
        """
        self._engine = engine
        self._logger = logger or logging.getLogger(__name__)
        self._max_queue_size = max_queue_size
        self._event_queue: asyncio.Queue[AGUIEvent | None] = asyncio.Queue(maxsize=max_queue_size)
        self._flow_id: str | None = None
        self._node_count: int = 0
        self._completed_nodes: int = 0

    async def attach_to_flow(self, flow_id: str) -> None:
        """フローにアタッチしてイベントの収集を開始.

        Args:
            flow_id: フロー ID
        """
        self._flow_id = flow_id
        self._node_count = 0
        self._completed_nodes = 0

        # 全てのフックタイプにハンドラーを登録
        self._engine.register_hook(HookType.ON_START, self._on_flow_start)
        self._engine.register_hook(HookType.ON_COMPLETE, self._on_flow_complete)
        self._engine.register_hook(HookType.ON_ERROR, self._on_flow_error)
        self._engine.register_hook(HookType.ON_CANCEL, self._on_flow_cancel)
        self._engine.register_hook(HookType.ON_NODE_EXEC, self._on_node_start)
        self._engine.register_hook(HookType.ON_NODE_COMPLETE, self._on_node_complete)

        self._logger.info(f"Attached to flow: {flow_id}")

    async def detach_from_flow(self) -> None:
        """フローからデタッチしてイベント収集を停止."""
        # 全てのフックハンドラーを解除
        self._engine.unregister_hook(HookType.ON_START, self._on_flow_start)
        self._engine.unregister_hook(HookType.ON_COMPLETE, self._on_flow_complete)
        self._engine.unregister_hook(HookType.ON_ERROR, self._on_flow_error)
        self._engine.unregister_hook(HookType.ON_CANCEL, self._on_flow_cancel)
        self._engine.unregister_hook(HookType.ON_NODE_EXEC, self._on_node_start)
        self._engine.unregister_hook(HookType.ON_NODE_COMPLETE, self._on_node_complete)

        # ストリーム終了シグナルを送信
        await self._event_queue.put(None)

        self._logger.info(f"Detached from flow: {self._flow_id}")
        self._flow_id = None

    async def _emit_event(self, event: AGUIEvent) -> None:
        """イベントをキューに追加.

        Args:
            event: AG-UI イベント
        """
        try:
            await asyncio.wait_for(self._event_queue.put(event), timeout=1.0)
            self._logger.debug(f"Emitted event: {event.event_type}")
        except TimeoutError:
            self._logger.warning(f"Event queue full, dropping event: {event.event_type}")

    async def _on_flow_start(self, event: HookEvent) -> None:
        """フロー開始フックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        agui_event = FlowStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"workflow_name": event.workflow_name},
        )
        await self._emit_event(agui_event)

    async def _on_flow_complete(self, event: HookEvent) -> None:
        """フロー完了フックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        agui_event = FlowCompleteEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={
                "workflow_name": event.workflow_name,
                "result": event.data.get("result"),
            },
        )
        await self._emit_event(agui_event)

    async def _on_flow_error(self, event: HookEvent) -> None:
        """フローエラーフックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        error = event.data.get("error")
        agui_event = FlowErrorEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"workflow_name": event.workflow_name},
            error_message=str(error) if error else "Unknown error",
            error_type=type(error).__name__ if error else "Exception",
        )
        await self._emit_event(agui_event)

    async def _on_flow_cancel(self, event: HookEvent) -> None:
        """フローキャンセルフックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        agui_event = FlowCancelEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"workflow_name": event.workflow_name},
        )
        await self._emit_event(agui_event)

    async def _on_node_start(self, event: HookEvent) -> None:
        """ノード開始フックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        node_name = event.data.get("node_name", "unknown")
        node_id = event.data.get("node_id", node_name)

        self._node_count += 1

        agui_event = NodeStartEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={"workflow_name": event.workflow_name},
            node_id=node_id,
            node_name=node_name,
        )
        await self._emit_event(agui_event)

        # プログレスイベントも発行
        await self._emit_progress_event()

    async def _on_node_complete(self, event: HookEvent) -> None:
        """ノード完了フックハンドラー.

        Args:
            event: フックイベント
        """
        if self._flow_id is None:
            return

        node_name = event.data.get("node_name", "unknown")
        node_id = event.data.get("node_id", node_name)

        self._completed_nodes += 1

        # エラーがある場合はエラーイベントを発行
        if "error" in event.data:
            error = event.data["error"]
            agui_event: NodeErrorEvent | NodeCompleteEvent = NodeErrorEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                data={"workflow_name": event.workflow_name},
                node_id=node_id,
                node_name=node_name,
                error_message=str(error),
                error_type=type(error).__name__,
            )
        else:
            agui_event = NodeCompleteEvent(
                timestamp=time.time(),
                flow_id=self._flow_id,
                data={
                    "workflow_name": event.workflow_name,
                    "result": event.data.get("result"),
                },
                node_id=node_id,
                node_name=node_name,
            )

        await self._emit_event(agui_event)

        # プログレスイベントも発行
        await self._emit_progress_event()

    async def _emit_progress_event(self) -> None:
        """プログレスイベントを発行."""
        if self._flow_id is None or self._node_count == 0:
            return

        percentage = (self._completed_nodes / self._node_count) * 100

        progress_event = ProgressEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            current=self._completed_nodes,
            total=self._node_count,
            percentage=percentage,
        )
        await self._emit_event(progress_event)

    async def emit_log(self, level: str, message: str, source: str | None = None) -> None:
        """ログイベントを発行.

        Args:
            level: ログレベル (DEBUG, INFO, WARNING, ERROR)
            message: ログメッセージ
            source: ログソース (オプション)
        """
        if self._flow_id is None:
            return

        log_event = LogEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            data={},
            level=level,
            message=message,
            source=source,
        )
        await self._emit_event(log_event)

    async def stream_events(self) -> AsyncIterator[AGUIEvent]:
        """イベントストリームを取得.

        Yields:
            AG-UI イベント
        """
        while True:
            event = await self._event_queue.get()

            # None はストリーム終了シグナル
            if event is None:
                break

            yield event

    def get_queue_size(self) -> int:
        """現在のキューサイズを取得.

        Returns:
            キュー内のイベント数
        """
        return self._event_queue.qsize()
