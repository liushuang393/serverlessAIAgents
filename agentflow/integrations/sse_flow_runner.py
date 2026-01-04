# -*- coding: utf-8 -*-
"""SSEFlowRunner - SSE イベント発射と業務ロジックの分離.

このモジュールは、AgentPipeline とSSE配信を分離し、
テスト容易性とコードの再利用性を向上させます。

設計原則:
- 分離: SSEイベント発射とビジネスロジックを分離
- 互換: AG-UIプロトコル準拠
- 柔軟: AgentPipelineまたは任意のFlow実行を委譲可能

使用例:
    >>> from agentflow.integrations.sse_flow_runner import SSEFlowRunner
    >>> from agentflow.patterns.agent_pipeline import AgentPipeline
    >>> 
    >>> pipeline = AgentPipeline(agents=[...], flow_id="my-flow")
    >>> runner = SSEFlowRunner(pipeline)
    >>> 
    >>> # SSEストリーム配信
    >>> async for event in runner.run_with_events(input_data):
    ...     yield event.to_sse()  # SSE形式に変換
    >>>
    >>> # FastAPI統合
    >>> @router.get("/stream")
    >>> async def stream_endpoint():
    ...     return StreamingResponse(
    ...         runner.stream_sse(input_data),
    ...         media_type="text/event-stream",
    ...     )
"""

import asyncio
import logging
import time
import uuid
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from typing import Any, Protocol

from agentflow.protocols.agui_events import (
    AGUIEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
)


class FlowProtocol(Protocol):
    """Flow プロトコル（ダックタイピング用）.

    AgentPipeline または任意のFlowクラスと互換。
    """

    flow_id: str

    async def run_with_events(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[tuple[dict[str, Any] | None, AGUIEvent | None]]:
        """イベント付きでFlowを実行.

        Yields:
            (結果 | None, イベント | None) のタプル
        """
        ...


class SimplePipelineProtocol(Protocol):
    """シンプルなパイプラインプロトコル.

    run_sync のみを持つシンプルなパイプライン用。
    """

    async def run_sync(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """同期的にFlowを実行."""
        ...


@dataclass
class SSEConfig:
    """SSE設定.

    Attributes:
        flow_id_prefix: フローID生成時のプレフィックス
        emit_heartbeat: ハートビートを発射するか
        heartbeat_interval: ハートビート間隔（秒）
        max_event_queue_size: イベントキューの最大サイズ
        event_timeout: イベント送信タイムアウト（秒）
    """

    flow_id_prefix: str = "sse"
    emit_heartbeat: bool = False
    heartbeat_interval: float = 15.0
    max_event_queue_size: int = 1000
    event_timeout: float = 1.0


class SSEFlowRunner:
    """SSE フロー実行器.

    AgentPipeline などのFlowをラップし、SSE配信に特化した機能を提供。
    業務ロジックとSSE発射を分離することで、テスト容易性を向上。

    Attributes:
        flow: FlowProtocol または SimplePipelineProtocol
        config: SSE設定

    使用例:
        >>> # AgentPipeline をラップ
        >>> runner = SSEFlowRunner(pipeline)
        >>> async for event in runner.run_with_events(input_data):
        ...     yield event
        >>>
        >>> # カスタムFlow をラップ
        >>> runner = SSEFlowRunner.wrap_simple(my_workflow.process)
        >>> async for event in runner.run_with_events(input_data):
        ...     yield event
    """

    def __init__(
        self,
        flow: FlowProtocol | None = None,
        config: SSEConfig | None = None,
        flow_id: str | None = None,
    ) -> None:
        """初期化.

        Args:
            flow: FlowProtocol 準拠のFlowインスタンス
            config: SSE設定（オプション）
            flow_id: フローID（オプション、Flowから取得可能な場合は不要）
        """
        self._logger = logging.getLogger("agentflow.sse_runner")
        self._flow = flow
        self._config = config or SSEConfig()
        self._flow_id = flow_id or getattr(flow, "flow_id", None)
        self._event_queue: asyncio.Queue[AGUIEvent] = asyncio.Queue(
            maxsize=self._config.max_event_queue_size
        )

    @property
    def flow_id(self) -> str:
        """フローIDを取得（未設定の場合は自動生成）."""
        if self._flow_id:
            return self._flow_id
        return f"{self._config.flow_id_prefix}-{uuid.uuid4().hex[:8]}"

    def set_flow_id(self, flow_id: str) -> None:
        """フローIDを設定."""
        self._flow_id = flow_id

    async def run_with_events(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[AGUIEvent]:
        """SSEイベントストリームを生成.

        内部のFlowを実行し、AG-UI準拠イベントをyield。

        Args:
            input_data: 入力データ

        Yields:
            AGUIEvent
        """
        if self._flow is None:
            raise ValueError("Flow is not set. Use set_flow() or pass flow to constructor.")

        flow_id = self.flow_id
        now = time.time()

        # Flow開始イベント
        yield FlowStartEvent(
            timestamp=now,
            flow_id=flow_id,
            data={"input_keys": list(input_data.keys())},
        )

        try:
            # Flowを実行してイベントを中継
            final_result: dict[str, Any] | None = None

            async for result, event in self._flow.run_with_events(input_data):
                if event is not None:
                    # FlowStartEvent は既に発射済みなのでスキップ
                    if not isinstance(event, FlowStartEvent):
                        yield event
                if result is not None:
                    final_result = result

            # Flow完了イベント（FlowCompleteEvent がまだ発射されていない場合）
            if final_result is not None:
                # 結果付きで完了イベントを発射
                yield FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=flow_id,
                    data={},
                    result=final_result,
                    include_result=True,
                )

        except Exception as e:
            self._logger.error(f"Flow execution error: {e}")
            yield FlowErrorEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                data={},
                error_message=str(e),
                error_type=type(e).__name__,
            )
            raise

    async def stream_sse(
        self,
        input_data: dict[str, Any],
        event_format: str = "json",
    ) -> AsyncIterator[str]:
        """SSE形式の文字列ストリームを生成.

        FastAPI StreamingResponse などで直接使用可能。

        Args:
            input_data: 入力データ
            event_format: イベント形式（"json" または "simple"）

        Yields:
            SSE形式の文字列（"data: {...}\\n\\n"）
        """
        async for event in self.run_with_events(input_data):
            sse_line = self._format_sse_event(event, event_format)
            yield sse_line

    def _format_sse_event(self, event: AGUIEvent, event_format: str) -> str:
        """AGUIEvent を SSE文字列に変換.

        Args:
            event: AGUIEvent インスタンス
            event_format: "json" または "simple"

        Returns:
            SSE形式の文字列
        """
        event_type = event.event_type
        if event_format == "json":
            # JSON形式
            data = event.model_dump_json()
            return f"event: {event_type}\ndata: {data}\n\n"
        else:
            # シンプル形式
            return f"event: {event_type}\ndata: {event.flow_id}\n\n"

    def set_flow(self, flow: FlowProtocol) -> None:
        """Flowを設定.

        Args:
            flow: FlowProtocol 準拠のFlowインスタンス
        """
        self._flow = flow
        self._flow_id = getattr(flow, "flow_id", self._flow_id)

    @classmethod
    def wrap_simple(
        cls,
        process_func: Any,
        flow_id: str | None = None,
    ) -> "SSEFlowRunner":
        """シンプルな処理関数をSSEFlowRunnerでラップ.

        run_with_events を持たないシンプルな関数をラップする
        ユーティリティメソッド。

        Args:
            process_func: async def process(input_data) -> result の関数
            flow_id: フローID（オプション）

        Returns:
            SSEFlowRunner インスタンス

        使用例:
            >>> async def my_process(data):
            ...     return {"result": "done"}
            >>> runner = SSEFlowRunner.wrap_simple(my_process, "my-flow")
            >>> async for event in runner.run_with_events(data):
            ...     yield event
        """
        wrapper = _SimpleFlowWrapper(process_func, flow_id or "simple-flow")
        return cls(flow=wrapper, flow_id=flow_id)


class _SimpleFlowWrapper:
    """シンプルな処理関数をFlowProtocolにラップするアダプタ."""

    def __init__(self, process_func: Any, flow_id: str) -> None:
        self._process_func = process_func
        self.flow_id = flow_id

    async def run_with_events(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[tuple[dict[str, Any] | None, AGUIEvent | None]]:
        """シンプル関数を実行してイベントを生成."""
        # 開始イベント
        yield (
            None,
            FlowStartEvent(
                timestamp=time.time(),
                flow_id=self.flow_id,
                data={},
            ),
        )

        try:
            # 処理実行
            result = await self._process_func(input_data)

            # 完了イベント
            yield (
                None,
                FlowCompleteEvent(
                    timestamp=time.time(),
                    flow_id=self.flow_id,
                    data={},
                    result=result,
                    include_result=True,
                ),
            )

            # 結果を返す
            yield (result, None)

        except Exception as e:
            yield (
                None,
                FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self.flow_id,
                    data={},
                    error_message=str(e),
                    error_type=type(e).__name__,
                ),
            )
            raise


__all__ = [
    "FlowProtocol",
    "SSEConfig",
    "SSEFlowRunner",
    "SimplePipelineProtocol",
]

