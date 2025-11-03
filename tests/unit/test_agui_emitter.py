"""AG-UI イベントエミッターのユニットテスト."""

import asyncio
from unittest.mock import AsyncMock, MagicMock

import pytest

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.hooks import HookEvent, HookType
from agentflow.protocols.agui_emitter import AGUIEventEmitter
from agentflow.protocols.agui_events import (
    AGUIEventType,
    FlowCancelEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeStartEvent,
    ProgressEvent,
)


@pytest.fixture
def mock_engine() -> AgentFlowEngine:
    """モックエンジンを作成.

    Returns:
        AgentFlowEngine インスタンス
    """
    return AgentFlowEngine()


class TestAGUIEventEmitter:
    """AG-UI イベントエミッターのテストスイート."""

    async def test_emitter_initialization(self, mock_engine: AgentFlowEngine) -> None:
        """エミッターの初期化をテスト."""
        emitter = AGUIEventEmitter(mock_engine)

        assert emitter._engine is mock_engine
        assert emitter._max_queue_size == 1000
        assert emitter._flow_id is None
        assert emitter._node_count == 0
        assert emitter._completed_nodes == 0

    async def test_attach_to_flow(self, mock_engine: AgentFlowEngine) -> None:
        """フローへのアタッチをテスト."""
        emitter = AGUIEventEmitter(mock_engine)

        await emitter.attach_to_flow("test-flow")

        assert emitter._flow_id == "test-flow"
        # フックが登録されていることを確認
        assert len(mock_engine.hooks.get_hooks(HookType.ON_START)) == 1
        assert len(mock_engine.hooks.get_hooks(HookType.ON_COMPLETE)) == 1
        assert len(mock_engine.hooks.get_hooks(HookType.ON_ERROR)) == 1
        assert len(mock_engine.hooks.get_hooks(HookType.ON_CANCEL)) == 1
        assert len(mock_engine.hooks.get_hooks(HookType.ON_NODE_EXEC)) == 1
        assert len(mock_engine.hooks.get_hooks(HookType.ON_NODE_COMPLETE)) == 1

    async def test_detach_from_flow(self, mock_engine: AgentFlowEngine) -> None:
        """フローからのデタッチをテスト."""
        emitter = AGUIEventEmitter(mock_engine)

        await emitter.attach_to_flow("test-flow")
        await emitter.detach_from_flow()

        assert emitter._flow_id is None
        # フックが解除されていることを確認
        assert len(mock_engine.hooks.get_hooks(HookType.ON_START)) == 0
        assert len(mock_engine.hooks.get_hooks(HookType.ON_COMPLETE)) == 0

    async def test_flow_start_event(self, mock_engine: AgentFlowEngine) -> None:
        """フロー開始イベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        # フロー開始イベントをトリガー
        hook_event = HookEvent(
            hook_type=HookType.ON_START,
            workflow_name="test-workflow",
            data={},
        )
        await emitter._on_flow_start(hook_event)

        # イベントがキューに追加されたことを確認
        assert emitter.get_queue_size() == 1

        # イベントを取得して検証
        event = await emitter._event_queue.get()
        assert isinstance(event, FlowStartEvent)
        assert event.event_type == AGUIEventType.FLOW_START
        assert event.flow_id == "test-flow"
        assert event.data["workflow_name"] == "test-workflow"

    async def test_flow_complete_event(self, mock_engine: AgentFlowEngine) -> None:
        """フロー完了イベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        hook_event = HookEvent(
            hook_type=HookType.ON_COMPLETE,
            workflow_name="test-workflow",
            data={"result": {"status": "success"}},
        )
        await emitter._on_flow_complete(hook_event)

        event = await emitter._event_queue.get()
        assert isinstance(event, FlowCompleteEvent)
        assert event.event_type == AGUIEventType.FLOW_COMPLETE
        assert event.data["result"]["status"] == "success"

    async def test_flow_error_event(self, mock_engine: AgentFlowEngine) -> None:
        """フローエラーイベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        test_error = ValueError("Test error")
        hook_event = HookEvent(
            hook_type=HookType.ON_ERROR,
            workflow_name="test-workflow",
            data={"error": test_error},
        )
        await emitter._on_flow_error(hook_event)

        event = await emitter._event_queue.get()
        assert isinstance(event, FlowErrorEvent)
        assert event.event_type == AGUIEventType.FLOW_ERROR
        assert event.error_message == "Test error"
        assert event.error_type == "ValueError"

    async def test_flow_cancel_event(self, mock_engine: AgentFlowEngine) -> None:
        """フローキャンセルイベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        hook_event = HookEvent(
            hook_type=HookType.ON_CANCEL,
            workflow_name="test-workflow",
            data={},
        )
        await emitter._on_flow_cancel(hook_event)

        event = await emitter._event_queue.get()
        assert isinstance(event, FlowCancelEvent)
        assert event.event_type == AGUIEventType.FLOW_CANCEL

    async def test_node_start_event(self, mock_engine: AgentFlowEngine) -> None:
        """ノード開始イベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        hook_event = HookEvent(
            hook_type=HookType.ON_NODE_EXEC,
            workflow_name="test-workflow",
            data={"node_id": "node-1", "node_name": "Test Node"},
        )
        await emitter._on_node_start(hook_event)

        # ノード開始イベントとプログレスイベントが発行される
        assert emitter.get_queue_size() == 2

        node_event = await emitter._event_queue.get()
        assert isinstance(node_event, NodeStartEvent)
        assert node_event.node_id == "node-1"
        assert node_event.node_name == "Test Node"

        progress_event = await emitter._event_queue.get()
        assert isinstance(progress_event, ProgressEvent)
        assert progress_event.current == 0
        assert progress_event.total == 1

    async def test_node_complete_event(self, mock_engine: AgentFlowEngine) -> None:
        """ノード完了イベントをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        # まずノードを開始
        await emitter._on_node_start(
            HookEvent(
                hook_type=HookType.ON_NODE_EXEC,
                workflow_name="test-workflow",
                data={"node_id": "node-1", "node_name": "Test Node"},
            )
        )

        # キューをクリア
        while not emitter._event_queue.empty():
            await emitter._event_queue.get()

        # ノード完了
        hook_event = HookEvent(
            hook_type=HookType.ON_NODE_COMPLETE,
            workflow_name="test-workflow",
            data={
                "node_id": "node-1",
                "node_name": "Test Node",
                "result": {"output": "test"},
            },
        )
        await emitter._on_node_complete(hook_event)

        # ノード完了イベントとプログレスイベントが発行される
        assert emitter.get_queue_size() == 2

        node_event = await emitter._event_queue.get()
        assert isinstance(node_event, NodeCompleteEvent)
        assert node_event.node_id == "node-1"

        progress_event = await emitter._event_queue.get()
        assert isinstance(progress_event, ProgressEvent)
        assert progress_event.current == 1
        assert progress_event.total == 1
        assert progress_event.percentage == 100.0

    async def test_emit_log(self, mock_engine: AgentFlowEngine) -> None:
        """ログイベント発行をテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        await emitter.emit_log("INFO", "Test log message", "test-source")

        event = await emitter._event_queue.get()
        assert isinstance(event, LogEvent)
        assert event.level == "INFO"
        assert event.message == "Test log message"
        assert event.source == "test-source"

    async def test_stream_events(self, mock_engine: AgentFlowEngine) -> None:
        """イベントストリームをテスト."""
        emitter = AGUIEventEmitter(mock_engine)
        await emitter.attach_to_flow("test-flow")

        # いくつかのイベントを発行
        await emitter.emit_log("INFO", "Message 1")
        await emitter.emit_log("INFO", "Message 2")
        await emitter.detach_from_flow()  # ストリーム終了シグナル

        # ストリームから読み取り
        events = []
        async for event in emitter.stream_events():
            events.append(event)

        assert len(events) == 2
        assert all(isinstance(e, LogEvent) for e in events)

    async def test_queue_backpressure(self, mock_engine: AgentFlowEngine) -> None:
        """キューバックプレッシャーをテスト."""
        # 小さいキューサイズでエミッターを作成
        emitter = AGUIEventEmitter(mock_engine, max_queue_size=2)
        await emitter.attach_to_flow("test-flow")

        # キューを満杯にする
        await emitter.emit_log("INFO", "Message 1")
        await emitter.emit_log("INFO", "Message 2")

        # 3つ目のメッセージはタイムアウトでドロップされる
        await emitter.emit_log("INFO", "Message 3")

        # キューサイズは2のまま
        assert emitter.get_queue_size() == 2

    async def test_event_without_flow_id(self, mock_engine: AgentFlowEngine) -> None:
        """flow_id なしでのイベント発行をテスト."""
        emitter = AGUIEventEmitter(mock_engine)

        # attach_to_flow を呼ばずにイベントを発行
        hook_event = HookEvent(
            hook_type=HookType.ON_START,
            workflow_name="test-workflow",
            data={},
        )
        await emitter._on_flow_start(hook_event)

        # イベントは発行されない
        assert emitter.get_queue_size() == 0

