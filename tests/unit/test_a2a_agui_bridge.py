"""A2A → AG-UI/A2UI ブリッジのユニットテスト."""

from unittest.mock import AsyncMock

from kernel.protocols.a2a.agui_bridge import A2AToAGUIBridge
from kernel.protocols.a2a.types import (
    A2ATaskState,
    Artifact,
    FilePart,
    Message,
    Role,
    TaskStatus,
)
from kernel.protocols.agui_events import AGUIEventType


# ============================================================
# ステータス → AG-UI イベント変換テスト
# ============================================================


class TestStatusToAGUIEvent:
    """_status_to_agui_event のテスト."""

    def setup_method(self) -> None:
        """各テスト前にブリッジを初期化."""
        self.bridge = A2AToAGUIBridge(flow_id="test-flow")

    def test_submitted_to_flow_start(self) -> None:
        """submitted → flow.start."""
        status = TaskStatus(state=A2ATaskState.SUBMITTED)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.FLOW_START
        assert event.data["task_id"] == "t-1"

    def test_working_with_message_to_progress(self) -> None:
        """working + メッセージ → progress."""
        msg = Message.from_text("処理中...", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.WORKING, message=msg)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.PROGRESS
        assert event.data["message"] == "処理中..."

    def test_working_without_message_to_node_start(self) -> None:
        """working + メッセージなし → node.start."""
        status = TaskStatus(state=A2ATaskState.WORKING)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.NODE_START

    def test_input_required_to_clarification(self) -> None:
        """input_required → clarification.required."""
        msg = Message.from_text("入力してください", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.INPUT_REQUIRED, message=msg)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.CLARIFICATION_REQUIRED

    def test_completed_to_flow_complete(self) -> None:
        """completed → flow.complete."""
        status = TaskStatus(state=A2ATaskState.COMPLETED)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.FLOW_COMPLETE

    def test_failed_to_flow_error(self) -> None:
        """failed → flow.error."""
        msg = Message.from_text("エラー発生", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.FAILED, message=msg)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.FLOW_ERROR
        assert event.data["error"] == "エラー発生"

    def test_canceled_to_flow_cancel(self) -> None:
        """canceled → flow.cancel."""
        status = TaskStatus(state=A2ATaskState.CANCELED)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is not None
        assert event.event_type == AGUIEventType.FLOW_CANCEL

    def test_unknown_state_returns_none(self) -> None:
        """未知の状態は None."""
        status = TaskStatus(state=A2ATaskState.UNKNOWN)
        event = self.bridge._status_to_agui_event("t-1", status)
        assert event is None


# ============================================================
# on_status_update 統合テスト
# ============================================================


class TestOnStatusUpdate:
    """on_status_update のテスト."""

    async def test_バッファにイベントが蓄積される(self) -> None:
        """emitter なしでもバッファにイベントが記録される."""
        bridge = A2AToAGUIBridge()
        status = TaskStatus(state=A2ATaskState.SUBMITTED)
        await bridge.on_status_update("t-1", status)
        events = bridge.get_buffered_events()
        assert len(events) == 1
        assert events[0].event_type == AGUIEventType.FLOW_START

    async def test_agui_emitterに配信される(self) -> None:
        """agui_emitter.emit() が呼ばれる."""
        mock_emitter = AsyncMock()
        bridge = A2AToAGUIBridge(agui_emitter=mock_emitter)
        status = TaskStatus(state=A2ATaskState.COMPLETED)
        await bridge.on_status_update("t-1", status)
        mock_emitter.emit.assert_called_once()

    async def test_unknownはイベント発火しない(self) -> None:
        """unknown 状態はイベントを発火しない."""
        bridge = A2AToAGUIBridge()
        status = TaskStatus(state=A2ATaskState.UNKNOWN)
        await bridge.on_status_update("t-1", status)
        assert len(bridge.get_buffered_events()) == 0


# ============================================================
# on_artifact_update テスト
# ============================================================


class TestOnArtifactUpdate:
    """on_artifact_update のテスト."""

    async def test_テキストアーティファクト(self) -> None:
        """テキストアーティファクトで AG-UI progress イベントが発火."""
        bridge = A2AToAGUIBridge()
        artifact = Artifact.from_text("テスト結果")
        await bridge.on_artifact_update("t-1", artifact)
        events = bridge.get_buffered_events()
        assert len(events) == 1
        assert events[0].event_type == AGUIEventType.PROGRESS
        assert events[0].data["task_id"] == "t-1"

    async def test_データアーティファクト(self) -> None:
        """構造化データアーティファクト."""
        bridge = A2AToAGUIBridge()
        artifact = Artifact.from_data({"score": 0.95})
        await bridge.on_artifact_update("t-1", artifact)
        assert len(bridge.get_buffered_events()) == 1

    async def test_a2ui_emitterにテキストコンポーネントが配信される(self) -> None:
        """A2UI emitter にテキストコンポーネントが配信される."""
        mock_a2ui = AsyncMock()
        bridge = A2AToAGUIBridge(a2ui_emitter=mock_a2ui)
        artifact = Artifact.from_text("テキスト")
        await bridge.on_artifact_update("t-1", artifact)
        mock_a2ui.emit_component.assert_called_once()

    async def test_a2ui_emitterにカードコンポーネントが配信される(self) -> None:
        """DataPart → A2UI Card コンポーネント."""
        mock_a2ui = AsyncMock()
        bridge = A2AToAGUIBridge(a2ui_emitter=mock_a2ui)
        artifact = Artifact.from_data({"key": "val"}, name="結果カード")
        await bridge.on_artifact_update("t-1", artifact)
        mock_a2ui.emit_component.assert_called_once()

    async def test_a2ui_emitterにイメージコンポーネントが配信される(self) -> None:
        """FilePart → A2UI Image コンポーネント."""
        mock_a2ui = AsyncMock()
        bridge = A2AToAGUIBridge(a2ui_emitter=mock_a2ui)
        artifact = Artifact(parts=[FilePart(uri="file:///img.png", name="photo")])
        await bridge.on_artifact_update("t-1", artifact)
        mock_a2ui.emit_component.assert_called_once()


# ============================================================
# バッファ操作テスト
# ============================================================


class TestBufferOperations:
    """イベントバッファのテスト."""

    async def test_バッファクリア(self) -> None:
        """clear_buffer でバッファが空になる."""
        bridge = A2AToAGUIBridge()
        status = TaskStatus(state=A2ATaskState.SUBMITTED)
        await bridge.on_status_update("t-1", status)
        assert len(bridge.get_buffered_events()) == 1
        bridge.clear_buffer()
        assert len(bridge.get_buffered_events()) == 0

    async def test_flow_idが設定される(self) -> None:
        """カスタム flow_id がイベントに設定される."""
        bridge = A2AToAGUIBridge(flow_id="custom-flow")
        status = TaskStatus(state=A2ATaskState.SUBMITTED)
        await bridge.on_status_update("t-1", status)
        events = bridge.get_buffered_events()
        assert events[0].flow_id == "custom-flow"


# ============================================================
# input_required → A2UI Form テスト
# ============================================================


class TestInputRequiredA2UI:
    """input_required の A2UI Form 変換テスト."""

    async def test_input_requiredでa2ui_formが配信される(self) -> None:
        """input_required → A2UI Form コンポーネント."""
        mock_a2ui = AsyncMock()
        bridge = A2AToAGUIBridge(a2ui_emitter=mock_a2ui)
        msg = Message.from_text("ファイルを選択してください", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.INPUT_REQUIRED, message=msg)
        await bridge.on_status_update("t-1", status)
        mock_a2ui.emit_component.assert_called_once()

    async def test_a2ui_emitterなしでもエラーにならない(self) -> None:
        """A2UI emitter なしで input_required を処理してもエラーなし."""
        bridge = A2AToAGUIBridge()
        msg = Message.from_text("入力してください", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.INPUT_REQUIRED, message=msg)
        # エラーなく実行完了
        await bridge.on_status_update("t-1", status)
        assert len(bridge.get_buffered_events()) == 1
