"""A2A → AG-UI / A2UI 自動変換ブリッジ.

A2ATaskManager のオブザーバーとして登録され、
タスクイベントが発火するたびに AG-UI / A2UI イベントに変換して配信する。
Agent 開発者はこのクラスを意識する必要なし。

既存活用（変更不要）:
- agentflow/protocols/agui_events.py — AGUIEvent, FlowStartEvent 等
- agentflow/protocols/a2ui/emitter.py — A2UIEmitter.emit_component()
- agentflow/protocols/a2ui/components.py — A2UIComponent
- agentflow/api/sse_emitter.py — SSEEmitter
"""

from __future__ import annotations

import logging
import time
from typing import Any

from kernel.protocols.a2a.types import (
    A2ATaskState,
    Artifact,
    DataPart,
    FilePart,
    TaskStatus,
    TextPart,
)
from kernel.protocols.agui_events import (
    AGUIEvent,
    ClarificationRequiredEvent,
    FlowCancelEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    NodeStartEvent,
    ProgressEvent,
)


class A2AToAGUIBridge:
    """A2A イベント → AG-UI / A2UI 自動変換ブリッジ.

    TaskEventObserver プロトコルを実装し、A2ATaskManager から通知を受ける。
    """

    def __init__(
        self,
        *,
        agui_emitter: Any | None = None,
        a2ui_emitter: Any | None = None,
        flow_id: str = "a2a",
    ) -> None:
        """初期化.

        Args:
            agui_emitter: AGUIEventEmitter インスタンス（オプション）
            a2ui_emitter: A2UIEmitter インスタンス（オプション）
            flow_id: AG-UI イベントのフロー ID
        """
        self._agui_emitter = agui_emitter
        self._a2ui_emitter = a2ui_emitter
        self._flow_id = flow_id
        self._logger = logging.getLogger("agentflow.a2a.agui_bridge")
        # イベントバッファ（emitter がない場合の記録用）
        self._event_buffer: list[AGUIEvent] = []

    async def on_status_update(self, task_id: str, status: TaskStatus) -> None:
        """A2A TaskStatusUpdate → AG-UI イベント変換.

        Args:
            task_id: タスク ID
            status: 更新後の状態
        """
        agui_event = self._status_to_agui_event(task_id, status)
        if agui_event is not None:
            await self._emit_agui(agui_event)

        # A2UI: input_required の場合は入力促進
        if status.state == A2ATaskState.INPUT_REQUIRED and self._a2ui_emitter:
            await self._emit_a2ui_input_required(task_id, status)

    async def on_artifact_update(self, task_id: str, artifact: Artifact) -> None:
        """A2A TaskArtifactUpdate → AG-UI data + A2UI コンポーネント.

        Args:
            task_id: タスク ID
            artifact: 追加されたアーティファクト
        """
        # AG-UI: data イベント
        data_event = ProgressEvent(
            timestamp=time.time(),
            flow_id=self._flow_id,
            current=0,
            total=1,
            percentage=0.0,
            data={
                "task_id": task_id,
                "artifact_id": artifact.artifact_id,
                "artifact": artifact.model_dump(),
            },
        )
        await self._emit_agui(data_event)

        # A2UI: コンテンツ型に応じたリッチコンポーネント
        if self._a2ui_emitter:
            await self._emit_a2ui_artifact(artifact)

    def _status_to_agui_event(self, task_id: str, status: TaskStatus) -> AGUIEvent | None:
        """A2A 状態 → AG-UI イベントに変換."""
        now = time.time()
        state = status.state
        msg_text = ""
        if status.message:
            msg_text = status.message.text

        if state == A2ATaskState.SUBMITTED:
            return FlowStartEvent(
                timestamp=now,
                flow_id=self._flow_id,
                data={"task_id": task_id},
            )
        if state == A2ATaskState.WORKING:
            if msg_text:
                return ProgressEvent(
                    timestamp=now,
                    flow_id=self._flow_id,
                    current=0,
                    total=1,
                    percentage=0.0,
                    data={"task_id": task_id, "message": msg_text, "percentage": 0},
                )
            return NodeStartEvent(
                timestamp=now,
                flow_id=self._flow_id,
                node_id=task_id,
                node_name=task_id,
                data={"task_id": task_id},
            )
        if state == A2ATaskState.INPUT_REQUIRED:
            return ClarificationRequiredEvent(
                timestamp=now,
                flow_id=self._flow_id,
                original_question=msg_text or "Additional input required",
                questions=[],
                message=msg_text,
                data={"task_id": task_id},
            )
        if state == A2ATaskState.COMPLETED:
            return FlowCompleteEvent(
                timestamp=now,
                flow_id=self._flow_id,
                data={"task_id": task_id, "message": msg_text},
            )
        if state == A2ATaskState.FAILED:
            return FlowErrorEvent(
                timestamp=now,
                flow_id=self._flow_id,
                error_message=msg_text or "Task failed",
                error_type="A2ATaskFailed",
                data={"task_id": task_id, "error": msg_text},
            )
        if state == A2ATaskState.CANCELED:
            return FlowCancelEvent(
                timestamp=now,
                flow_id=self._flow_id,
                data={"task_id": task_id},
            )
        return None

    async def _emit_agui(self, event: AGUIEvent) -> None:
        """AG-UI イベントを配信."""
        self._event_buffer.append(event)
        self._logger.debug(f"AG-UI イベント: {event.event_type.value}")

        if self._agui_emitter and hasattr(self._agui_emitter, "emit"):
            await self._agui_emitter.emit(event)

    async def _emit_a2ui_input_required(self, task_id: str, status: TaskStatus) -> None:
        """A2UI: 入力要求のコンポーネント配信."""
        emitter = self._a2ui_emitter
        if emitter is None:
            return
        try:
            from kernel.protocols.a2ui.components import (
                A2UIComponent,
                ComponentType,
            )

            msg_text = status.message.text if status.message else "入力が必要です"
            component = A2UIComponent(
                component_type=ComponentType.FORM,
                id=f"input_{task_id}",
                props={"label": msg_text, "task_id": task_id},
            )
            await emitter.emit_component(component)
        except ImportError:
            self._logger.debug("A2UI コンポーネントモジュールが利用不可")

    async def _emit_a2ui_artifact(self, artifact: Artifact) -> None:
        """A2UI: アーティファクトのリッチコンポーネント配信."""
        emitter = self._a2ui_emitter
        if emitter is None:
            return
        try:
            from kernel.protocols.a2ui.components import (
                A2UIComponent,
                ComponentType,
            )

            for part in artifact.parts:
                if isinstance(part, TextPart):
                    component = A2UIComponent(
                        component_type=ComponentType.TEXT,
                        props={"text": part.text},
                    )
                    await emitter.emit_component(component)
                elif isinstance(part, DataPart):
                    component = A2UIComponent(
                        component_type=ComponentType.CARD,
                        props={"data": part.data, "title": artifact.name or "結果"},
                    )
                    await emitter.emit_component(component)
                elif isinstance(part, FilePart):
                    component = A2UIComponent(
                        component_type=ComponentType.IMAGE,
                        props={"src": part.uri, "alt": part.name or ""},
                    )
                    await emitter.emit_component(component)
        except ImportError:
            self._logger.debug("A2UI コンポーネントモジュールが利用不可")

    def get_buffered_events(self) -> list[AGUIEvent]:
        """バッファされた AG-UI イベントを取得（テスト/デバッグ用）."""
        return list(self._event_buffer)

    def clear_buffer(self) -> None:
        """イベントバッファをクリア."""
        self._event_buffer.clear()
