"""A2UI エミッター - AG-UI 統合.

このモジュールは A2UI コンポーネントを typed AG-UI イベントとして配信します。
AGUIEventEmitter と統合して使用します。
"""

import logging
import time
from collections.abc import AsyncIterator
from typing import Any

from kernel.protocols.a2ui.components import A2UIComponent
from kernel.protocols.agui_events import (
    A2UIClearEvent,
    A2UIComponentEvent,
    A2UIUpdateEvent,
)


class A2UIEmitter:
    """A2UI エミッター - コンポーネントを AG-UI イベントとして配信.

    Example:
        >>> emitter = A2UIEmitter(agui_emitter)
        >>> card = CardComponent(title="結果", children=[...])
        >>> await emitter.emit_component(card)
    """

    def __init__(self, agui_emitter: Any | None = None) -> None:
        """初期化.

        Args:
            agui_emitter: AGUIEventEmitter インスタンス（オプション）
        """
        self._agui_emitter = agui_emitter
        self._logger = logging.getLogger(__name__)
        self._event_queue: list[dict[str, Any]] = []

    async def emit_component(
        self,
        component: A2UIComponent,
        surface_id: str = "main",
    ) -> None:
        """コンポーネントを配信.

        Args:
            component: 配信するコンポーネント
            surface_id: 描画先サーフェス ID
        """
        event_data = {
            "type": "a2ui_component",
            "surface_id": surface_id,
            "component": component.to_dict(),
        }

        self._event_queue.append(event_data)
        self._logger.debug(f"Emitted component: {component.component_type.value}")

        # AG-UI エミッターがあれば連携
        if self._agui_emitter:
            await self._emit_to_agui(event_data)

    async def emit_update(
        self,
        component_id: str,
        updates: dict[str, Any],
        surface_id: str = "main",
    ) -> None:
        """コンポーネントを更新.

        Args:
            component_id: 更新対象コンポーネント ID
            updates: 更新内容
            surface_id: 描画先サーフェス ID
        """
        event_data = {
            "type": "a2ui_update",
            "surface_id": surface_id,
            "component_id": component_id,
            "updates": updates,
        }

        self._event_queue.append(event_data)
        self._logger.debug(f"Emitted update for: {component_id}")

        if self._agui_emitter:
            await self._emit_to_agui(event_data)

    async def emit_clear(self, surface_id: str = "main") -> None:
        """サーフェスをクリア.

        Args:
            surface_id: クリア対象サーフェス ID
        """
        event_data = {
            "type": "a2ui_clear",
            "surface_id": surface_id,
        }

        self._event_queue.append(event_data)
        self._logger.debug(f"Cleared surface: {surface_id}")

        if self._agui_emitter:
            await self._emit_to_agui(event_data)

    async def _emit_to_agui(self, event_data: dict[str, Any]) -> None:
        """AG-UI イベントとして配信.

        Args:
            event_data: イベントデータ
        """
        emitter = self._agui_emitter
        if emitter is None:
            return

        flow_id = getattr(emitter, "_flow_id", None) or "a2ui"
        event_type = str(event_data.get("type", "")).strip()
        agui_event: A2UIComponentEvent | A2UIUpdateEvent | A2UIClearEvent
        if event_type == "a2ui_component":
            agui_event = A2UIComponentEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                surface_id=str(event_data.get("surface_id", "main")),
                component=dict(event_data.get("component", {})),
                data={},
            )
        elif event_type == "a2ui_update":
            agui_event = A2UIUpdateEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                surface_id=str(event_data.get("surface_id", "main")),
                component_id=str(event_data.get("component_id", "")),
                updates=dict(event_data.get("updates", {})),
                data={},
            )
        elif event_type == "a2ui_clear":
            agui_event = A2UIClearEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                surface_id=str(event_data.get("surface_id", "main")),
                data={},
            )
        else:
            self._logger.debug("Unknown A2UI event type skipped: %s", event_type)
            return

        if hasattr(emitter, "emit"):
            await emitter.emit(agui_event)
        elif hasattr(emitter, "_emit_event"):
            await emitter._emit_event(agui_event)

    async def stream_events(self) -> AsyncIterator[dict[str, Any]]:
        """イベントをストリーム.

        Yields:
            イベントデータ
        """
        while self._event_queue:
            yield self._event_queue.pop(0)

    def get_pending_events(self) -> list[dict[str, Any]]:
        """未配信イベントを取得.

        Returns:
            イベントリスト
        """
        return list(self._event_queue)

    def clear_events(self) -> None:
        """イベントキューをクリア."""
        self._event_queue.clear()
