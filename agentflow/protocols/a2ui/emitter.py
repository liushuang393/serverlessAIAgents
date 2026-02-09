"""A2UI エミッター - AG-UI 統合.

このモジュールは A2UI コンポーネントを AG-UI イベントとして配信します。
AGUIEventEmitter と統合して使用します。
"""

import json
import logging
from collections.abc import AsyncIterator
from typing import Any

from agentflow.protocols.a2ui.components import A2UIComponent


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
        # AG-UI の ProgressEvent として配信
        if hasattr(self._agui_emitter, "emit_progress"):
            await self._agui_emitter.emit_progress(
                message=json.dumps(event_data),
                progress=0.0,
            )

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

