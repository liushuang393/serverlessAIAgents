"""Layer 4 Replay - フロー再生サービス.

過去のフロー実行を記録・再生し、デバッグやテストに活用する。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import StrEnum
from typing import Any


_logger = logging.getLogger(__name__)


class ReplayMode(StrEnum):
    """再生モード."""

    FULL = "full"
    STEP_BY_STEP = "step_by_step"
    FROM_CHECKPOINT = "from_checkpoint"


@dataclass
class ReplayEvent:
    """再生イベント.

    Attributes:
        event_type: イベントタイプ
        timestamp: タイムスタンプ
        payload: ペイロード
        step_index: ステップインデックス
    """

    event_type: str
    timestamp: datetime
    payload: dict[str, Any] = field(default_factory=dict)
    step_index: int = 0


@dataclass
class ReplaySession:
    """再生セッション.

    Attributes:
        session_id: セッションID
        flow_id: フローID
        events: イベントリスト
        mode: 再生モード
    """

    session_id: str
    flow_id: str
    events: list[ReplayEvent] = field(default_factory=list)
    mode: ReplayMode = ReplayMode.FULL


class ReplayRecorder:
    """フロー実行記録器."""

    def __init__(self) -> None:
        """初期化."""
        self._events: list[ReplayEvent] = []

    def record(self, event_type: str, payload: dict[str, Any], step_index: int = 0) -> None:
        """イベントを記録.

        Args:
            event_type: イベントタイプ
            payload: ペイロード
            step_index: ステップインデックス
        """
        self._events.append(
            ReplayEvent(
                event_type=event_type,
                timestamp=datetime.now(tz=UTC),
                payload=payload,
                step_index=step_index,
            )
        )

    def get_events(self) -> list[ReplayEvent]:
        """記録済みイベントを取得."""
        return list(self._events)

    def clear(self) -> None:
        """記録をクリア."""
        self._events.clear()


__all__ = [
    "ReplayEvent",
    "ReplayMode",
    "ReplayRecorder",
    "ReplaySession",
]
