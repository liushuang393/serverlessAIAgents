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


@dataclass
class StepDiscrepancy:
    """再生時の不一致情報.

    Attributes:
        step_index: 不一致が発生したステップ番号
        field: 不一致のフィールド名
        original: 元の値
        replayed: 再生時の値
    """

    step_index: int
    field: str
    original: Any
    replayed: Any


@dataclass
class ReplayResult:
    """再生結果.

    Attributes:
        replay_id: 再生対象のセッションID
        steps_executed: 実行されたステップ数
        success: 全ステップが不一致なく完了したか
        discrepancies: 不一致リスト
    """

    replay_id: str
    steps_executed: int
    success: bool
    discrepancies: list[StepDiscrepancy] = field(default_factory=list)


class ReplayRunner:
    """記録済みフローの再生実行器.

    ReplayRecorder で記録されたイベントを再実行し、
    元の結果との差異を検出する。
    """

    def __init__(self) -> None:
        """初期化."""
        self._sessions: dict[str, ReplaySession] = {}

    def register_session(self, session: ReplaySession) -> None:
        """再生対象のセッションを登録.

        Args:
            session: 再生セッション
        """
        self._sessions[session.session_id] = session

    async def run(self, replay_id: str) -> ReplayResult:
        """記録済みステップを再実行し、元の結果と比較する.

        Args:
            replay_id: 再生対象のセッションID

        Returns:
            再生結果（不一致情報を含む）
        """
        session = self._sessions.get(replay_id)
        if session is None:
            _logger.warning("再生セッションが見つかりません: %s", replay_id)
            return ReplayResult(
                replay_id=replay_id,
                steps_executed=0,
                success=False,
                discrepancies=[
                    StepDiscrepancy(
                        step_index=0,
                        field="session",
                        original=replay_id,
                        replayed=None,
                    )
                ],
            )

        discrepancies: list[StepDiscrepancy] = []
        steps_executed = 0

        for event in session.events:
            steps_executed += 1
            # 再実行: ペイロードの expected_result と actual_result を比較
            expected = event.payload.get("expected_result")
            actual = await self._replay_step(event)

            if expected is not None and actual != expected:
                discrepancies.append(
                    StepDiscrepancy(
                        step_index=event.step_index,
                        field=event.event_type,
                        original=expected,
                        replayed=actual,
                    )
                )

            _logger.debug(
                "ステップ再生: index=%d, type=%s, match=%s",
                event.step_index,
                event.event_type,
                expected == actual,
            )

        success = len(discrepancies) == 0
        _logger.info(
            "再生完了: id=%s, steps=%d, success=%s, discrepancies=%d",
            replay_id,
            steps_executed,
            success,
            len(discrepancies),
        )

        return ReplayResult(
            replay_id=replay_id,
            steps_executed=steps_executed,
            success=success,
            discrepancies=discrepancies,
        )

    async def _replay_step(self, event: ReplayEvent) -> Any:
        """単一ステップを再実行する.

        Args:
            event: 再生対象のイベント

        Returns:
            再実行結果（ペイロードの replay_result、なければ None）
        """
        # 基本実装: ペイロード内の replay_result を返す
        # サブクラスで実際のステップ実行ロジックをオーバーライド可能
        return event.payload.get("replay_result")


__all__ = [
    "ReplayEvent",
    "ReplayMode",
    "ReplayRecorder",
    "ReplayResult",
    "ReplayRunner",
    "ReplaySession",
    "StepDiscrepancy",
]
