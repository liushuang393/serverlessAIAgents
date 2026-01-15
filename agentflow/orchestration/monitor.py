# -*- coding: utf-8 -*-
"""監視Agent - 実行監視とアラート.

実行状況を監視し、異常を検出してアラートを発行する。

設計原則:
- リアルタイム監視
- 異常検出とアラート
- 自動リカバリ提案

使用例:
    >>> from agentflow.orchestration.monitor import MonitorAgent
    >>>
    >>> monitor = MonitorAgent()
    >>> monitor.start_monitoring(execution_id)
    >>>
    >>> async for event in monitor.watch():
    ...     if event.event_type == MonitorEventType.ALERT:
    ...         handle_alert(event)
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field


class MonitorEventType(str, Enum):
    """監視イベント種別."""

    STARTED = "started"           # 実行開始
    PROGRESS = "progress"         # 進捗更新
    STEP_STARTED = "step_started"  # ステップ開始
    STEP_COMPLETED = "step_completed"  # ステップ完了
    STEP_FAILED = "step_failed"   # ステップ失敗
    ALERT = "alert"               # アラート
    WARNING = "warning"           # 警告
    COMPLETED = "completed"       # 実行完了
    FAILED = "failed"             # 実行失敗
    TIMEOUT = "timeout"           # タイムアウト
    STUCK = "stuck"               # スタック検出
    RECOVERY = "recovery"         # リカバリ提案


class AlertSeverity(str, Enum):
    """アラート深刻度."""

    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class MonitorEvent:
    """監視イベント.

    Attributes:
        event_type: イベント種別
        execution_id: 実行ID
        step_id: ステップID（該当する場合）
        message: メッセージ
        severity: 深刻度
        data: 追加データ
        timestamp: タイムスタンプ
    """

    event_type: MonitorEventType
    execution_id: str
    step_id: str | None = None
    message: str = ""
    severity: AlertSeverity = AlertSeverity.INFO
    data: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "event_type": self.event_type.value,
            "execution_id": self.execution_id,
            "step_id": self.step_id,
            "message": self.message,
            "severity": self.severity.value,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
        }


class MonitorThresholds(BaseModel):
    """監視閾値.

    Attributes:
        step_timeout_warning: ステップタイムアウト警告（秒）
        stuck_detection_interval: スタック検出間隔（秒）
        max_consecutive_failures: 最大連続失敗数
        progress_stall_threshold: 進捗停滞閾値（秒）
    """

    step_timeout_warning: float = Field(default=30.0, ge=1.0)
    stuck_detection_interval: float = Field(default=60.0, ge=10.0)
    max_consecutive_failures: int = Field(default=3, ge=1)
    progress_stall_threshold: float = Field(default=120.0, ge=30.0)


@dataclass
class ExecutionState:
    """実行状態.

    監視対象の実行状態を追跡。
    """

    execution_id: str
    started_at: datetime = field(default_factory=datetime.now)
    last_activity: datetime = field(default_factory=datetime.now)
    current_step_id: str | None = None
    current_step_started: datetime | None = None
    completed_steps: int = 0
    failed_steps: int = 0
    consecutive_failures: int = 0
    progress: float = 0.0
    is_active: bool = True


class MonitorAgent:
    """監視Agent.

    実行状況を監視し、異常を検出する。

    主な機能:
    - リアルタイム実行監視
    - タイムアウト検出
    - スタック検出
    - アラート発行
    - リカバリ提案

    Example:
        >>> monitor = MonitorAgent()
        >>> monitor.start_monitoring("exec-123")
        >>>
        >>> # イベントを監視
        >>> async for event in monitor.watch("exec-123"):
        ...     if event.event_type == MonitorEventType.ALERT:
        ...         handle_alert(event)
    """

    def __init__(
        self,
        thresholds: MonitorThresholds | None = None,
        alert_callback: Callable[[MonitorEvent], Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            thresholds: 監視閾値
            alert_callback: アラートコールバック
        """
        self._thresholds = thresholds or MonitorThresholds()
        self._alert_callback = alert_callback
        self._executions: dict[str, ExecutionState] = {}
        self._event_queue: asyncio.Queue[MonitorEvent] = asyncio.Queue()
        self._monitor_tasks: dict[str, asyncio.Task[None]] = {}
        self._logger = logging.getLogger(__name__)

    def start_monitoring(self, execution_id: str) -> None:
        """実行の監視を開始.

        Args:
            execution_id: 実行ID
        """
        if execution_id in self._executions:
            self._logger.warning(f"既に監視中: {execution_id}")
            return

        state = ExecutionState(execution_id=execution_id)
        self._executions[execution_id] = state

        # 監視タスクを開始
        task = asyncio.create_task(self._monitor_loop(execution_id))
        self._monitor_tasks[execution_id] = task

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.STARTED,
            execution_id=execution_id,
            message="監視を開始しました",
        ))

        self._logger.info(f"監視開始: {execution_id}")

    def stop_monitoring(self, execution_id: str) -> None:
        """実行の監視を停止.

        Args:
            execution_id: 実行ID
        """
        if execution_id not in self._executions:
            return

        self._executions[execution_id].is_active = False

        if execution_id in self._monitor_tasks:
            self._monitor_tasks[execution_id].cancel()
            del self._monitor_tasks[execution_id]

        del self._executions[execution_id]
        self._logger.info(f"監視停止: {execution_id}")

    async def watch(
        self,
        execution_id: str | None = None,
    ):
        """イベントを監視.

        Args:
            execution_id: 実行ID（Noneの場合は全て）

        Yields:
            MonitorEvent
        """
        while True:
            try:
                event = await asyncio.wait_for(
                    self._event_queue.get(),
                    timeout=1.0,
                )

                if execution_id is None or event.execution_id == execution_id:
                    yield event

                # 終了イベントでループを抜ける
                if event.event_type in (
                    MonitorEventType.COMPLETED,
                    MonitorEventType.FAILED,
                ):
                    if execution_id and event.execution_id == execution_id:
                        break

            except asyncio.TimeoutError:
                # 監視対象がなくなったらループを抜ける
                if execution_id and execution_id not in self._executions:
                    break
                continue

    def report_step_started(
        self,
        execution_id: str,
        step_id: str,
        step_name: str = "",
    ) -> None:
        """ステップ開始を報告.

        Args:
            execution_id: 実行ID
            step_id: ステップID
            step_name: ステップ名
        """
        state = self._executions.get(execution_id)
        if not state:
            return

        state.current_step_id = step_id
        state.current_step_started = datetime.now()
        state.last_activity = datetime.now()

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.STEP_STARTED,
            execution_id=execution_id,
            step_id=step_id,
            message=f"ステップ開始: {step_name}",
            data={"step_name": step_name},
        ))

    def report_step_completed(
        self,
        execution_id: str,
        step_id: str,
        step_name: str = "",
        duration_ms: float = 0.0,
    ) -> None:
        """ステップ完了を報告.

        Args:
            execution_id: 実行ID
            step_id: ステップID
            step_name: ステップ名
            duration_ms: 実行時間
        """
        state = self._executions.get(execution_id)
        if not state:
            return

        state.current_step_id = None
        state.current_step_started = None
        state.last_activity = datetime.now()
        state.completed_steps += 1
        state.consecutive_failures = 0

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.STEP_COMPLETED,
            execution_id=execution_id,
            step_id=step_id,
            message=f"ステップ完了: {step_name}",
            data={"step_name": step_name, "duration_ms": duration_ms},
        ))

    def report_step_failed(
        self,
        execution_id: str,
        step_id: str,
        step_name: str = "",
        error: str = "",
    ) -> None:
        """ステップ失敗を報告.

        Args:
            execution_id: 実行ID
            step_id: ステップID
            step_name: ステップ名
            error: エラーメッセージ
        """
        state = self._executions.get(execution_id)
        if not state:
            return

        state.current_step_id = None
        state.current_step_started = None
        state.last_activity = datetime.now()
        state.failed_steps += 1
        state.consecutive_failures += 1

        severity = AlertSeverity.ERROR
        if state.consecutive_failures >= self._thresholds.max_consecutive_failures:
            severity = AlertSeverity.CRITICAL

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.STEP_FAILED,
            execution_id=execution_id,
            step_id=step_id,
            message=f"ステップ失敗: {step_name} - {error}",
            severity=severity,
            data={"step_name": step_name, "error": error},
        ))

        # 連続失敗が閾値を超えたらアラート
        if state.consecutive_failures >= self._thresholds.max_consecutive_failures:
            self._emit_alert(
                execution_id,
                f"連続{state.consecutive_failures}回の失敗を検出",
                AlertSeverity.CRITICAL,
                {"consecutive_failures": state.consecutive_failures},
            )

    def report_progress(
        self,
        execution_id: str,
        progress: float,
        message: str = "",
    ) -> None:
        """進捗を報告.

        Args:
            execution_id: 実行ID
            progress: 進捗率（0.0-1.0）
            message: メッセージ
        """
        state = self._executions.get(execution_id)
        if not state:
            return

        state.progress = progress
        state.last_activity = datetime.now()

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.PROGRESS,
            execution_id=execution_id,
            message=message or f"進捗: {progress * 100:.1f}%",
            data={"progress": progress},
        ))

    def report_completed(
        self,
        execution_id: str,
        message: str = "",
    ) -> None:
        """実行完了を報告.

        Args:
            execution_id: 実行ID
            message: メッセージ
        """
        state = self._executions.get(execution_id)
        if state:
            state.is_active = False
            state.progress = 1.0

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.COMPLETED,
            execution_id=execution_id,
            message=message or "実行が完了しました",
            data={
                "completed_steps": state.completed_steps if state else 0,
                "failed_steps": state.failed_steps if state else 0,
            },
        ))

        self.stop_monitoring(execution_id)

    def report_failed(
        self,
        execution_id: str,
        error: str,
    ) -> None:
        """実行失敗を報告.

        Args:
            execution_id: 実行ID
            error: エラーメッセージ
        """
        state = self._executions.get(execution_id)
        if state:
            state.is_active = False

        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.FAILED,
            execution_id=execution_id,
            message=f"実行が失敗しました: {error}",
            severity=AlertSeverity.CRITICAL,
            data={"error": error},
        ))

        self.stop_monitoring(execution_id)

    async def _monitor_loop(self, execution_id: str) -> None:
        """監視ループ.

        定期的に状態をチェックし、異常を検出する。
        """
        while True:
            try:
                await asyncio.sleep(5.0)  # 5秒ごとにチェック

                state = self._executions.get(execution_id)
                if not state or not state.is_active:
                    break

                # スタック検出
                await self._check_stuck(state)

                # ステップタイムアウト検出
                await self._check_step_timeout(state)

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"監視ループエラー: {e}")

    async def _check_stuck(self, state: ExecutionState) -> None:
        """スタック検出.

        一定時間進捗がない場合にアラートを発行。
        """
        elapsed = (datetime.now() - state.last_activity).total_seconds()

        if elapsed > self._thresholds.progress_stall_threshold:
            self._emit_event(MonitorEvent(
                event_type=MonitorEventType.STUCK,
                execution_id=state.execution_id,
                message=f"進捗停滞を検出: {elapsed:.0f}秒間更新なし",
                severity=AlertSeverity.WARNING,
                data={"elapsed_seconds": elapsed},
            ))

            # リカバリ提案
            self._emit_event(MonitorEvent(
                event_type=MonitorEventType.RECOVERY,
                execution_id=state.execution_id,
                message="リカバリ提案: 実行を再開するか、手動介入を検討してください",
                data={
                    "suggestions": [
                        "現在のステップを再実行",
                        "実行をキャンセルして最初から",
                        "手動介入で問題を解決",
                    ],
                },
            ))

    async def _check_step_timeout(self, state: ExecutionState) -> None:
        """ステップタイムアウト検出."""
        if not state.current_step_started:
            return

        elapsed = (datetime.now() - state.current_step_started).total_seconds()

        if elapsed > self._thresholds.step_timeout_warning:
            self._emit_event(MonitorEvent(
                event_type=MonitorEventType.WARNING,
                execution_id=state.execution_id,
                step_id=state.current_step_id,
                message=f"ステップ実行時間が長くなっています: {elapsed:.0f}秒",
                severity=AlertSeverity.WARNING,
                data={"elapsed_seconds": elapsed},
            ))

    def _emit_event(self, event: MonitorEvent) -> None:
        """イベントを発行."""
        try:
            self._event_queue.put_nowait(event)
        except asyncio.QueueFull:
            self._logger.warning("イベントキューが満杯です")

        # アラートコールバック
        if (
            self._alert_callback
            and event.severity in (AlertSeverity.ERROR, AlertSeverity.CRITICAL)
        ):
            try:
                self._alert_callback(event)
            except Exception as e:
                self._logger.error(f"アラートコールバックエラー: {e}")

    def _emit_alert(
        self,
        execution_id: str,
        message: str,
        severity: AlertSeverity,
        data: dict[str, Any] | None = None,
    ) -> None:
        """アラートを発行."""
        self._emit_event(MonitorEvent(
            event_type=MonitorEventType.ALERT,
            execution_id=execution_id,
            message=message,
            severity=severity,
            data=data or {},
        ))

    def get_execution_state(self, execution_id: str) -> ExecutionState | None:
        """実行状態を取得."""
        return self._executions.get(execution_id)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "active_executions": len(self._executions),
            "event_queue_size": self._event_queue.qsize(),
            "thresholds": {
                "step_timeout_warning": self._thresholds.step_timeout_warning,
                "stuck_detection_interval": self._thresholds.stuck_detection_interval,
                "max_consecutive_failures": self._thresholds.max_consecutive_failures,
            },
        }


# エクスポート
__all__ = [
    "MonitorEventType",
    "AlertSeverity",
    "MonitorEvent",
    "MonitorThresholds",
    "ExecutionState",
    "MonitorAgent",
]
