"""実行追跡モジュール.

スキル実行履歴の記録、タイムライン表示、統計情報を提供。

使用例:
    >>> tracker = ExecutionTracker()
    >>> event = ExecutionEvent(
    ...     skill_name="read_file",
    ...     params={"path": "/tmp/test.txt"},
    ...     status=ExecutionStatus.SUCCESS,
    ... )
    >>> await tracker.record(event)
    >>> timeline = await tracker.get_timeline(
    ...     start=datetime.now() - timedelta(hours=24),
    ...     end=datetime.now(),
    ... )
"""

from __future__ import annotations

import logging
import uuid
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


class ExecutionStatus(str, Enum):
    """実行ステータス."""

    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    CANCELLED = "cancelled"
    TIMEOUT = "timeout"


@dataclass
class ExecutionEvent:
    """実行イベント.

    Attributes:
        id: イベントID
        skill_name: スキル名
        params: パラメータ
        started_at: 開始日時
        completed_at: 完了日時
        status: ステータス
        result: 実行結果
        error: エラーメッセージ
        duration_ms: 実行時間（ミリ秒）
        user_id: 実行ユーザーID
        approval_id: 関連する承認ID
        metadata: 追加メタデータ
    """

    skill_name: str
    params: dict[str, Any]
    status: ExecutionStatus = ExecutionStatus.PENDING
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    started_at: datetime = field(default_factory=datetime.now)
    completed_at: datetime | None = None
    result: Any = None
    error: str | None = None
    duration_ms: float = 0.0
    user_id: str = "system"
    approval_id: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "skill_name": self.skill_name,
            "params": self.params,
            "status": self.status.value,
            "started_at": self.started_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "result": self.result,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "user_id": self.user_id,
            "approval_id": self.approval_id,
            "metadata": self.metadata,
        }

    def complete(
        self,
        status: ExecutionStatus,
        result: Any = None,
        error: str | None = None,
    ) -> None:
        """実行完了を記録."""
        self.status = status
        self.result = result
        self.error = error
        self.completed_at = datetime.now()
        self.duration_ms = (self.completed_at - self.started_at).total_seconds() * 1000


@dataclass
class ExecutionStats:
    """実行統計.

    Attributes:
        total_executions: 総実行数
        success_count: 成功数
        failed_count: 失敗数
        cancelled_count: キャンセル数
        avg_duration_ms: 平均実行時間
        by_skill: スキル別統計
        by_hour: 時間別統計
        by_status: ステータス別統計
    """

    total_executions: int = 0
    success_count: int = 0
    failed_count: int = 0
    cancelled_count: int = 0
    timeout_count: int = 0
    avg_duration_ms: float = 0.0
    by_skill: dict[str, int] = field(default_factory=dict)
    by_hour: dict[int, int] = field(default_factory=dict)
    by_status: dict[str, int] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "total_executions": self.total_executions,
            "success_count": self.success_count,
            "failed_count": self.failed_count,
            "cancelled_count": self.cancelled_count,
            "timeout_count": self.timeout_count,
            "success_rate": round(self.success_count / max(self.total_executions, 1) * 100, 2),
            "avg_duration_ms": round(self.avg_duration_ms, 2),
            "by_skill": self.by_skill,
            "by_hour": self.by_hour,
            "by_status": self.by_status,
        }


class ExecutionTracker:
    """実行追跡クラス.

    スキル実行の履歴を記録し、タイムライン表示と統計情報を提供。
    """

    def __init__(
        self,
        max_history_size: int = 10000,
        websocket_hub: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            max_history_size: 履歴の最大保持件数
            websocket_hub: WebSocketHub（リアルタイム通知用）
        """
        self._events: list[ExecutionEvent] = []
        self._running: dict[str, ExecutionEvent] = {}
        self._max_size = max_history_size
        self._hub = websocket_hub
        self._logger = logging.getLogger(__name__)

    async def start_execution(
        self,
        skill_name: str,
        params: dict[str, Any],
        user_id: str = "system",
        approval_id: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> ExecutionEvent:
        """実行開始を記録.

        Args:
            skill_name: スキル名
            params: パラメータ
            user_id: ユーザーID
            approval_id: 承認ID
            metadata: メタデータ

        Returns:
            実行イベント
        """
        event = ExecutionEvent(
            skill_name=skill_name,
            params=params,
            status=ExecutionStatus.RUNNING,
            user_id=user_id,
            approval_id=approval_id,
            metadata=metadata or {},
        )

        self._running[event.id] = event
        self._logger.info(
            "実行開始: id=%s, skill=%s, user=%s",
            event.id,
            skill_name,
            user_id,
        )

        # WebSocket通知
        if self._hub:
            await self._hub.broadcast(
                {
                    "type": "execution_started",
                    "data": event.to_dict(),
                }
            )

        return event

    async def complete_execution(
        self,
        event_id: str,
        status: ExecutionStatus,
        result: Any = None,
        error: str | None = None,
    ) -> ExecutionEvent | None:
        """実行完了を記録.

        Args:
            event_id: イベントID
            status: 最終ステータス
            result: 実行結果
            error: エラーメッセージ

        Returns:
            更新されたイベント
        """
        event = self._running.pop(event_id, None)
        if event is None:
            self._logger.warning("実行イベントが見つかりません: %s", event_id)
            return None

        event.complete(status, result, error)
        await self.record(event)

        self._logger.info(
            "実行完了: id=%s, skill=%s, status=%s, duration_ms=%.2f",
            event.id,
            event.skill_name,
            status.value,
            event.duration_ms,
        )

        return event

    async def record(self, event: ExecutionEvent) -> None:
        """イベントを記録.

        Args:
            event: 実行イベント
        """
        self._events.append(event)

        # サイズ制限
        if len(self._events) > self._max_size:
            self._events = self._events[-self._max_size :]

        # WebSocket通知
        if self._hub:
            await self._hub.broadcast(
                {
                    "type": "execution_completed",
                    "data": event.to_dict(),
                }
            )

    async def get_timeline(
        self,
        start: datetime | None = None,
        end: datetime | None = None,
        skill_filter: str | None = None,
        status_filter: ExecutionStatus | None = None,
        user_filter: str | None = None,
        limit: int = 100,
        offset: int = 0,
    ) -> list[ExecutionEvent]:
        """タイムラインを取得.

        Args:
            start: 開始日時
            end: 終了日時
            skill_filter: スキル名フィルター
            status_filter: ステータスフィルター
            user_filter: ユーザーフィルター
            limit: 取得件数
            offset: オフセット

        Returns:
            イベントリスト（新しい順）
        """
        filtered = self._events.copy()

        # 日時フィルター
        if start:
            filtered = [e for e in filtered if e.started_at >= start]
        if end:
            filtered = [e for e in filtered if e.started_at <= end]

        # スキルフィルター
        if skill_filter:
            filtered = [e for e in filtered if e.skill_name == skill_filter]

        # ステータスフィルター
        if status_filter:
            filtered = [e for e in filtered if e.status == status_filter]

        # ユーザーフィルター
        if user_filter:
            filtered = [e for e in filtered if e.user_id == user_filter]

        # 新しい順にソート
        filtered = sorted(filtered, key=lambda e: e.started_at, reverse=True)

        return filtered[offset : offset + limit]

    async def get_statistics(
        self,
        period: timedelta | None = None,
        start: datetime | None = None,
        end: datetime | None = None,
    ) -> ExecutionStats:
        """統計情報を取得.

        Args:
            period: 期間（startより優先）
            start: 開始日時
            end: 終了日時

        Returns:
            統計情報
        """
        end = end or datetime.now()
        if period:
            start = end - period
        elif start is None:
            start = datetime.min

        # 対象イベントをフィルター
        events = [e for e in self._events if start <= e.started_at <= end]

        if not events:
            return ExecutionStats()

        # 基本統計
        total = len(events)
        success = sum(1 for e in events if e.status == ExecutionStatus.SUCCESS)
        failed = sum(1 for e in events if e.status == ExecutionStatus.FAILED)
        cancelled = sum(1 for e in events if e.status == ExecutionStatus.CANCELLED)
        timeout = sum(1 for e in events if e.status == ExecutionStatus.TIMEOUT)

        # 平均実行時間
        completed = [e for e in events if e.duration_ms > 0]
        avg_duration = sum(e.duration_ms for e in completed) / max(len(completed), 1)

        # スキル別統計
        by_skill: dict[str, int] = defaultdict(int)
        for e in events:
            by_skill[e.skill_name] += 1

        # 時間別統計
        by_hour: dict[int, int] = defaultdict(int)
        for e in events:
            by_hour[e.started_at.hour] += 1

        # ステータス別統計
        by_status: dict[str, int] = defaultdict(int)
        for e in events:
            by_status[e.status.value] += 1

        return ExecutionStats(
            total_executions=total,
            success_count=success,
            failed_count=failed,
            cancelled_count=cancelled,
            timeout_count=timeout,
            avg_duration_ms=avg_duration,
            by_skill=dict(by_skill),
            by_hour=dict(by_hour),
            by_status=dict(by_status),
        )

    def get_running(self) -> list[ExecutionEvent]:
        """実行中のイベント一覧."""
        return list(self._running.values())

    def get_recent(self, count: int = 10) -> list[ExecutionEvent]:
        """最近のイベント."""
        return sorted(self._events, key=lambda e: e.started_at, reverse=True)[:count]

    async def export(
        self,
        start: datetime | None = None,
        end: datetime | None = None,
        format: str = "json",
    ) -> str | list[dict[str, Any]]:
        """履歴をエクスポート.

        Args:
            start: 開始日時
            end: 終了日時
            format: 出力形式（json/csv）

        Returns:
            エクスポートデータ
        """
        events = await self.get_timeline(start=start, end=end, limit=99999)
        data = [e.to_dict() for e in events]

        if format == "json":
            return data
        if format == "csv":
            import csv
            import io

            output = io.StringIO()
            if data:
                writer = csv.DictWriter(output, fieldnames=data[0].keys())
                writer.writeheader()
                for row in data:
                    # ネストされた辞書を文字列化
                    flat_row = {
                        k: str(v) if isinstance(v, (dict, list)) else v for k, v in row.items()
                    }
                    writer.writerow(flat_row)
            return output.getvalue()

        return data
