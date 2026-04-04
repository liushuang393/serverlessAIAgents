"""定期実行パイプライン — AutonomousPipeline のスケジュール実行.

長時間タスク（機票価格監視等）を定期的に再実行するための
サブスクリプション管理とチェックロジックを提供する。
"""

from __future__ import annotations

import logging
import uuid
from collections.abc import Awaitable, Callable
from datetime import UTC, datetime, timedelta
from enum import StrEnum
from typing import Any

from pydantic import BaseModel, Field

from harness.orchestration.models import ExecutionPlan


_logger = logging.getLogger(__name__)


class ScheduleStatus(StrEnum):
    """スケジュール状態."""

    ACTIVE = "active"
    PAUSED = "paused"
    CANCELLED = "cancelled"
    COMPLETED = "completed"


class ScheduleEntry(BaseModel):
    """定期実行エントリ.

    ExecutionPlan と実行間隔を紐付ける。
    """

    schedule_id: str = Field(
        default_factory=lambda: f"sched-{uuid.uuid4().hex[:8]}",
        description="スケジュールID",
    )
    plan: ExecutionPlan = Field(..., description="実行計画")
    interval_hours: int = Field(..., ge=1, le=168, description="実行間隔（時間）")
    status: ScheduleStatus = Field(default=ScheduleStatus.ACTIVE, description="状態")
    until: datetime | None = Field(default=None, description="終了日時（None=無期限）")
    next_run_at: datetime = Field(..., description="次回実行日時")
    last_run_at: datetime | None = Field(default=None, description="最終実行日時")
    run_count: int = Field(default=0, ge=0, description="累計実行回数")
    last_result: dict[str, Any] | None = Field(default=None, description="最終実行結果")
    context: dict[str, Any] = Field(default_factory=dict, description="実行コンテキスト")
    created_at: datetime = Field(
        default_factory=lambda: datetime.now(UTC),
        description="作成日時",
    )


class ScheduledPipelineResult(BaseModel):
    """定期チェック結果."""

    checked: int = Field(default=0, description="チェックしたスケジュール数")
    executed: int = Field(default=0, description="実行したスケジュール数")
    errors: list[dict[str, Any]] = Field(default_factory=list, description="エラー情報")


class ScheduledPipeline:
    """定期実行パイプライン.

    ExecutionPlan を指定間隔で繰り返し実行する。
    App 側の定期監視ループ（FlightWatchService.check_due_subscriptions 等）を
    フレームワーク機能として汎用化する。

    ストレージはインメモリ。永続化が必要な場合は外部ストアを注入する。

    Example:
        >>> scheduler = ScheduledPipeline()
        >>> entry = scheduler.schedule(plan, interval_hours=6)
        >>> # 定期的に呼び出す
        >>> result = await scheduler.check_due(execute_fn)
    """

    def __init__(self) -> None:
        """初期化."""
        self._entries: dict[str, ScheduleEntry] = {}

    def schedule(
        self,
        plan: ExecutionPlan,
        *,
        interval_hours: int = 6,
        until: datetime | None = None,
        context: dict[str, Any] | None = None,
    ) -> ScheduleEntry:
        """定期実行スケジュールを登録.

        Args:
            plan: 実行計画
            interval_hours: 実行間隔（時間）
            until: 終了日時（None=無期限）
            context: 実行コンテキスト

        Returns:
            作成された ScheduleEntry
        """
        now = datetime.now(UTC)
        entry = ScheduleEntry(
            plan=plan,
            interval_hours=interval_hours,
            until=until,
            next_run_at=now + timedelta(hours=interval_hours),
            context=context or {},
        )
        self._entries[entry.schedule_id] = entry

        _logger.info(
            "スケジュール登録: id=%s, plan=%s, interval=%dh, next=%s",
            entry.schedule_id,
            plan.plan_id,
            interval_hours,
            entry.next_run_at.isoformat(),
        )

        return entry

    # 実行関数の型: (ExecutionPlan, dict[str, Any]) -> Awaitable[dict[str, Any]]
    ExecuteFn = Callable[[ExecutionPlan, dict[str, Any]], Awaitable[dict[str, Any]]]

    async def check_due(
        self,
        execute_fn: ExecuteFn,
    ) -> ScheduledPipelineResult:
        """期限到来したスケジュールを実行.

        Args:
            execute_fn: 実行関数 (plan: ExecutionPlan, context: dict) -> dict

        Returns:
            チェック結果
        """
        now = datetime.now(UTC)
        result = ScheduledPipelineResult()

        due_entries = [
            entry
            for entry in self._entries.values()
            if entry.status == ScheduleStatus.ACTIVE and entry.next_run_at <= now
        ]

        result.checked = len(due_entries)

        for entry in due_entries:
            # 有効期限チェック
            if entry.until is not None and now > entry.until:
                entry.status = ScheduleStatus.COMPLETED
                _logger.info("スケジュール完了（期限切れ）: id=%s", entry.schedule_id)
                continue

            try:
                exec_result = await execute_fn(entry.plan, entry.context)
                entry.last_result = exec_result if isinstance(exec_result, dict) else {"result": str(exec_result)}
                entry.run_count += 1
                result.executed += 1

                _logger.info(
                    "スケジュール実行成功: id=%s, run_count=%d",
                    entry.schedule_id,
                    entry.run_count,
                )
            except Exception as exc:
                result.errors.append(
                    {
                        "schedule_id": entry.schedule_id,
                        "error": str(exc),
                    }
                )
                _logger.warning(
                    "スケジュール実行エラー: id=%s, error=%s",
                    entry.schedule_id,
                    exc,
                )

            # 次回実行時刻を更新
            entry.last_run_at = now
            entry.next_run_at = now + timedelta(hours=entry.interval_hours)

        return result

    def cancel(self, schedule_id: str) -> bool:
        """スケジュールをキャンセル.

        Args:
            schedule_id: スケジュールID

        Returns:
            キャンセル成功したか
        """
        entry = self._entries.get(schedule_id)
        if entry is None:
            return False
        entry.status = ScheduleStatus.CANCELLED
        _logger.info("スケジュールキャンセル: id=%s", schedule_id)
        return True

    def pause(self, schedule_id: str) -> bool:
        """スケジュールを一時停止."""
        entry = self._entries.get(schedule_id)
        if entry is None or entry.status != ScheduleStatus.ACTIVE:
            return False
        entry.status = ScheduleStatus.PAUSED
        return True

    def resume(self, schedule_id: str) -> bool:
        """スケジュールを再開."""
        entry = self._entries.get(schedule_id)
        if entry is None or entry.status != ScheduleStatus.PAUSED:
            return False
        entry.status = ScheduleStatus.ACTIVE
        entry.next_run_at = datetime.now(UTC) + timedelta(hours=entry.interval_hours)
        return True

    def get(self, schedule_id: str) -> ScheduleEntry | None:
        """スケジュールを取得."""
        return self._entries.get(schedule_id)

    def list_active(self) -> list[ScheduleEntry]:
        """アクティブなスケジュール一覧."""
        return [entry for entry in self._entries.values() if entry.status == ScheduleStatus.ACTIVE]


__all__ = [
    "ScheduleEntry",
    "ScheduleStatus",
    "ScheduledPipeline",
    "ScheduledPipelineResult",
]
