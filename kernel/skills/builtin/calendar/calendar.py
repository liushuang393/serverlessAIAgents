"""カレンダースキル.

カレンダーイベントの管理、空き時間検索を提供。
実際のカレンダー連携は外部サービス（Google Calendar、Outlook等）を想定。

使用例:
    >>> skill = CalendarSkill()
    >>> events = await skill.list_events(
    ...     start=datetime.now(),
    ...     end=datetime.now() + timedelta(days=7),
    ... )
    >>> free_slots = await skill.find_free_slots(duration_minutes=60)
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


class EventStatus(str, Enum):
    """イベントステータス."""

    CONFIRMED = "confirmed"
    TENTATIVE = "tentative"
    CANCELLED = "cancelled"


class RecurrenceType(str, Enum):
    """繰り返しタイプ."""

    NONE = "none"
    DAILY = "daily"
    WEEKLY = "weekly"
    MONTHLY = "monthly"
    YEARLY = "yearly"


@dataclass
class CalendarEvent:
    """カレンダーイベント.

    Attributes:
        id: イベントID
        title: タイトル
        start: 開始日時
        end: 終了日時
        description: 説明
        location: 場所
        attendees: 参加者リスト
        status: ステータス
        recurrence: 繰り返しタイプ
        reminder_minutes: リマインダー（分前）
        metadata: メタデータ
    """

    id: str
    title: str
    start: datetime
    end: datetime
    description: str = ""
    location: str = ""
    attendees: list[str] = field(default_factory=list)
    status: EventStatus = EventStatus.CONFIRMED
    recurrence: RecurrenceType = RecurrenceType.NONE
    reminder_minutes: int = 15
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "title": self.title,
            "start": self.start.isoformat(),
            "end": self.end.isoformat(),
            "description": self.description,
            "location": self.location,
            "attendees": self.attendees,
            "status": self.status.value,
            "recurrence": self.recurrence.value,
            "reminder_minutes": self.reminder_minutes,
            "duration_minutes": int((self.end - self.start).total_seconds() / 60),
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> CalendarEvent:
        """辞書から生成."""
        return cls(
            id=data.get("id", str(uuid.uuid4())),
            title=data["title"],
            start=datetime.fromisoformat(data["start"]) if isinstance(data["start"], str) else data["start"],
            end=datetime.fromisoformat(data["end"]) if isinstance(data["end"], str) else data["end"],
            description=data.get("description", ""),
            location=data.get("location", ""),
            attendees=data.get("attendees", []),
            status=EventStatus(data.get("status", "confirmed")),
            recurrence=RecurrenceType(data.get("recurrence", "none")),
            reminder_minutes=data.get("reminder_minutes", 15),
            metadata=data.get("metadata", {}),
        )


@dataclass
class TimeSlot:
    """時間枠.

    Attributes:
        start: 開始日時
        end: 終了日時
        duration_minutes: 長さ（分）
    """

    start: datetime
    end: datetime
    duration_minutes: int

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "start": self.start.isoformat(),
            "end": self.end.isoformat(),
            "duration_minutes": self.duration_minutes,
        }


class CalendarSkill:
    """カレンダースキル.

    カレンダーイベントの管理機能を提供。
    実際のカレンダー連携はアダプター経由で行う。
    """

    def __init__(
        self,
        adapter: Any = None,
        working_hours: tuple[int, int] = (9, 18),
        working_days: list[int] | None = None,
    ) -> None:
        """初期化.

        Args:
            adapter: カレンダーアダプター（Google Calendar, Outlook等）
            working_hours: 勤務時間（開始時, 終了時）
            working_days: 勤務日（0=月曜...6=日曜）
        """
        self._adapter = adapter
        self._working_hours = working_hours
        self._working_days = working_days or [0, 1, 2, 3, 4]  # 月-金
        self._events: dict[str, CalendarEvent] = {}
        self._logger = logging.getLogger(__name__)

    async def list_events(
        self,
        start: datetime,
        end: datetime,
        calendar_id: str | None = None,
    ) -> list[CalendarEvent]:
        """イベント一覧を取得.

        Args:
            start: 開始日時
            end: 終了日時
            calendar_id: カレンダーID

        Returns:
            イベントリスト
        """
        # アダプター経由で取得
        if self._adapter and hasattr(self._adapter, "list_events"):
            events = await self._adapter.list_events(start, end, calendar_id)
            return [CalendarEvent.from_dict(e) if isinstance(e, dict) else e for e in events]

        # ローカルストレージから取得
        events = [e for e in self._events.values() if e.start >= start and e.end <= end]
        return sorted(events, key=lambda e: e.start)

    async def create_event(
        self,
        title: str,
        start: datetime,
        end: datetime,
        description: str = "",
        location: str = "",
        attendees: list[str] | None = None,
        reminder_minutes: int = 15,
        calendar_id: str | None = None,
    ) -> CalendarEvent:
        """イベントを作成.

        Args:
            title: タイトル
            start: 開始日時
            end: 終了日時
            description: 説明
            location: 場所
            attendees: 参加者リスト
            reminder_minutes: リマインダー（分前）
            calendar_id: カレンダーID

        Returns:
            作成されたイベント
        """
        event = CalendarEvent(
            id=str(uuid.uuid4()),
            title=title,
            start=start,
            end=end,
            description=description,
            location=location,
            attendees=attendees or [],
            reminder_minutes=reminder_minutes,
        )

        # アダプター経由で作成
        if self._adapter and hasattr(self._adapter, "create_event"):
            created = await self._adapter.create_event(event.to_dict(), calendar_id)
            if isinstance(created, dict):
                event = CalendarEvent.from_dict(created)

        self._events[event.id] = event
        self._logger.info("イベント作成: id=%s, title=%s", event.id, title)

        return event

    async def update_event(
        self,
        event_id: str,
        updates: dict[str, Any],
        calendar_id: str | None = None,
    ) -> CalendarEvent | None:
        """イベントを更新.

        Args:
            event_id: イベントID
            updates: 更新内容
            calendar_id: カレンダーID

        Returns:
            更新されたイベント
        """
        event = self._events.get(event_id)
        if event is None:
            return None

        # 更新を適用
        if "title" in updates:
            event.title = updates["title"]
        if "start" in updates:
            event.start = (
                updates["start"] if isinstance(updates["start"], datetime) else datetime.fromisoformat(updates["start"])
            )
        if "end" in updates:
            event.end = (
                updates["end"] if isinstance(updates["end"], datetime) else datetime.fromisoformat(updates["end"])
            )
        if "description" in updates:
            event.description = updates["description"]
        if "location" in updates:
            event.location = updates["location"]
        if "attendees" in updates:
            event.attendees = updates["attendees"]
        if "status" in updates:
            event.status = EventStatus(updates["status"])

        # アダプター経由で更新
        if self._adapter and hasattr(self._adapter, "update_event"):
            await self._adapter.update_event(event_id, event.to_dict(), calendar_id)

        self._logger.info("イベント更新: id=%s", event_id)

        return event

    async def delete_event(
        self,
        event_id: str,
        calendar_id: str | None = None,
    ) -> bool:
        """イベントを削除.

        Args:
            event_id: イベントID
            calendar_id: カレンダーID

        Returns:
            削除成功したか
        """
        if event_id not in self._events:
            return False

        # アダプター経由で削除
        if self._adapter and hasattr(self._adapter, "delete_event"):
            await self._adapter.delete_event(event_id, calendar_id)

        del self._events[event_id]
        self._logger.info("イベント削除: id=%s", event_id)

        return True

    async def find_free_slots(
        self,
        duration_minutes: int,
        start: datetime | None = None,
        end: datetime | None = None,
        attendees: list[str] | None = None,
        max_slots: int = 5,
    ) -> list[TimeSlot]:
        """空き時間を検索.

        Args:
            duration_minutes: 必要な時間（分）
            start: 検索開始日時
            end: 検索終了日時
            attendees: 参加者リスト（全員の空き時間を検索）
            max_slots: 最大スロット数

        Returns:
            空き時間リスト
        """
        start = start or datetime.now()
        end = end or start + timedelta(days=7)

        # イベント取得
        events = await self.list_events(start, end)

        # アダプター経由で参加者の空き時間を取得
        if self._adapter and hasattr(self._adapter, "find_free_busy") and attendees:
            busy_times = await self._adapter.find_free_busy(attendees, start, end)
            # busy_times をイベントリストに追加
            for busy in busy_times:
                events.append(
                    CalendarEvent(
                        id="busy",
                        title="Busy",
                        start=busy["start"]
                        if isinstance(busy["start"], datetime)
                        else datetime.fromisoformat(busy["start"]),
                        end=busy["end"] if isinstance(busy["end"], datetime) else datetime.fromisoformat(busy["end"]),
                    )
                )

        # 営業時間内の空きスロットを検索
        free_slots: list[TimeSlot] = []
        current = start

        while current < end and len(free_slots) < max_slots:
            # 営業日チェック
            if current.weekday() not in self._working_days:
                current = self._next_working_day(current)
                continue

            # 営業時間内にリセット
            work_start = current.replace(hour=self._working_hours[0], minute=0, second=0, microsecond=0)
            work_end = current.replace(hour=self._working_hours[1], minute=0, second=0, microsecond=0)

            if current < work_start:
                current = work_start
            elif current >= work_end:
                current = self._next_working_day(current)
                continue

            # この時間帯が空いているか確認
            slot_end = current + timedelta(minutes=duration_minutes)

            if slot_end > work_end:
                current = self._next_working_day(current)
                continue

            # イベントと衝突確認
            is_free = True
            for event in events:
                if event.status == EventStatus.CANCELLED:
                    continue
                if current < event.end and slot_end > event.start:
                    # 衝突あり、次の空き時間へ
                    current = event.end
                    is_free = False
                    break

            if is_free:
                free_slots.append(
                    TimeSlot(
                        start=current,
                        end=slot_end,
                        duration_minutes=duration_minutes,
                    )
                )
                current = slot_end
            else:
                # 衝突後の時間から再検索
                pass

        self._logger.info("空き時間検索: %d slots found", len(free_slots))

        return free_slots

    def _next_working_day(self, current: datetime) -> datetime:
        """次の営業日の開始時刻を取得."""
        next_day = current + timedelta(days=1)
        next_day = next_day.replace(hour=self._working_hours[0], minute=0, second=0, microsecond=0)

        while next_day.weekday() not in self._working_days:
            next_day += timedelta(days=1)

        return next_day

    async def get_upcoming_events(
        self,
        hours: int = 24,
        calendar_id: str | None = None,
    ) -> list[CalendarEvent]:
        """今後のイベントを取得.

        Args:
            hours: 何時間以内のイベント
            calendar_id: カレンダーID

        Returns:
            イベントリスト
        """
        now = datetime.now()
        end = now + timedelta(hours=hours)
        return await self.list_events(now, end, calendar_id)

    async def get_today_events(
        self,
        calendar_id: str | None = None,
    ) -> list[CalendarEvent]:
        """今日のイベントを取得.

        Args:
            calendar_id: カレンダーID

        Returns:
            イベントリスト
        """
        now = datetime.now()
        start = now.replace(hour=0, minute=0, second=0, microsecond=0)
        end = start + timedelta(days=1)
        return await self.list_events(start, end, calendar_id)
