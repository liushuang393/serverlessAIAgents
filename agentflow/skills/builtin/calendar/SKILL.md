---
name: calendar
description: カレンダースキル。イベント管理、空き時間検索、会議スケジューリング。
version: 1.0.0
triggers:
  - calendar
  - meeting
  - schedule
  - 予定
  - 会議
  - スケジュール
tags:
  - calendar
  - schedule
  - meeting
  - productivity
examples:
  - "今週の予定を教えて"
  - "会議を予約して"
  - "空いている時間を探して"
allowed-tools:
  - Bash
  - Read
  - Write
user-invocable: true
---

# Calendar Skill

カレンダーイベントの管理、空き時間検索を提供するスキル。

## 機能

- **イベント管理**: 作成・更新・削除・一覧取得
- **空き時間検索**: 指定期間内の空きスロット検索
- **繰り返し対応**: DAILY/WEEKLY/MONTHLY/YEARLY

## 使用例

```python
from agentflow.skills.builtin.calendar import CalendarSkill

skill = CalendarSkill()
events = await skill.list_events(
    start=datetime.now(),
    end=datetime.now() + timedelta(days=7)
)
free_slots = await skill.find_free_slots(duration_minutes=60)
```

## 外部連携

実際のカレンダー連携には以下のサービスを想定:
- Google Calendar
- Microsoft Outlook
- Apple Calendar

