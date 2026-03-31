"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_calendar = import_module("kernel.skills.builtin.calendar.calendar")
CalendarEvent = _calendar.CalendarEvent
CalendarSkill = _calendar.CalendarSkill


warnings.warn(
    "shared.skills.builtin.calendar は非推奨です。kernel.skills.builtin.calendar を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["CalendarEvent", "CalendarSkill"]
