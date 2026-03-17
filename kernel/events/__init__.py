"""Layer 3 Events - イベント契約とシンク."""

from kernel.events.sinks import EventSink, NoOpEventSink
from kernel.events.types import EventType


__all__ = ["EventSink", "EventType", "NoOpEventSink"]
