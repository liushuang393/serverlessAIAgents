"""Layer 3 の event 公開 API."""

from kernel.events.sinks import EventSink, NoOpEventSink


__all__ = ["EventSink", "NoOpEventSink"]
