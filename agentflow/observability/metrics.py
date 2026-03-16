"""agentflow.observability.metrics 後方互換スタブ. 実体は infrastructure.observability.metrics."""

from infrastructure.observability.metrics import (  # noqa: F401
    Counter,
    Gauge,
    Histogram,
    MetricsCollector,
    get_metrics,
    setup_metrics,
)

__all__ = ["Counter", "Gauge", "Histogram", "MetricsCollector", "get_metrics", "setup_metrics"]
