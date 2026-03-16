"""Layer 1 の observability infrastructure 公開 API.

アダプタパターン(ports/exporters/registry)に加え、
ログ・メトリクス・トレーシング・Sentry統合等の実装を提供。
"""

# --- アダプタパターン (ports / exporters / registry) ---
from infrastructure.observability.exporters import (  # noqa: F401
    InMemoryTraceExporter,
    LoggingTraceExporter,
    NoOpTraceExporter,
)
from infrastructure.observability.ports import TraceExporter  # noqa: F401
from infrastructure.observability.registry import (  # noqa: F401
    TraceExporterRegistry,
    get_trace_exporter,
)

# --- ログ ---
from infrastructure.observability.logging import (  # noqa: F401
    AgentFlowLogger,
    LogLevel,
    get_context,
    get_logger,
    replace_context,
    setup_logging,
)

# --- メトリクス ---
from infrastructure.observability.metrics import (  # noqa: F401
    Counter,
    Gauge,
    Histogram,
    MetricsCollector,
    get_metrics,
    setup_metrics,
)

# --- OTLP ---
from infrastructure.observability.otel_exporter import OTLPExporter  # noqa: F401

# --- Sentry ---
from infrastructure.observability.sentry_integration import (  # noqa: F401
    capture_exception,
    capture_message,
    setup_sentry,
)

# --- セットアップ ---
from infrastructure.observability.setup import setup_observability  # noqa: F401
from infrastructure.observability.startup import log_startup_info  # noqa: F401

# --- トレーシング ---
from infrastructure.observability.tracing import (  # noqa: F401
    Span,
    Tracer,
    get_tracer,
    setup_tracing,
)


__all__ = [
    # アダプタパターン
    "InMemoryTraceExporter",
    "LoggingTraceExporter",
    "NoOpTraceExporter",
    "TraceExporter",
    "TraceExporterRegistry",
    "get_trace_exporter",
    # ログ
    "AgentFlowLogger",
    "LogLevel",
    "get_context",
    "get_logger",
    "replace_context",
    "setup_logging",
    # メトリクス
    "Counter",
    "Gauge",
    "Histogram",
    "MetricsCollector",
    "get_metrics",
    "setup_metrics",
    # OTLP
    "OTLPExporter",
    # Sentry
    "capture_exception",
    "capture_message",
    "setup_sentry",
    # セットアップ
    "setup_observability",
    "log_startup_info",
    # トレーシング
    "Span",
    "Tracer",
    "get_tracer",
    "setup_tracing",
]
