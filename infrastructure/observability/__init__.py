"""Layer 1 の observability infrastructure 公開 API.

アダプタパターン(ports/exporters/registry)に加え、
ログ・メトリクス・トレーシング・Sentry統合等の実装を提供。
"""

# --- アダプタパターン (ports / exporters / registry) ---
from infrastructure.observability.exporters import (
    InMemoryTraceExporter,
    LoggingTraceExporter,
    NoOpTraceExporter,
)

# --- ログ ---
from infrastructure.observability.logging import (
    AgentFlowLogger,
    LogLevel,
    get_context,
    get_logger,
    replace_context,
    setup_logging,
)

# --- メトリクス ---
from infrastructure.observability.metrics import (
    Counter,
    Gauge,
    Histogram,
    MetricsCollector,
    get_metrics,
    setup_metrics,
)

# --- OTLP ---
from infrastructure.observability.otel_exporter import OTLPExporter
from infrastructure.observability.ports import TraceExporter
from infrastructure.observability.registry import (
    TraceExporterRegistry,
    get_trace_exporter,
)

# --- Sentry ---
from infrastructure.observability.sentry_integration import (
    capture_exception,
    capture_message,
    setup_sentry,
)

# --- セットアップ ---
from infrastructure.observability.setup import setup_observability
from infrastructure.observability.startup import log_startup_info

# --- トレーシング ---
from infrastructure.observability.tracing import (
    Span,
    Tracer,
    get_tracer,
    setup_tracing,
)


__all__ = [
    # ログ
    "AgentFlowLogger",
    # メトリクス
    "Counter",
    "Gauge",
    "Histogram",
    # アダプタパターン
    "InMemoryTraceExporter",
    "LogLevel",
    "LoggingTraceExporter",
    "MetricsCollector",
    "NoOpTraceExporter",
    # OTLP
    "OTLPExporter",
    # トレーシング
    "Span",
    "TraceExporter",
    "TraceExporterRegistry",
    "Tracer",
    # Sentry
    "capture_exception",
    "capture_message",
    "get_context",
    "get_logger",
    "get_metrics",
    "get_trace_exporter",
    "get_tracer",
    "log_startup_info",
    "replace_context",
    "setup_logging",
    "setup_metrics",
    # セットアップ
    "setup_observability",
    "setup_sentry",
    "setup_tracing",
]
