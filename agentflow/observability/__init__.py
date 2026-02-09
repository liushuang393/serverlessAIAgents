"""AgentFlow 可観測性モジュール.

統一されたログ収集、性能監視、エラー追跡を提供します。

機能:
- 構造化ログ（JSON 形式、OpenTelemetry 互換）
- 性能メトリクス（レイテンシ、スループット、エラー率）
- 分散トレーシング（Trace ID、Span）
- Sentry 統合（エラー追跡）
- Prometheus メトリクス

使用例:
    >>> from agentflow.observability import setup_observability, get_tracer
    >>>
    >>> # 初期化
    >>> setup_observability(
    ...     service_name="my-agent",
    ...     sentry_dsn="https://...",
    ...     enable_metrics=True
    ... )
    >>>
    >>> # トレーシング
    >>> tracer = get_tracer()
    >>> with tracer.span("my-operation"):
    ...     # 処理
    ...     pass
"""

from agentflow.observability.logging import (
    AgentFlowLogger,
    LogLevel,
    get_context,
    get_logger,
    replace_context,
    setup_logging,
)
from agentflow.observability.metrics import (
    Counter,
    Gauge,
    Histogram,
    MetricsCollector,
    get_metrics,
    setup_metrics,
)
from agentflow.observability.otel_exporter import OTLPExporter
from agentflow.observability.sentry_integration import (
    capture_exception,
    capture_message,
    setup_sentry,
)
from agentflow.observability.setup import setup_observability
from agentflow.observability.startup import log_startup_info
from agentflow.observability.tracing import (
    Span,
    Tracer,
    get_tracer,
    setup_tracing,
)


__all__ = [
    # Logging
    "AgentFlowLogger",
    "Counter",
    "Gauge",
    "Histogram",
    "LogLevel",
    # Metrics
    "MetricsCollector",
    # OTLP Exporter
    "OTLPExporter",
    "Span",
    # Tracing
    "Tracer",
    "capture_exception",
    "capture_message",
    "get_context",
    "get_logger",
    "get_metrics",
    "get_tracer",
    "log_startup_info",
    "replace_context",
    "setup_logging",
    "setup_metrics",
    # Setup
    "setup_observability",
    # Sentry
    "setup_sentry",
    "setup_tracing",
]
