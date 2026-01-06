# -*- coding: utf-8 -*-
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
    setup_logging,
    get_logger,
    LogLevel,
)
from agentflow.observability.metrics import (
    MetricsCollector,
    Counter,
    Gauge,
    Histogram,
    get_metrics,
    setup_metrics,
)
from agentflow.observability.tracing import (
    Tracer,
    Span,
    get_tracer,
    setup_tracing,
)
from agentflow.observability.sentry_integration import (
    setup_sentry,
    capture_exception,
    capture_message,
)
from agentflow.observability.setup import setup_observability
from agentflow.observability.startup import log_startup_info
from agentflow.observability.otel_exporter import OTLPExporter

__all__ = [
    # Setup
    "setup_observability",
    "log_startup_info",
    # Logging
    "AgentFlowLogger",
    "setup_logging",
    "get_logger",
    "LogLevel",
    # Metrics
    "MetricsCollector",
    "Counter",
    "Gauge",
    "Histogram",
    "get_metrics",
    "setup_metrics",
    # Tracing
    "Tracer",
    "Span",
    "get_tracer",
    "setup_tracing",
    # Sentry
    "setup_sentry",
    "capture_exception",
    "capture_message",
    # OTLP Exporter
    "OTLPExporter",
]

