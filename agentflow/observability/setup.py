"""可観測性セットアップモジュール.

ログ、メトリクス、トレーシング、エラー追跡を一括設定します。

使用例:
    >>> from agentflow.observability import setup_observability
    >>>
    >>> setup_observability(
    ...     service_name="my-agent",
    ...     log_level="INFO",
    ...     sentry_dsn="https://...",
    ...     enable_metrics=True,
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from agentflow.observability.logging import LogLevel, setup_logging
from agentflow.observability.metrics import setup_metrics
from agentflow.observability.sentry_integration import setup_sentry
from agentflow.observability.tracing import setup_tracing


if TYPE_CHECKING:
    from starlette.requests import Request
    from starlette.responses import Response


logger = logging.getLogger(__name__)


@dataclass
class ObservabilityConfig:
    """可観測性設定.

    Attributes:
        service_name: サービス名
        log_level: ログレベル
        log_format: ログ形式（json / text）
        enable_metrics: メトリクス有効化
        enable_tracing: トレーシング有効化
        sentry_dsn: Sentry DSN
        sentry_environment: Sentry 環境
        traces_sample_rate: トレースサンプリング率
    """

    service_name: str = "agentflow"
    log_level: LogLevel = LogLevel.INFO
    log_format: str = "json"
    enable_metrics: bool = True
    enable_tracing: bool = True
    sentry_dsn: str | None = None
    sentry_environment: str | None = None
    traces_sample_rate: float = 0.1
    extra: dict[str, Any] = field(default_factory=dict)


def setup_observability(
    service_name: str = "agentflow",
    log_level: str | LogLevel = "INFO",
    log_format: str = "json",
    enable_metrics: bool = True,
    enable_tracing: bool = True,
    sentry_dsn: str | None = None,
    sentry_environment: str | None = None,
    traces_sample_rate: float = 0.1,
    **kwargs: Any,
) -> dict[str, Any]:
    """可観測性を一括設定.

    Args:
        service_name: サービス名
        log_level: ログレベル
        log_format: ログ形式（json / text）
        enable_metrics: メトリクス有効化
        enable_tracing: トレーシング有効化
        sentry_dsn: Sentry DSN
        sentry_environment: Sentry 環境
        traces_sample_rate: トレースサンプリング率
        **kwargs: 追加設定

    Returns:
        設定結果
    """
    result: dict[str, Any] = {
        "service_name": service_name,
        "logging": False,
        "metrics": False,
        "tracing": False,
        "sentry": False,
    }

    # ログレベルを変換
    if isinstance(log_level, str):
        log_level = LogLevel(log_level.upper())

    # 1. ログ設定
    try:
        setup_logging(level=log_level, format=log_format)
        result["logging"] = True
        logger.info(f"Logging configured: level={log_level}, format={log_format}")
    except Exception as e:
        logger.warning(f"Failed to setup logging: {e}")

    # 2. メトリクス設定
    if enable_metrics:
        try:
            metrics = setup_metrics(prefix=service_name.replace("-", "_"))
            result["metrics"] = True
            result["metrics_collector"] = metrics
            logger.info("Metrics collector configured")
        except Exception as e:
            logger.warning(f"Failed to setup metrics: {e}")

    # 3. トレーシング設定
    if enable_tracing:
        try:
            tracer = setup_tracing(service_name=service_name)
            result["tracing"] = True
            result["tracer"] = tracer
            logger.info("Tracing configured")
        except Exception as e:
            logger.warning(f"Failed to setup tracing: {e}")

    # 4. Sentry 設定
    if sentry_dsn:
        try:
            sentry_ok = setup_sentry(
                dsn=sentry_dsn,
                environment=sentry_environment,
                traces_sample_rate=traces_sample_rate,
            )
            result["sentry"] = sentry_ok
            if sentry_ok:
                logger.info("Sentry configured")
        except Exception as e:
            logger.warning(f"Failed to setup Sentry: {e}")

    logger.info(f"Observability setup complete for {service_name}")
    return result


def create_fastapi_middleware() -> Any:
    """FastAPI 用のミドルウェアを作成.

    Returns:
        ミドルウェアクラス
    """
    try:
        from starlette.middleware.base import BaseHTTPMiddleware
    except ImportError:
        logger.warning("Starlette not installed. FastAPI middleware not available.")
        return None

    from agentflow.observability.metrics import get_metrics
    from agentflow.observability.tracing import get_tracer

    class ObservabilityMiddleware(BaseHTTPMiddleware):
        """可観測性ミドルウェア."""

        async def dispatch(self, request: Request, call_next: Any) -> Response:
            """リクエストを処理."""
            tracer = get_tracer()
            metrics = get_metrics()

            # メトリクス
            request_counter = metrics.counter(
                "http_requests_total",
                "Total HTTP requests",
                label_names=["method", "path", "status"],
            )
            request_duration = metrics.histogram(
                "http_request_duration_seconds",
                "HTTP request duration",
                label_names=["method", "path"],
            )

            # トレーシング
            span_name = f"{request.method} {request.url.path}"
            with tracer.span(span_name) as span:
                span.set_attribute("http.method", request.method)
                span.set_attribute("http.url", str(request.url))

                with request_duration.time(
                    labels={"method": request.method, "path": request.url.path}
                ):
                    response = await call_next(request)

                span.set_attribute("http.status_code", response.status_code)
                request_counter.inc(
                    labels={
                        "method": request.method,
                        "path": request.url.path,
                        "status": str(response.status_code),
                    }
                )

                return response

    return ObservabilityMiddleware


def get_prometheus_endpoint() -> Any:
    """Prometheus エンドポイントを作成.

    Returns:
        FastAPI ルーター
    """
    try:
        from fastapi import APIRouter
        from fastapi.responses import PlainTextResponse
    except ImportError:
        logger.warning("FastAPI not installed. Prometheus endpoint not available.")
        return None

    from agentflow.observability.metrics import get_metrics

    router = APIRouter(tags=["metrics"])

    @router.get("/metrics", response_class=PlainTextResponse)
    async def prometheus_metrics() -> str:
        """Prometheus 形式でメトリクスを出力."""
        metrics = get_metrics()
        return metrics.to_prometheus()

    return router

