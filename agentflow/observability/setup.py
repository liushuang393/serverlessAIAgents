"""agentflow.observability.setup 後方互換スタブ. 実体は infrastructure.observability.setup."""

from infrastructure.observability.setup import (  # noqa: F401
    ObservabilityConfig,
    create_fastapi_middleware,
    get_prometheus_endpoint,
    setup_observability,
)

__all__ = [
    "ObservabilityConfig",
    "create_fastapi_middleware",
    "get_prometheus_endpoint",
    "setup_observability",
]
