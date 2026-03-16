"""agentflow.llm.router 後方互換スタブ. 実体は infrastructure.llm.router."""

from infrastructure.llm.router import (  # noqa: F401
    ModelRouter,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)

__all__ = ["ModelRouter", "RoutingConfig", "RoutingStrategy", "create_router_from_env"]
