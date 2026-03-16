"""agentflow.llm.model_router 後方互換スタブ. 実体は infrastructure.llm.model_router."""

from infrastructure.llm.model_router import (  # noqa: F401
    MODELS,
    ModelCapability,
    ModelInfo,
    ModelRouter,
    ModelStats,
    ModelTier,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)

__all__ = [
    "MODELS",
    "ModelCapability",
    "ModelInfo",
    "ModelRouter",
    "ModelStats",
    "ModelTier",
    "RoutingConfig",
    "RoutingStrategy",
    "create_router_from_env",
]
