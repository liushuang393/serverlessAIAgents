"""Layer 1 の LLM infrastructure 公開 API.

アダプタパターン(ports/adapters/registry)に加え、
LLMクライアント・モデルルーター・ゲートウェイ等の実装を提供。
"""

# --- アダプタパターン (ports / adapters / registry) ---
from infrastructure.llm.adapters import (  # noqa: F401
    AgentFlowLLMBackend,
    MockLLMBackend,
    NoOpLLMBackend,
)
from infrastructure.llm.ports import LLMBackend  # noqa: F401
from infrastructure.llm.registry import (  # noqa: F401
    LLMBackendRegistry,
    get_llm_backend,
)

# --- LLM コントラクト ---
from infrastructure.llm.contracts import (  # noqa: F401
    LLMContractBinding,
    LLMContractModelRef,
    LLMContractResolutionError,
    LLMContractsConfig,
    resolve_contract_model_alias,
    resolve_contract_model_ref,
    resolve_known_model_ids,
    resolve_known_providers,
)

# --- LLM クライアント ---
from infrastructure.llm.llm_client import (  # noqa: F401
    LLMClient,
    LLMConfig,
    LLMMessage,
    LLMResponse,
    ToolCall,
)

# --- モデル定義 ---
from infrastructure.llm.models import (  # noqa: F401
    ModelCapability,
    ModelInfo,
    ModelTier,
)

# --- ルーター ---
from infrastructure.llm.router import (  # noqa: F401
    ModelRouter,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)

# --- モデルルーター (高レベルファサード / 再エクスポート) ---
# model_router.py は models, router, stats の再エクスポートファイル

# --- 統計 ---
from infrastructure.llm.stats import ModelStats  # noqa: F401

# --- ゲートウェイ ---
from infrastructure.llm.gateway import (  # noqa: F401
    GatewayResponse,
    GatewayToolCall,
    LiteLLMGateway,
    load_gateway_config,
)


__all__ = [
    # アダプタパターン
    "AgentFlowLLMBackend",
    "LLMBackend",
    "LLMBackendRegistry",
    "MockLLMBackend",
    "NoOpLLMBackend",
    "get_llm_backend",
    # コントラクト
    "LLMContractBinding",
    "LLMContractModelRef",
    "LLMContractResolutionError",
    "LLMContractsConfig",
    "resolve_contract_model_alias",
    "resolve_contract_model_ref",
    "resolve_known_model_ids",
    "resolve_known_providers",
    # クライアント
    "LLMClient",
    "LLMConfig",
    "LLMMessage",
    "LLMResponse",
    "ToolCall",
    # モデル
    "ModelCapability",
    "ModelInfo",
    "ModelTier",
    # ルーター
    "ModelRouter",
    "RoutingConfig",
    "RoutingStrategy",
    "create_router_from_env",

    # 統計
    "ModelStats",
    # ゲートウェイ
    "GatewayResponse",
    "GatewayToolCall",
    "LiteLLMGateway",
    "load_gateway_config",
]
