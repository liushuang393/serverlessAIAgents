"""Layer 1 の LLM infrastructure 公開 API.

アダプタパターン(ports/adapters/registry)に加え、
LLMクライアント・モデルルーター・ゲートウェイ等の実装を提供。
"""

# --- アダプタパターン (ports / adapters / registry) ---
from infrastructure.llm.adapters import (
    AgentFlowLLMBackend,
    MockLLMBackend,
    NoOpLLMBackend,
)

# --- LLM コントラクト ---
from infrastructure.llm.contracts import (
    LLMContractBinding,
    LLMContractModelRef,
    LLMContractResolutionError,
    LLMContractsConfig,
    resolve_contract_model_alias,
    resolve_contract_model_ref,
    resolve_known_model_ids,
    resolve_known_providers,
)

# --- ゲートウェイ ---
from infrastructure.llm.gateway import (
    GatewayResponse,
    GatewayToolCall,
    LiteLLMGateway,
    load_gateway_config,
)

# --- LLM クライアント ---
from infrastructure.llm.llm_client import (
    LLMClient,
    LLMConfig,
    LLMMessage,
    LLMResponse,
    ToolCall,
)

# --- モデル定義 ---
from infrastructure.llm.models import (
    ModelCapability,
    ModelInfo,
    ModelTier,
)
from infrastructure.llm.ports import LLMBackend
from infrastructure.llm.registry import (
    LLMBackendRegistry,
    get_llm_backend,
)

# --- ルーター ---
from infrastructure.llm.router import (
    ModelRouter,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)

# --- モデルルーター (高レベルファサード / 再エクスポート) ---
# model_router.py は models, router, stats の再エクスポートファイル
# --- 統計 ---
from infrastructure.llm.stats import ModelStats


__all__ = [
    # アダプタパターン
    "AgentFlowLLMBackend",
    # ゲートウェイ
    "GatewayResponse",
    "GatewayToolCall",
    "LLMBackend",
    "LLMBackendRegistry",
    # クライアント
    "LLMClient",
    "LLMConfig",
    # コントラクト
    "LLMContractBinding",
    "LLMContractModelRef",
    "LLMContractResolutionError",
    "LLMContractsConfig",
    "LLMMessage",
    "LLMResponse",
    "LiteLLMGateway",
    "MockLLMBackend",
    # モデル
    "ModelCapability",
    "ModelInfo",
    # ルーター
    "ModelRouter",
    # 統計
    "ModelStats",
    "ModelTier",
    "NoOpLLMBackend",
    "RoutingConfig",
    "RoutingStrategy",
    "ToolCall",
    "create_router_from_env",
    "get_llm_backend",
    "load_gateway_config",
    "resolve_contract_model_alias",
    "resolve_contract_model_ref",
    "resolve_known_model_ids",
    "resolve_known_providers",
]
