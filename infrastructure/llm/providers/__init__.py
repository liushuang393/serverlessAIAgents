"""LLM providers — facade, cost optimizer, model selector, multi-LLM router.

Re-exports from sub-modules for convenience.
"""

from infrastructure.llm.providers.cost_optimizer import CostBudget, CostOptimizer, CostSummary
from infrastructure.llm.providers.llm_provider import (
    LLMProvider,
    LLMProviderConfig,
    get_llm,
    reset_llm,
)
from infrastructure.llm.providers.model_selector import (
    ModelSelectionCriteria,
    ModelSelector,
    TaskType,
)
from infrastructure.llm.providers.multi_llm_router import (
    AgentModelMapping,
    MultiLLMRouter,
    RouterConfig,
)
from infrastructure.llm.providers.tool_executor import (
    BatchResult,
    FallbackStrategy,
    FunctionCall,
    RetryConfig,
    RetryStrategy,
    SemanticFallbackStrategy,
    SimpleFallbackStrategy,
    ToolCall,
    ToolCallStatus,
    ToolExecutor,
    ToolResult,
)
from infrastructure.llm.providers.tool_provider import (
    OperationType,
    RegisteredTool,
    RiskLevel,
    ToolProvider,
    tool,
)

# 後方互換: embedding / vectordb プロバイダの re-export
from infrastructure.providers.embedding_provider import (
    EmbeddingProvider,
    get_embedding,
)
from infrastructure.providers.vectordb_provider import (
    VectorDBProvider,
    get_vectordb,
)


__all__ = [
    "AgentModelMapping",
    "CostBudget",
    "CostOptimizer",
    "CostSummary",
    "LLMProvider",
    "LLMProviderConfig",
    "ModelSelectionCriteria",
    "ModelSelector",
    "MultiLLMRouter",
    "OperationType",
    "RegisteredTool",
    "RetryConfig",
    "RetryStrategy",
    "RiskLevel",
    "RouterConfig",
    "SemanticFallbackStrategy",
    "SimpleFallbackStrategy",
    "TaskType",
    "ToolCall",
    "ToolCallStatus",
    "ToolExecutor",
    "ToolProvider",
    "ToolResult",
    "get_llm",
    "reset_llm",
    "tool",
    # embedding / vectordb
    "EmbeddingProvider",
    "VectorDBProvider",
    "get_embedding",
    "get_vectordb",
]
