"""Core components of AgentFlow framework."""

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.exceptions import (
    AgentExecutionError,
    AgentFlowError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)

# NEW: 統一エラーレスポンス（RFC 7807 互換）
from agentflow.core.error_response import (
    ErrorCode,
    ErrorResponse,
    AgentFlowAPIError,
    ValidationError as APIValidationError,
    NotFoundError,
    TimeoutError as APITimeoutError,
    RateLimitError,
    ExecutionError,
    create_error_response,
    exception_to_response,
    create_exception_handlers,
)
from agentflow.core.resilient_agent import (
    BaseDecisionAgent,
    InputT,
    OutputT,
    ResilientAgent,
)
from agentflow.core.flow_definition import (
    AgentDefinition,
    AgentStatus,
    FlowDefinition,
    FlowDefinitionRegistry,
)
from agentflow.core.hooks import HookType, LifecycleHooks
from agentflow.core.loader import AgentLoader
from agentflow.core.manager import AgentBlockManager, AgentInfo
from agentflow.core.metadata import (
    A2AConfig,
    AGUIConfig,
    DependencySpec,
    InputField,
    InterfaceDefinition,
    MCPConfig,
    MetaInfo,
    OutputField,
    PocketFlowConfig,
    ProtocolConfig,
    VisualConfig,
)
from agentflow.core.metadata import (
    AgentMetadata as AgentMetadataModel,
)
from agentflow.core.registry import ProtocolRegistry, Registry
from agentflow.core.result_store import (
    FileResultStore,
    FlowResult,
    MemoryResultStore,
    ResultStore,
    ResultStoreManager,
)
from agentflow.core.schemas import SchemaLoader, SchemaValidationError
from agentflow.core.types import AgentMetadata, ExecutionContext, WorkflowConfig
from agentflow.core.validator import AgentValidator, ValidationResult

# 信頼性強化
from agentflow.core.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerOpenError,
    CircuitState,
)
from agentflow.core.reliability import (
    get_circuit_breaker,
    reliable,
)
from agentflow.core.retry import RetryableAgent, RetryConfig

# ==========================================================================
# Core Interfaces（安定インターフェース - 変更厳禁）
# ==========================================================================
from agentflow.core.interfaces import (
    # Enums
    CodeOutputType,
    DeployTarget,
    # Data types
    WorkflowDefinition,
    NodeDefinition,
    EdgeDefinition,
    GeneratedCode,
    FilePreview,
    CodeGenOptions,
    DeployConfig,
    DeployEvent,
    DeployResult,
    ConfigField,
    ConfigTemplate,
    ValidationResult as InterfaceValidationResult,
    ExecutionEvent,
    DebugEvent,
    # Interfaces
    ICodeGenerator,
    IDeployExecutor,
    IConfigManager,
    IWorkflowRunner,
)


__all__ = [
    "A2AConfig",
    "AGUIConfig",
    "AgentBlockManager",
    "AgentDefinition",
    "AgentExecutionError",
    "AgentFlowEngine",
    "AgentFlowError",
    "AgentInfo",
    "AgentLoader",
    "AgentMetadata",
    "AgentMetadataModel",
    "AgentRetryExhaustedError",
    "AgentStatus",
    "AgentTimeoutError",
    "AgentValidator",
    "BaseDecisionAgent",
    "DependencySpec",
    "ExecutionContext",
    "FileResultStore",
    "FlowDefinition",
    "FlowDefinitionRegistry",
    "FlowResult",
    "HookType",
    "InputField",
    "InputT",
    "InterfaceDefinition",
    "LifecycleHooks",
    "MCPConfig",
    "MemoryResultStore",
    "MetaInfo",
    "OutputField",
    "OutputT",
    "PocketFlowConfig",
    "ProtocolConfig",
    "ProtocolError",
    "ProtocolRegistry",
    "Registry",
    "ResilientAgent",
    "ResultStore",
    "ResultStoreManager",
    "SchemaLoader",
    "SchemaValidationError",
    "ValidationResult",
    "VisualConfig",
    "WorkflowConfig",
    "WorkflowError",
    "WorkflowNotFoundError",
    # ==========================================================================
    # NEW: 統一エラーレスポンス（RFC 7807 互換）
    # ==========================================================================
    "ErrorCode",
    "ErrorResponse",
    "AgentFlowAPIError",
    "APIValidationError",
    "NotFoundError",
    "APITimeoutError",
    "RateLimitError",
    "ExecutionError",
    "create_error_response",
    "exception_to_response",
    "create_exception_handlers",
    # ==========================================================================
    # 信頼性強化
    # ==========================================================================
    "CircuitBreaker",
    "CircuitBreakerConfig",
    "CircuitBreakerOpenError",
    "CircuitState",
    "RetryableAgent",
    "RetryConfig",
    "get_circuit_breaker",
    "reliable",
    # ==========================================================================
    # Core Interfaces（安定インターフェース）
    # ==========================================================================
    "CodeOutputType",
    "DeployTarget",
    "WorkflowDefinition",
    "NodeDefinition",
    "EdgeDefinition",
    "GeneratedCode",
    "FilePreview",
    "CodeGenOptions",
    "DeployConfig",
    "DeployEvent",
    "DeployResult",
    "ConfigField",
    "ConfigTemplate",
    "InterfaceValidationResult",
    "ExecutionEvent",
    "DebugEvent",
    "ICodeGenerator",
    "IDeployExecutor",
    "IConfigManager",
    "IWorkflowRunner",
]
