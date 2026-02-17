"""Core components of AgentFlow framework."""

from agentflow.core.agent_registry import (
    AgentEntry,
    AgentRegistry,
    get_global_agent_registry,
    reset_global_agent_registry,
)

# NEW: 宣言式Agent定義（AgentBlueprint）
from agentflow.core.blueprint import (
    AgentBlueprint,
    AgentBlueprintModel,
    ConstraintsConfig,
    MemoryConfig,
    SafetyConfig,
    SkillConfig,
    ToolConfig,
)
from agentflow.core.blueprint import (
    ValidationResult as BlueprintValidationResult,
)
from agentflow.core.capability_spec import (
    AgentCapabilitySpec,
    CapabilityRequirement,
    LLMRequirements,
)

# 信頼性強化
from agentflow.core.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerOpenError,
    CircuitState,
)

# NEW: 強約束・多重校験・回滚重试（Manus分析に基づく高信頼性設計）
from agentflow.core.constraint_validator import (
    ConstraintType,
    ConstraintValidator,
    DangerousOperationConfig,
    ToolCallConstraint,
    ValidationSeverity,
)
from agentflow.core.constraint_validator import (
    ValidationResult as ConstraintValidationResult,
)
from agentflow.core.dual_verifier import (
    ConsistencyVerifyStrategy,
    DualVerifier,
    SafetyVerifyStrategy,
    SchemaVerifyStrategy,
    VerifyResult,
    VerifyStatus,
    VerifyStrategy,
    VerifyType,
)
from agentflow.core.engine import AgentFlowEngine

# NEW: 統一エラーレスポンス（RFC 7807 互換）
from agentflow.core.error_response import (
    AgentFlowAPIError,
    ErrorCode,
    ErrorResponse,
    ExecutionError,
    NotFoundError,
    RateLimitError,
    create_error_response,
    create_exception_handlers,
    exception_to_response,
)
from agentflow.core.error_response import (
    TimeoutError as APITimeoutError,
)
from agentflow.core.error_response import (
    ValidationError as APIValidationError,
)
from agentflow.core.exceptions import (
    AgentExecutionError,
    AgentFlowError,
    AgentOutputValidationError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)
from agentflow.core.flow_definition import (
    AgentDefinition,
    AgentStatus,
    FlowDefinition,
    FlowDefinitionRegistry,
)
from agentflow.core.hooks import HookType, LifecycleHooks

# ==========================================================================
# Core Interfaces（安定インターフェース - 変更厳禁）
# ==========================================================================
from agentflow.core.interfaces import (
    CodeGenOptions,
    # Enums
    CodeOutputType,
    ConfigField,
    ConfigTemplate,
    DebugEvent,
    DeployConfig,
    DeployEvent,
    DeployResult,
    DeployTarget,
    EdgeDefinition,
    ExecutionEvent,
    FilePreview,
    GeneratedCode,
    # Interfaces
    ICodeGenerator,
    IConfigManager,
    IDeployExecutor,
    IWorkflowRunner,
    NodeDefinition,
    # Data types
    WorkflowDefinition,
)
from agentflow.core.interfaces import (
    ValidationResult as InterfaceValidationResult,
)
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
from agentflow.core.reliability import (
    get_circuit_breaker,
    reliable,
)
from agentflow.core.retry_advisor import (
    RetryAction,
    RetryAdvice,
    RetryAdvisor,
    RetryContext,
)
from agentflow.core.resilient_agent import (
    BaseDecisionAgent,
    InputT,
    OutputT,
    ResilientAgent,
)
from agentflow.core.result_store import (
    FileResultStore,
    FlowResult,
    MemoryResultStore,
    ResultStore,
    ResultStoreManager,
)
from agentflow.core.retry import RetryableAgent, RetryConfig
from agentflow.core.rollback_manager import (
    Checkpoint,
    CheckpointStatus,
    RetryStrategy,
    RollbackManager,
    RollbackResult,
)
from agentflow.core.rollback_manager import (
    RetryConfig as RollbackRetryConfig,
)
from agentflow.core.schemas import SchemaLoader, SchemaValidationError
from agentflow.core.tool_binding import BoundTools, ToolBinder, ToolExecutor
from agentflow.core.type_safe import safe_enum, safe_float, safe_int

# ==========================================================================
# NEW: 統一ツールカタログ（全ツールソース統合）
# ==========================================================================
from agentflow.core.tool_catalog import (
    CatalogEntry,
    CatalogSource,
    ToolCatalogManager,
    get_tool_catalog,
    reset_tool_catalog,
)

# ==========================================================================
# NEW: 統一ツール・Agent レジストリ（Auto-Agent Architecture）
# ==========================================================================
from agentflow.core.tool_definition import ToolDefinition, ToolSource
from agentflow.core.tool_discovery import ToolDiscoveryService
from agentflow.core.tool_registry import (
    ToolRegistry,
    get_global_tool_registry,
    reset_global_tool_registry,
)
from agentflow.core.types import AgentMetadata, ExecutionContext, WorkflowConfig
from agentflow.core.validator import AgentValidator, ValidationResult


__all__ = [
    "A2AConfig",
    "AGUIConfig",
    "APITimeoutError",
    "APIValidationError",
    "AgentBlockManager",
    # ==========================================================================
    # 宣言式Agent定義（AgentBlueprint）
    # ==========================================================================
    "AgentBlueprint",
    "AgentBlueprintModel",
    "AgentCapabilitySpec",
    "AgentDefinition",
    "AgentEntry",
    "AgentExecutionError",
    "AgentFlowAPIError",
    "AgentFlowEngine",
    "AgentFlowError",
    "AgentInfo",
    "AgentLoader",
    "AgentMetadata",
    "AgentMetadataModel",
    "AgentOutputValidationError",
    "AgentRegistry",
    "AgentRetryExhaustedError",
    "AgentStatus",
    "AgentTimeoutError",
    "AgentValidator",
    "BaseDecisionAgent",
    "BlueprintValidationResult",
    "BoundTools",
    "CapabilityRequirement",
    "Checkpoint",
    "CheckpointStatus",
    # ==========================================================================
    # 信頼性強化
    # ==========================================================================
    "CircuitBreaker",
    "CircuitBreakerConfig",
    "CircuitBreakerOpenError",
    "CircuitState",
    "CodeGenOptions",
    # ==========================================================================
    # Core Interfaces（安定インターフェース）
    # ==========================================================================
    "CodeOutputType",
    "ConfigField",
    "ConfigTemplate",
    "ConsistencyVerifyStrategy",
    "ConstraintType",
    "ConstraintValidationResult",
    # ==========================================================================
    # 強約束・多重校験・回滚重试
    # ==========================================================================
    "ConstraintValidator",
    "ConstraintsConfig",
    "DangerousOperationConfig",
    "DebugEvent",
    "DependencySpec",
    "DeployConfig",
    "DeployEvent",
    "DeployResult",
    "DeployTarget",
    "DualVerifier",
    "EdgeDefinition",
    # ==========================================================================
    # NEW: 統一エラーレスポンス（RFC 7807 互換）
    # ==========================================================================
    "ErrorCode",
    "ErrorResponse",
    "ExecutionContext",
    "ExecutionError",
    "ExecutionEvent",
    "FilePreview",
    "FileResultStore",
    "FlowDefinition",
    "FlowDefinitionRegistry",
    "FlowResult",
    "GeneratedCode",
    "HookType",
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
    "IWorkflowRunner",
    "InputField",
    "InputT",
    "InterfaceDefinition",
    "InterfaceValidationResult",
    "LLMRequirements",
    "LifecycleHooks",
    "MCPConfig",
    "MemoryConfig",
    "MemoryResultStore",
    "MetaInfo",
    "NodeDefinition",
    "NotFoundError",
    "OutputField",
    "OutputT",
    "PocketFlowConfig",
    "ProtocolConfig",
    "ProtocolError",
    "ProtocolRegistry",
    "RateLimitError",
    "Registry",
    "ResilientAgent",
    "ResultStore",
    "ResultStoreManager",
    "RetryConfig",
    "RetryAction",
    "RetryAdvice",
    "RetryAdvisor",
    "RetryStrategy",
    "RetryableAgent",
    "RetryContext",
    "RollbackManager",
    "RollbackResult",
    "RollbackRetryConfig",
    "SafetyConfig",
    "SafetyVerifyStrategy",
    "SchemaLoader",
    "SchemaValidationError",
    "SchemaVerifyStrategy",
    "SkillConfig",
    "ToolBinder",
    "ToolCallConstraint",
    "ToolConfig",
    # ==========================================================================
    # 統一ツールカタログ（全ツールソース統合）
    # ==========================================================================
    "CatalogEntry",
    "CatalogSource",
    "ToolCatalogManager",
    # ==========================================================================
    # 統一ツール・Agent レジストリ（Auto-Agent Architecture）
    # ==========================================================================
    "ToolDefinition",
    "ToolDiscoveryService",
    "ToolExecutor",
    "ToolRegistry",
    "ToolSource",
    "safe_enum",
    "safe_float",
    "safe_int",
    "ValidationResult",
    "ValidationSeverity",
    "VerifyResult",
    "VerifyStatus",
    "VerifyStrategy",
    "VerifyType",
    "VisualConfig",
    "WorkflowConfig",
    "WorkflowDefinition",
    "WorkflowError",
    "WorkflowNotFoundError",
    "create_error_response",
    "create_exception_handlers",
    "exception_to_response",
    "get_circuit_breaker",
    "get_global_agent_registry",
    "get_global_tool_registry",
    "get_tool_catalog",
    "reliable",
    "reset_global_agent_registry",
    "reset_global_tool_registry",
    "reset_tool_catalog",
]
