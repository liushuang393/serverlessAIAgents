"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow はAIエージェント開発のための軽量フレームワークです。
MCP、A2A、AG-UI プロトコルをネイティブサポートしています。

=============================================================================
公開API（推奨）
=============================================================================

Engine Pattern（メインAPI - 4種類の予定義パターン）:
    >>> from agentflow import SimpleEngine, PipelineEngine, GateEngine, RAGEngine
    >>>
    >>> # 1. SimpleEngine - 単一Agent質問応答
    >>> engine = SimpleEngine(agent=MyAgent)
    >>> result = await engine.run({"question": "こんにちは"})
    >>>
    >>> # 2. PipelineEngine - マルチステップフロー
    >>> engine = PipelineEngine(
    ...     stages=[
    ...         {"name": "gate", "agent": GateAgent, "gate": True},
    ...         {"name": "analysis", "agents": [Agent1, Agent2]},
    ...         {"name": "review", "agent": ReviewAgent, "review": True},
    ...     ],
    ...     max_revisions=2,
    ... )
    >>> async for event in engine.run_stream(inputs):
    ...     print(event)

Decorator API（最も簡単）:
    >>> from agentflow import agent, tool, AgentClient
    >>> @agent
    ... class MyAgent:
    ...     system_prompt = "親切なアシスタント"
    ...     @tool
    ...     def search(self, query: str) -> list:
    ...         return []
    >>> result = await AgentClient.get("MyAgent").invoke({"question": "..."})

松耦合Provider（推奨）:
    >>> from agentflow import get_llm, get_db, get_vectordb, get_embedding
    >>> llm = get_llm()  # 環境変数から自動検出
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])

=============================================================================
内部API（上級者向け - 直接使用は非推奨）
=============================================================================

- agentflow.flow: 低レベルフロー構築API（create_flow等）
- agentflow.patterns: デザインパターン実装
- agentflow.core: 基底クラスとユーティリティ
- agentflow.integrations: フレームワーク統合

環境変数:
    フレームワーク初期化時に .env ファイルを自動読み込み。
"""

# =============================================================================
# 環境変数の読み込みは明示的に行う（init_agentflow を使用）
# =============================================================================

# =============================================================================
# 公開API: Engine Pattern（メインAPI）
# =============================================================================
# =============================================================================
# 公開API: Decorator API
# =============================================================================
from agentflow.agent_decorator import AgentClient, agent, get_skill, list_skills

# =============================================================================
# 公開API: 統一 API 層（NEW - 前後台交互）
# =============================================================================
from agentflow.api import (
    APIError,
    # レスポンス
    APIResponse,
    ErrorCode,
    PagedResponse,
    # Rich Builder
    RichResponseBuilder,
    RouterConfig,
    # SSE
    SSEEmitter,
    SSEEvent,
    StreamEvent,
    StreamEventType,
    # WebSocket
    WebSocketHub,
    WSMessage,
    WSMessageType,
    # Router Factory
    create_agent_router,
    create_websocket_router,
)

# =============================================================================
# 公開API: Channels - 多平台メッセージ統合（v1.1.0 NEW）
# =============================================================================
from agentflow.channels import (
    ChannelMessage,
    # Base
    MessageChannelAdapter,
    # Gateway
    MessageGateway,
    MessageMetadata,
    UserInfo,
)
from agentflow.channels.base import MessageType

# =============================================================================
# 公開API: Context Engineering（上下文エンジニアリング）
# =============================================================================
from agentflow.context import (
    BudgetAllocation,
    BudgetConfig,
    CompressionResult,
    ContextConfig,
    # 統合インターフェース（推奨）
    ContextEngineer,
    KeyNote,
    # 重要Notes永続化
    KeyNotesStore,
    NoteImportance,
    RetrievalDecision,
    # RAG検索判定
    RetrievalGate,
    RetrievalReason,
    # Token予算管理
    TokenBudgetManager,
    # ツール関連性選択
    ToolRelevanceSelector,
    ToolScore,
    # ターン圧縮
    TurnBasedCompressor,
    TurnConfig,
)

# =============================================================================
# 公開API: Agent基底クラス
# =============================================================================
from agentflow.core.agent_block import AgentBlock
from agentflow.core.agent_registry import (
    AgentRegistry,
    get_global_agent_registry,
    reset_global_agent_registry,
)
from agentflow.core.capability_spec import (
    AgentCapabilitySpec,
    CapabilityRequirement,
    LLMRequirements,
)

# =============================================================================
# 後方互換性のため維持（非推奨 - 将来削除予定）
# =============================================================================
from agentflow.core.engine import AgentFlowEngine  # 非推奨: enginesを使用してください

# =============================================================================
# 公開API: 例外クラス
# =============================================================================
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
from agentflow.core.resilient_agent import (
    BaseDecisionAgent,
    ResilientAgent,
)
from agentflow.core.retry_advisor import (
    RetryAction,
    RetryAdvice,
    RetryAdvisor,
    RetryContext,
)
from agentflow.core.tool_binding import BoundTools, ToolBinder, ToolExecutor
from agentflow.core.type_safe import safe_enum, safe_float, safe_int

# =============================================================================
# 公開API: 統一ツール・Agentレジストリ（Auto-Agent Architecture）
# =============================================================================
from agentflow.core.tool_definition import ToolDefinition, ToolSource
from agentflow.core.tool_discovery import ToolDiscoveryService
from agentflow.core.tool_registry import (
    ToolRegistry,
    get_global_tool_registry,
    reset_global_tool_registry,
)
from agentflow.core.types import AgentMetadata, WorkflowConfig
from agentflow.engines import (
    BaseEngine,
    EngineConfig,
    GateEngine,
    HierarchicalPlanner,
    MonitoredExecutor,
    # PEV Engine（Plan-Execute-Verify）
    PEVEngine,
    PEVEngineConfig,
    PipelineEngine,
    RAGEngine,
    ResultVerifier,
    SimpleEngine,
)
from agentflow.engines.base import HITLEngineConfig

# Flow API - 後方互換（内部APIとして維持）
from agentflow.flow import (
    AgentNode,
    AgentProtocol,
    Flow,
    FlowBuilder,
    FlowConfig,
    FlowContext,
    FlowNode,
    GateNode,
    MemoryAccessor,
    NextAction,
    NodeResult,
    NodeType,
    ParallelNode,
    ReviewNode,
    ReviewVerdict,
    create_flow,
)

# =============================================================================
# 公開API: Human-in-the-Loop (HITL)
# =============================================================================
from agentflow.hitl import (
    ApprovalManager,
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    CheckpointData,
    Checkpointer,
    Command,
    CommandType,
    HITLConfig,
    InterruptError,
    InterruptSignal,
    MemoryCheckpointer,
    create_hitl_router,
    get_checkpointer,
    interrupt,
)

# =============================================================================
# 公開API: SSE/AG-UI（FastAPI統合用）
# =============================================================================
from agentflow.integrations.fastapi_integration import create_sse_response

# =============================================================================
# Knowledge Store（Memvid長期知識記憶）
# =============================================================================
from agentflow.memory.knowledge import (
    InMemoryKnowledgeStore,
    # 型定義
    KnowledgeEntry,
    # マネージャー
    KnowledgeManager,
    KnowledgeSource,
    # ストアインターフェース
    KnowledgeStore,
    # ストア実装
    MemvidKnowledgeStore,
    # 主要API（推奨）
    get_knowledge_manager,
    get_knowledge_store,
    is_memvid_available,
    reset_knowledge_manager,
)

# =============================================================================
# 公開API: Observability
# =============================================================================
from agentflow.observability import (
    LogLevel,
    get_logger,
    setup_logging,
    setup_observability,
)

# =============================================================================
# 公開API: 適応型コーディネーター（能力差を吸収する分業型Agent設計）
# =============================================================================
from agentflow.patterns.adaptive_coordinator import (
    AdaptiveCoordinator,
    AgentCapability,
    AgentProfile,
    DelegationResult,
    TaskRequirement,
)
from agentflow.perception import (
    PerceptionEvent,
    PerceptionEventType,
)

# =============================================================================
# 公開API: 富文本コンポーネント（共通モジュール）
# =============================================================================
from agentflow.protocols.a2ui.rich_content import (
    Alert,
    AlertType,
    ChartType,
    ChartView,
    Citation,
    CodeBlock,
    CollapsibleSection,
    DataTable,
    Link,
    MarkdownContent,
    Progress,
    # コンポーネント
    RichComponent,
    # 列挙型
    RichComponentType,
    # ビルダー
    RichResponse,
    Tabs,
    Timeline,
)

# MCP Tool（後方互換）
from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)

# =============================================================================
# 公開API: 松耦合Provider
# =============================================================================
from agentflow.providers import (
    DBProvider,
    EmbeddingProvider,
    LLMProvider,
    VectorDBProvider,
    get_db,
    get_embedding,
    get_llm,
    get_vectordb,
    reset_db,
    reset_embedding,
    reset_llm,
    reset_vectordb,
)
from agentflow.providers.tool_provider import tool
from agentflow.reasoner import (
    ActionDecision,
    ActionType,
    ConstraintType,
    StructuredConstraint,
    StructuredConstraints,
)

# =============================================================================
# 公開API: Run/Replay/Compare
# =============================================================================
from agentflow.run import (
    LightningEventRecord,
    LightningRuntimeConfig,
    LightningStore,
    LightningTrainingRequest,
    LightningTrainingResult,
    MemoryLightningStore,
    MemoryRunStore,
    PromptRewardSample,
    RewardSignal,
    RunDiff,
    RunRecord,
    RunStore,
    TrajectoryAdapter,
    TransitionSample,
    build_optimized_llm_profile,
    is_microsoft_lightning_available,
    resolve_lightning_store,
    train_with_lightning_backend,
)
from agentflow.security.evidence_collector import (
    FileEvidence,
    NetworkCallEvidence,
    SystemEvidence,
)
from agentflow.security.local_first import (
    ExecutionLocation,
    LocalFirstEnforcer,
    LocalFirstPolicy,
    NetworkAccessDecision,
)

# =============================================================================
# 公開API: Agent OS（Task Lifecycle, Perception, Reasoner, Security）
# =============================================================================
from agentflow.task import (
    ControlPlane,
    ExecutionPlane,
    Task,
    TaskGraph,
    TaskID,
    TaskState,
    can_transition,
    is_terminal,
)
from agentflow.tools.cli import (
    CLIToolConfig,
    CLIValidator,
)

# =============================================================================
# 公開API: World Model（状態・因果・制約の明示的表現）
# =============================================================================
from agentflow.world_model import (
    ActionPrediction,
    # 因果モデル
    CausalModel,
    CausalNode,
    CausalRelation,
    # 制約ソルバー
    ConstraintSolver,
    ConstraintViolation,
    SolverResult,
    # 世界状態
    WorldState,
    WorldStateSnapshot,
)


try:
    from importlib.metadata import PackageNotFoundError, version

    __version__ = version("agentflow")
except PackageNotFoundError:
    __version__ = "1.6.1"

# =============================================================================
# 公開API: Runtime Context（プラットフォーム向け）
# =============================================================================
# =============================================================================
# 公開API: Code Intelligence（コード智能層）
# =============================================================================
from agentflow.code_intelligence import (
    ASTNode,
    ASTNodeType,
    CodeInventory,
    # Parser
    CodeParser,
    # Transformer
    CodeTransformer,
    MigrationPhase,
    # CI/CD
    MigrationPipelineGenerator,
    # Migration
    MigrationProject,
    MigrationTracker,
    ParseContext,
    ParseResult,
    PipelineConfig,
    # Quality
    QualityGate,
    QualityGateRunner,
    QualityReport,
    TransformContext,
    TransformResult,
    # AST
    UnifiedAST,
    get_parser,
    get_transformer,
    register_parser,
    register_transformer,
)
from agentflow.runtime import (
    RuntimeContext,
    get_runtime_context,
    init_agentflow,
    set_runtime_context,
    use_runtime_context,
)

# =============================================================================
# 公開API: Agent Wizard（Meta-Agent自動生成）
# =============================================================================
from agentflow.wizard import (
    # Models
    AgentSpec,
    # AgentWizard
    AgentWizard,
    CapabilityGap,
    # Gap Detector
    CapabilityGapDetector,
    GapAnalysis,
    # Self Improvement Loop
    SelfImprovementLoop,
    # Skill Forge
    SkillForge,
    SynthesisResult,
    # System Synthesizer
    SystemSynthesizer,
    TestCase,
    # Test Synthesizer
    TestSynthesizer,
)


# =============================================================================
# 公開シンボル定義
# =============================================================================
__all__ = [
    "APIError",
    # =========================================================================
    # 統一 API 層（NEW - 前後台交互）
    # =========================================================================
    # レスポンス
    "APIResponse",
    "ASTNode",
    "ASTNodeType",
    # Reasoner
    "ActionDecision",
    "ActionPrediction",
    "ActionType",
    # =========================================================================
    # 適応型コーディネーター（能力差を吸収する分業型Agent設計）
    # =========================================================================
    "AdaptiveCoordinator",
    # =========================================================================
    # Agent基底クラス
    # =========================================================================
    "AgentBlock",
    "AgentCapability",
    # Agent能力仕様
    "AgentCapabilitySpec",
    "AgentClient",
    "AgentExecutionError",
    # =========================================================================
    # 後方互換（非推奨）
    # =========================================================================
    "AgentFlowEngine",  # 非推奨
    # =========================================================================
    # 例外クラス
    # =========================================================================
    "AgentFlowError",
    "AgentMetadata",
    "AgentNode",
    "AgentOutputValidationError",
    "AgentProfile",
    "AgentProtocol",
    # Agentレジストリ
    "AgentRegistry",
    "AgentRetryExhaustedError",
    # Models
    "AgentSpec",
    "AgentTimeoutError",
    # =========================================================================
    # Agent Wizard（Meta-Agent自動生成）
    # =========================================================================
    "AgentWizard",
    "Alert",
    "AlertType",
    "ApprovalManager",
    "ApprovalRequest",
    "ApprovalResponse",
    "ApprovalStatus",
    "BaseDecisionAgent",
    # =========================================================================
    # Engine Pattern（メインAPI - 推奨）
    # =========================================================================
    "BaseEngine",
    # ツールバインディング
    "BoundTools",
    "BudgetAllocation",
    "BudgetConfig",
    # CLI Tool Framework
    "CLIToolConfig",
    "CLIValidator",
    "CapabilityGap",
    "CapabilityGapDetector",
    "CapabilityRequirement",
    # =========================================================================
    # World Model（状態・因果・制約の明示的表現）
    # =========================================================================
    # 因果モデル
    "CausalModel",
    "CausalNode",
    "CausalRelation",
    "ChannelMessage",
    "ChartType",
    "ChartView",
    "CheckpointData",
    "Checkpointer",
    "Citation",
    "CodeBlock",
    "CodeInventory",
    # Parser
    "CodeParser",
    # Transformer
    "CodeTransformer",
    "CollapsibleSection",
    "Command",
    "CommandType",
    "CompressionResult",
    # 制約ソルバー
    "ConstraintSolver",
    "ConstraintType",
    "ConstraintViolation",
    "ContextConfig",
    # =========================================================================
    # Context Engineering（上下文エンジニアリング）
    # =========================================================================
    # 統合インターフェース
    "ContextEngineer",
    "ControlPlane",
    "DBProvider",
    "DataTable",
    "DelegationResult",
    "EmbeddingProvider",
    "EngineConfig",
    "ErrorCode",
    "ExecutionLocation",
    "ExecutionPlane",
    "FileEvidence",
    "Flow",
    "FlowBuilder",
    "FlowConfig",
    "FlowContext",
    "FlowNode",
    "GapAnalysis",
    "GateEngine",
    "GateNode",
    "HITLConfig",
    "HITLEngineConfig",
    "HierarchicalPlanner",
    "InMemoryKnowledgeStore",
    "InterruptError",
    "InterruptSignal",
    "KeyNote",
    # 重要Notes永続化
    "KeyNotesStore",
    # 型定義
    "KnowledgeEntry",
    # マネージャー
    "KnowledgeManager",
    "KnowledgeSource",
    # ストアインターフェース
    "KnowledgeStore",
    "LLMProvider",
    "LLMRequirements",
    "Link",
    "LocalFirstEnforcer",
    # Local-First Policy
    "LocalFirstPolicy",
    "LogLevel",
    # MCP Tool
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
    "MarkdownContent",
    "MemoryAccessor",
    "MemoryCheckpointer",
    "MemoryLightningStore",
    "MemoryRunStore",
    # ストア実装
    "MemvidKnowledgeStore",
    # Base
    "MessageChannelAdapter",
    # =========================================================================
    # Channels - 多平台メッセージ統合（v1.1.0 NEW）
    # =========================================================================
    # Gateway
    "MessageGateway",
    "MessageMetadata",
    "MessageType",
    "MigrationPhase",
    # CI/CD
    "MigrationPipelineGenerator",
    # Migration
    "MigrationProject",
    "MigrationTracker",
    "MonitoredExecutor",
    "NetworkAccessDecision",
    "NetworkCallEvidence",
    "NextAction",
    "NodeResult",
    "NodeType",
    "NoteImportance",
    # PEV Engine（Plan-Execute-Verify）
    "PEVEngine",
    "PEVEngineConfig",
    "PagedResponse",
    "ParallelNode",
    "ParseContext",
    "ParseResult",
    # Perception
    "PerceptionEvent",
    "PerceptionEventType",
    "PipelineConfig",
    "PipelineEngine",
    "Progress",
    "ProtocolError",
    # Quality
    "QualityGate",
    "QualityGateRunner",
    "QualityReport",
    "RAGEngine",
    "ResilientAgent",
    "ResultVerifier",
    "RetryAction",
    "RetryAdvice",
    "RetryAdvisor",
    "RetryContext",
    "RetrievalDecision",
    # RAG検索判定
    "RetrievalGate",
    "RetrievalReason",
    "ReviewNode",
    "ReviewVerdict",
    "RewardSignal",
    # コンポーネント
    "RichComponent",
    # =========================================================================
    # 富文本コンポーネント（共通モジュール）
    # =========================================================================
    # 列挙型
    "RichComponentType",
    # ビルダー
    "RichResponse",
    # Rich Builder
    "RichResponseBuilder",
    "RouterConfig",
    "RunDiff",
    # =========================================================================
    # Run/Replay/Compare
    # =========================================================================
    "LightningEventRecord",
    "LightningRuntimeConfig",
    "LightningStore",
    "LightningTrainingRequest",
    "LightningTrainingResult",
    "PromptRewardSample",
    "RunRecord",
    "RunStore",
    "TrajectoryAdapter",
    "TransitionSample",
    "build_optimized_llm_profile",
    "is_microsoft_lightning_available",
    "resolve_lightning_store",
    "train_with_lightning_backend",
    # =========================================================================
    # Runtime Context（プラットフォーム向け）
    # =========================================================================
    "RuntimeContext",
    # SSE
    "SSEEmitter",
    "SSEEvent",
    "SelfImprovementLoop",
    "SimpleEngine",
    "SkillForge",
    "SolverResult",
    "StreamEvent",
    "StreamEventType",
    "StructuredConstraint",
    "StructuredConstraints",
    "SynthesisResult",
    # Security / Evidence
    "SystemEvidence",
    "SystemSynthesizer",
    "Tabs",
    "Task",
    "TaskGraph",
    # =========================================================================
    # Agent OS（Task Lifecycle, Perception, Reasoner, Security）
    # =========================================================================
    # Task Lifecycle
    "TaskID",
    "TaskRequirement",
    "TaskState",
    "TestCase",
    "TestSynthesizer",
    "Timeline",
    # Token予算管理
    "TokenBudgetManager",
    "ToolBinder",
    # =========================================================================
    # 統一ツール・Agentレジストリ（Auto-Agent Architecture）
    # =========================================================================
    # ツール定義
    "ToolDefinition",
    # ツール発見
    "ToolDiscoveryService",
    "ToolExecutor",
    # ツールレジストリ
    "ToolRegistry",
    # ツール関連性選択
    "ToolRelevanceSelector",
    "ToolScore",
    "ToolSource",
    "TransformContext",
    "TransformResult",
    # ターン圧縮
    "TurnBasedCompressor",
    "TurnConfig",
    # =========================================================================
    # Code Intelligence（コード智能層）
    # =========================================================================
    # AST
    "UnifiedAST",
    "UserInfo",
    "VectorDBProvider",
    "WSMessage",
    "WSMessageType",
    # WebSocket
    "WebSocketHub",
    "WorkflowConfig",
    "WorkflowError",
    "WorkflowNotFoundError",
    # 世界状態
    "WorldState",
    "WorldStateSnapshot",
    # =========================================================================
    # Decorator API（推奨）
    # =========================================================================
    "agent",
    "can_transition",
    # Router Factory
    "create_agent_router",
    # Flow API（内部API - 上級者向け）
    "create_flow",
    "create_hitl_router",
    # =========================================================================
    # SSE/AG-UI
    # =========================================================================
    "create_sse_response",
    "create_websocket_router",
    "get_checkpointer",
    "get_db",
    "get_embedding",
    "get_global_agent_registry",
    "get_global_tool_registry",
    # =========================================================================
    # Knowledge Store（Memvid長期知識記憶）
    # =========================================================================
    # 主要API
    "get_knowledge_manager",
    "get_knowledge_store",
    # =========================================================================
    # 松耦合Provider（推奨）
    # =========================================================================
    "get_llm",
    "get_logger",
    "get_parser",
    "get_runtime_context",
    "get_skill",
    "get_transformer",
    "get_vectordb",
    "init_agentflow",
    # =========================================================================
    # Human-in-the-Loop (HITL)
    # =========================================================================
    "interrupt",
    "is_memvid_available",
    "is_terminal",
    "list_skills",
    "register_parser",
    "register_transformer",
    "reset_db",
    "reset_embedding",
    "reset_global_agent_registry",
    "reset_global_tool_registry",
    "reset_knowledge_manager",
    "reset_llm",
    "reset_vectordb",
    "set_runtime_context",
    # =========================================================================
    # Observability
    # =========================================================================
    "setup_logging",
    "setup_observability",
    "tool",
    "use_runtime_context",
    "safe_enum",
    "safe_float",
    "safe_int",
]
