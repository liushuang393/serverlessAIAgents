"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow はAIエージェント開発のための軽量フレームワークです。
MCP、A2A、AG-UI プロトコルをネイティブサポートしています。

=============================================================================
Core API（最初に覚える）
=============================================================================

Agent定義とフロー構築:
    >>> from agentflow import Agent, FlowBuilder, Flow
    >>> from agentflow import SimpleEngine, PipelineEngine, GateEngine, RAGEngine, PEVEngine
    >>> from agentflow import agent, get_llm, SSEEmitter

    >>> # @agent デコレータ（プロトタイプ向け）
    >>> @agent
    ... class MyAgent:
    ...     system_prompt = "親切なアシスタント"

    >>> # PipelineEngine（マルチステップフロー）
    >>> engine = PipelineEngine(
    ...     stages=[
    ...         {"name": "gate", "agent": GateAgent, "gate": True},
    ...         {"name": "analysis", "agents": [Agent1, Agent2]},
    ...     ],
    ... )

=============================================================================
Standard API（必要になったら）
=============================================================================

    >>> from agentflow import AgentRegistry, FlowContext, create_flow
    >>> from agentflow import HITLConfig, interrupt, ApprovalManager
    >>> from agentflow import get_db, get_vectordb, get_embedding

=============================================================================
Advanced API（フレームワーク拡張時のみ）
=============================================================================

    # サブモジュールを直接参照することを推奨
    # agentflow.flow      - 低レベルフロー構築API
    # agentflow.patterns  - デザインパターン実装
    # agentflow.core      - 基底クラスとユーティリティ
    # agentflow.integrations - フレームワーク統合

環境変数:
    フレームワーク初期化時に .env ファイルを自動読み込み。
    明示的な初期化は init_agentflow() を使用してください。
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

# =============================================================================
# 公開API: 統一ツールカタログ（全ツールソース統合）
# =============================================================================
from agentflow.core.tool_catalog import (
    CatalogEntry,
    CatalogSource,
    ToolCatalogManager,
    get_tool_catalog,
    reset_tool_catalog,
)

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
from agentflow.core.type_safe import safe_enum, safe_float, safe_int
from agentflow.core.types import AgentMetadata, WorkflowConfig

# =============================================================================
# 公開API: 統一データベース管理（DB セッション + Alembic マイグレーション）
# =============================================================================
from agentflow.database import (
    DatabaseConfig,
    DatabaseManager,
    MigrationEnv,
    get_dialect,
    is_async_url,
    is_sqlite,
    to_async_url,
    to_sync_url,
)
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


# テスト・後方互換用エイリアス
FlowWrapper = Flow

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
    __version__ = "2.0.0"

# =============================================================================
# 公開API: Bootstrap（積木/プラグインモード + 動的能力自動接続）
# =============================================================================
from agentflow.agents.mixins import RAGCapableMixin
from agentflow.bootstrap import (
    AppCapabilityBootstrapper,
    CapabilityBundle,
    ConfigWatcher,
    build_rag_engine,
    build_skill_gateway,
)

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
# Agent は ResilientAgent の標準エイリアス（推奨エントリーポイント）
# =============================================================================
Agent = ResilientAgent


# =============================================================================
# 公開シンボル定義（Core / Standard / Advanced の3層構造）
# =============================================================================
__all__ = [  # noqa: RUF022 - 3層構造のため意図的な非アルファベット順
    # =========================================================================
    # Core API - まずここから（最もよく使う）
    # =========================================================================
    # Agent定義
    "Agent",            # ResilientAgent の推奨エイリアス（本番向け）
    "ResilientAgent",   # 型安全・リトライ付きAgent基底クラス
    "agent",            # @agent デコレータ（プロトタイプ向け）
    "AgentClient",      # Agentの呼び出し
    # フロー構築
    "FlowBuilder",
    "Flow",
    "create_flow",
    # Engine（高レベルAPI）
    "SimpleEngine",
    "PipelineEngine",
    "GateEngine",
    "RAGEngine",
    "PEVEngine",
    "EngineConfig",
    # Provider（松耦合）
    "get_llm",
    "get_db",
    "get_vectordb",
    "get_embedding",
    # SSE / ストリーミング
    "SSEEmitter",
    "SSEEvent",

    # =========================================================================
    # Standard API - 必要になったら使う
    # =========================================================================
    # Agent詳細
    "AgentBlock",           # レガシー（非推奨 - ResilientAgent を使用してください）
    "BaseDecisionAgent",
    "AgentRegistry",
    "get_global_agent_registry",
    # フロー詳細
    "FlowContext",
    "FlowConfig",
    "NodeResult",
    "ReviewVerdict",
    "NextAction",
    # HITL（Human-in-the-Loop）
    "HITLConfig",
    "HITLEngineConfig",
    "interrupt",
    "ApprovalManager",
    "Checkpointer",
    "MemoryCheckpointer",
    "Command",
    "CommandType",
    # エンジン詳細
    "BaseEngine",
    "HierarchicalPlanner",
    "PEVEngineConfig",
    # Provider詳細
    "LLMProvider",
    "DBProvider",
    "VectorDBProvider",
    "EmbeddingProvider",
    "reset_llm",
    "reset_db",
    "reset_vectordb",
    "reset_embedding",
    # API / イベント
    "APIResponse",
    "StreamEvent",
    "StreamEventType",
    "create_agent_router",
    # 例外
    "AgentFlowError",
    "AgentExecutionError",
    "AgentTimeoutError",
    "WorkflowError",
    # ユーティリティ
    "safe_enum",
    "init_agentflow",
    # Observability
    "get_logger",
    "setup_logging",
    "setup_observability",
    "LogLevel",

    # =========================================================================
    # Advanced API - フレームワーク拡張・上級者向け
    # （直接 from agentflow.xxx import yyy を推奨）
    # =========================================================================
    # 統一 API 層
    "APIError",
    "ErrorCode",
    "PagedResponse",
    "RichResponseBuilder",
    "RouterConfig",
    "WebSocketHub",
    "WSMessage",
    "WSMessageType",
    "create_websocket_router",
    # Channels - 多平台メッセージ統合
    "ChannelMessage",
    "MessageChannelAdapter",
    "MessageGateway",
    "MessageMetadata",
    "MessageType",
    "UserInfo",
    # Context Engineering
    "BudgetAllocation",
    "BudgetConfig",
    "CompressionResult",
    "ContextConfig",
    "ContextEngineer",
    "KeyNote",
    "KeyNotesStore",
    "NoteImportance",
    "RetrievalDecision",
    "RetrievalGate",
    "RetrievalReason",
    "TokenBudgetManager",
    "ToolRelevanceSelector",
    "ToolScore",
    "TurnBasedCompressor",
    "TurnConfig",
    # Agent能力仕様
    "AgentCapabilitySpec",
    "CapabilityRequirement",
    "LLMRequirements",
    # 後方互換（非推奨）
    "AgentFlowEngine",  # 非推奨: enginesを使用してください
    # 例外クラス詳細
    "AgentOutputValidationError",
    "AgentRetryExhaustedError",
    "ProtocolError",
    "WorkflowNotFoundError",
    # リトライ
    "RetryAction",
    "RetryAdvice",
    "RetryAdvisor",
    "RetryContext",
    # ツールバインディング
    "BoundTools",
    "ToolBinder",
    "ToolExecutor",
    # 統一ツールカタログ
    "CatalogEntry",
    "CatalogSource",
    "ToolCatalogManager",
    "get_tool_catalog",
    "reset_tool_catalog",
    # 統一ツール・Agentレジストリ
    "ToolDefinition",
    "ToolSource",
    "ToolDiscoveryService",
    "ToolRegistry",
    "get_global_tool_registry",
    "reset_global_tool_registry",
    # 型安全ユーティリティ
    "safe_float",
    "safe_int",
    # 型定義
    "AgentMetadata",
    "WorkflowConfig",
    # データベース管理
    "DatabaseConfig",
    "DatabaseManager",
    "MigrationEnv",
    "get_dialect",
    "is_async_url",
    "is_sqlite",
    "to_async_url",
    "to_sync_url",
    # エンジン詳細
    "MonitoredExecutor",
    "ResultVerifier",
    # フロー低レベルAPI
    "AgentNode",
    "AgentProtocol",
    "FlowNode",
    "FlowWrapper",
    "GateNode",
    "MemoryAccessor",
    "NodeType",
    "ParallelNode",
    "ReviewNode",
    # HITL詳細
    "ApprovalRequest",
    "ApprovalResponse",
    "ApprovalStatus",
    "CheckpointData",
    "InterruptError",
    "InterruptSignal",
    "create_hitl_router",
    "get_checkpointer",
    # SSE/AG-UI
    "create_sse_response",
    # Knowledge Store（Memvid長期知識記憶）
    "InMemoryKnowledgeStore",
    "KnowledgeEntry",
    "KnowledgeManager",
    "KnowledgeSource",
    "KnowledgeStore",
    "MemvidKnowledgeStore",
    "get_knowledge_manager",
    "get_knowledge_store",
    "is_memvid_available",
    "reset_knowledge_manager",
    # 適応型コーディネーター
    "AdaptiveCoordinator",
    "AgentCapability",
    "AgentProfile",
    "DelegationResult",
    "TaskRequirement",
    # Perception
    "PerceptionEvent",
    "PerceptionEventType",
    # 富文本コンポーネント
    "Alert",
    "AlertType",
    "ChartType",
    "ChartView",
    "Citation",
    "CodeBlock",
    "CollapsibleSection",
    "DataTable",
    "Link",
    "MarkdownContent",
    "Progress",
    "RichComponent",
    "RichComponentType",
    "RichResponse",
    "Tabs",
    "Timeline",
    # MCP Tool
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
    # Reasoner
    "ActionDecision",
    "ActionType",
    "ConstraintType",
    "StructuredConstraint",
    "StructuredConstraints",
    # Run/Replay/Compare
    "LightningEventRecord",
    "LightningRuntimeConfig",
    "LightningStore",
    "LightningTrainingRequest",
    "LightningTrainingResult",
    "MemoryLightningStore",
    "MemoryRunStore",
    "PromptRewardSample",
    "RewardSignal",
    "RunDiff",
    "RunRecord",
    "RunStore",
    "TrajectoryAdapter",
    "TransitionSample",
    "build_optimized_llm_profile",
    "is_microsoft_lightning_available",
    "resolve_lightning_store",
    "train_with_lightning_backend",
    # Security / Evidence
    "FileEvidence",
    "NetworkCallEvidence",
    "SystemEvidence",
    "ExecutionLocation",
    "LocalFirstEnforcer",
    "LocalFirstPolicy",
    "NetworkAccessDecision",
    # Agent OS（Task Lifecycle）
    "ControlPlane",
    "ExecutionPlane",
    "Task",
    "TaskGraph",
    "TaskID",
    "TaskState",
    "can_transition",
    "is_terminal",
    # CLI Tool Framework
    "CLIToolConfig",
    "CLIValidator",
    # World Model
    "ActionPrediction",
    "CausalModel",
    "CausalNode",
    "CausalRelation",
    "ConstraintSolver",
    "ConstraintViolation",
    "SolverResult",
    "WorldState",
    "WorldStateSnapshot",
    # Bootstrap
    "AppCapabilityBootstrapper",
    "CapabilityBundle",
    "ConfigWatcher",
    "RAGCapableMixin",
    "build_rag_engine",
    "build_skill_gateway",
    # Code Intelligence
    "ASTNode",
    "ASTNodeType",
    "CodeInventory",
    "CodeParser",
    "CodeTransformer",
    "MigrationPhase",
    "MigrationPipelineGenerator",
    "MigrationProject",
    "MigrationTracker",
    "ParseContext",
    "ParseResult",
    "PipelineConfig",
    "QualityGate",
    "QualityGateRunner",
    "QualityReport",
    "TransformContext",
    "TransformResult",
    "UnifiedAST",
    "get_parser",
    "get_transformer",
    "register_parser",
    "register_transformer",
    # Runtime Context
    "RuntimeContext",
    "get_runtime_context",
    "set_runtime_context",
    "use_runtime_context",
    # Agent Wizard
    "AgentSpec",
    "AgentWizard",
    "CapabilityGap",
    "CapabilityGapDetector",
    "GapAnalysis",
    "SelfImprovementLoop",
    "SkillForge",
    "SynthesisResult",
    "SystemSynthesizer",
    "TestCase",
    "TestSynthesizer",
    # Decorator API
    "get_skill",
    "list_skills",
    "tool",
    # Agent registry helpers
    "reset_global_agent_registry",
]
