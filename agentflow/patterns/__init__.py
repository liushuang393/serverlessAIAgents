"""AgentFlow Patterns - Agentic AI デザインパターン.

このモジュールは以下のパターンを提供します：

1. **DeepAgent（推奨）** - 智能型Agent協調
   - 認知分析 → タスク分解 → 動的Agent割当 → 自己進化
   - 複雑なタスク、柔軟な対応に最適

2. **Reflexion** - 失敗学習パターン
   - 失敗から自動的に学習
   - verbal reflection による改善
   - 参考論文: Reflexion (NeurIPS 2023)

3. **Reflection** - 自己改善ループ
   - 単一Agentの反復改善
   - Generate → Reflect → Improve サイクル

非推奨（Engine + A2AHub で代替）:
- AgentPipeline → PipelineEngine を使用
- AgentComposer → PipelineEngine + A2AHub を使用
- SwarmCoordinator → A2AHub を使用
- AdaptiveCoordinator → A2AHub を使用

設計原則:
- Agent定義: ResilientAgent[I, O] のみ
- Agent間通信: LocalA2AHub（A2Aプロトコル）のみ
- Agent実行: Engine 層（PipelineEngine / SimpleEngine / GateEngine 等）
"""

# =============================================================================
# 専門化Agent（FAQなど）
# =============================================================================
from agentflow.agents import (
    FAQAgent,
    FAQAgentConfig,
    SalesAgent,
    SalesAgentConfig,
)

# =============================================================================
# 基底クラス（非推奨 - 後方互換のため維持）
# =============================================================================
from agentflow.patterns.coordinator import (
    CoordinationPattern,
    CoordinatorBase,
)
from agentflow.patterns.deep_agent import (
    # データモデル
    AgentMessage,
    # Agent管理
    AgentPool,
    AgentType,
    AnalysisAgent,
    BaseAgent,
    CognitiveAnalysis,
    CompactionResult,
    CompactionStrategy,
    # コンテキスト管理
    ContextCompressor,
    # メインCoordinator
    DeepAgentCoordinator,
    EvolutionRecord,
    # ストレージ
    EvolutionStore,
    ExecutionAgent,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    MemoryTier,
    MessageType,
    ParallelGroup,
    PlanningAgent,
    ProgressEvent,
    # 進捗管理
    ProgressManager,
    QualityDimension,
    QualityReview,
    ReportAgent,
    ResearchAgent,
    ReviewAgent,
    RuntimeStore,
    # 進化
    SelfEvolver,
    TaskStatus,
    TodoItem,
)

# =============================================================================
# 内部使用（SSE進捗配信）
# =============================================================================
from agentflow.patterns.progress_emitter import (
    AgentMeta,
    ProgressEmitter,
)

# =============================================================================
# 3. Reflection Pattern - 自己改善ループ
# =============================================================================
from agentflow.patterns.reflection import (
    ImproverAgent,
    ReflectionLoop,
    ReflectionResult,
    ReflectionWorkflow,
    ReflectorAgent,
)

# =============================================================================
# 2. Reflexion Pattern（NEW）- 失敗学習パターン
# =============================================================================
from agentflow.patterns.reflexion import (
    FailurePattern,
    LearningOutcome,
    LLMReflectionGenerator,
    # データモデル
    Reflection,
    # 生成器
    ReflectionGenerator,
    ReflectionType,
    # メイン
    ReflectiveEvolver,
    Severity,
)

# =============================================================================
# 共有コンテキスト
# =============================================================================
from agentflow.patterns.shared_context import SharedContext

# =============================================================================
# 6. TaskDecomposer - 高度なタスク分解システム
# =============================================================================
from agentflow.patterns.task_decomposer import (
    # データモデル
    DecomposedTask,
    DecompositionConfig,
    DecompositionPlan,
    # 依存関係グラフ
    DependencyGraph,
    # メイン
    TaskDecomposer,
    TaskGranularity,
    TaskPriority,
)


__all__ = [
    # ==========================================================================
    # 1. DeepAgent Pattern（推奨）
    # ==========================================================================
    "AgentMessage",
    "AgentPool",
    "AgentType",
    "AnalysisAgent",
    "BaseAgent",
    "CognitiveAnalysis",
    "CompactionResult",
    "CompactionStrategy",
    "ContextCompressor",
    "DeepAgentCoordinator",
    "EvolutionRecord",
    "EvolutionStore",
    "ExecutionAgent",
    "MemoryEvolutionStore",
    "MemoryRuntimeStore",
    "MemoryTier",
    "MessageType",
    "ParallelGroup",
    "PlanningAgent",
    "ProgressEvent",
    "ProgressManager",
    "QualityDimension",
    "QualityReview",
    "ReportAgent",
    "ResearchAgent",
    "ReviewAgent",
    "RuntimeStore",
    "SelfEvolver",
    "TaskStatus",
    "TodoItem",
    # ==========================================================================
    # 2. Reflexion Pattern（失敗学習）
    # ==========================================================================
    "FailurePattern",
    "LLMReflectionGenerator",
    "LearningOutcome",
    "Reflection",
    "ReflectionGenerator",
    "ReflectionType",
    "ReflectiveEvolver",
    "Severity",
    # ==========================================================================
    # 3. Reflection Pattern（自己改善ループ）
    # ==========================================================================
    "ImproverAgent",
    "ReflectionLoop",
    "ReflectionResult",
    "ReflectionWorkflow",
    "ReflectorAgent",
    # ==========================================================================
    # 専門化Agent
    # ==========================================================================
    "FAQAgent",
    "FAQAgentConfig",
    "SalesAgent",
    "SalesAgentConfig",
    # ==========================================================================
    # 基底クラス（非推奨 - 後方互換）
    # ==========================================================================
    "CoordinationPattern",
    "CoordinatorBase",
    # ==========================================================================
    # 内部使用（SSE進捗配信）
    # ==========================================================================
    "AgentMeta",
    "ProgressEmitter",
    # ==========================================================================
    # 共有コンテキスト
    # ==========================================================================
    "SharedContext",
    # ==========================================================================
    # TaskDecomposer（タスク分解）
    # ==========================================================================
    "DecomposedTask",
    "DecompositionConfig",
    "DecompositionPlan",
    "DependencyGraph",
    "TaskDecomposer",
    "TaskGranularity",
    "TaskPriority",
]
