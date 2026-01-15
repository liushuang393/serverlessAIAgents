"""AgentFlow Patterns - Agentic AI デザインパターン.

このモジュールは4つの主要パターンを提供します：

1. **DeepAgent（推奨）** - 智能型Agent協調
   - 認知分析 → タスク分解 → 動的Agent割当 → 自己進化
   - AgentPoolで6個の汎用Agent + カスタムAgent追加対応
   - 複雑なタスク、柔軟な対応に最適

2. **Reflexion（NEW）** - 失敗学習パターン
   - 失敗から自動的に学習
   - verbal reflection による改善
   - 参考論文: Reflexion (NeurIPS 2023)

3. **Reflection** - 自己改善ループ
   - 単一Agentの反復改善
   - Generate → Reflect → Improve サイクル

4. **AgentPipeline** - 順次実行パイプライン
   - 複数Agentの順序実行
   - SSE進捗配信対応

設計原則:
- 簡単: AgentBlock ベース、理解しやすい
- 柔軟: カスタムAgent追加が容易
- 健壮: エラーハンドリングと fallback
- 独立: 外部フレームワーク不要

参考（思想のみ吸収）:
- LangChain: DeepAgents Framework
- Anthropic: Building Effective Agents
- Reflexion: Language Agents with Verbal Reinforcement Learning
"""

# =============================================================================
# 1. DeepAgent Pattern（推奨）- 智能型Agent協調
# =============================================================================
from agentflow.patterns.deep_agent import (
    # メインCoordinator
    DeepAgentCoordinator,
    # Agent管理
    AgentPool,
    BaseAgent,
    ResearchAgent,
    AnalysisAgent,
    PlanningAgent,
    ExecutionAgent,
    ReviewAgent,
    ReportAgent,
    # データモデル
    AgentMessage,
    AgentType,
    CognitiveAnalysis,
    CompactionStrategy,
    EvolutionRecord,
    MessageType,
    ParallelGroup,
    QualityDimension,
    QualityReview,
    TaskStatus,
    TodoItem,
    CompactionResult,
    MemoryTier,
    # ストレージ
    EvolutionStore,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    RuntimeStore,
    # コンテキスト管理
    ContextCompressor,
    # 進捗管理
    ProgressManager,
    ProgressEvent,
    # 進化
    SelfEvolver,
)

# =============================================================================
# 2. Reflexion Pattern（NEW）- 失敗学習パターン
# =============================================================================
from agentflow.patterns.reflexion import (
    # データモデル
    Reflection,
    ReflectionType,
    Severity,
    FailurePattern,
    LearningOutcome,
    # 生成器
    ReflectionGenerator,
    LLMReflectionGenerator,
    # メイン
    ReflectiveEvolver,
)

# =============================================================================
# 3. Reflection Pattern - 自己改善ループ
# =============================================================================
from agentflow.patterns.reflection import (
    ReflectionWorkflow,
    ReflectionLoop,
    ReflectionResult,
    ReflectorAgent,
    ImproverAgent,
)

# =============================================================================
# 4. AgentPipeline - 順次実行パイプライン
# =============================================================================
from agentflow.patterns.agent_pipeline import (
    AgentPipeline,
    AgentConfig,
    AgentProtocol,
    PipelineConfig,
    RevisionRequest,
)

# =============================================================================
# 内部使用（SSE進捗配信）
# =============================================================================
from agentflow.patterns.progress_emitter import (
    ProgressEmitter,
    AgentMeta,
)

# =============================================================================
# 共有コンテキスト
# =============================================================================
from agentflow.patterns.shared_context import SharedContext

# =============================================================================
# 基底クラス（協調器基底）
# =============================================================================
from agentflow.patterns.coordinator import (
    CoordinatorBase,
    CoordinationPattern,
)

# =============================================================================
# 5. AgentComposer（NEW）- 標準Agent組合パターン
# =============================================================================
from agentflow.patterns.composer import (
    AgentComposer,
    CompositionPattern,
    CompositionConfig,
    CompositionResult,
    AgentNode,
    AgentRole,
    TaskAssignment,
    AgentRouter,
    CapabilityBasedRouter,
    RoundRobinRouter,
)

# =============================================================================
# 6. TaskDecomposer - 高度なタスク分解システム
# =============================================================================
from agentflow.patterns.task_decomposer import (
    # データモデル
    DecomposedTask,
    DecompositionConfig,
    DecompositionPlan,
    TaskGranularity,
    TaskPriority,
    # 依存関係グラフ
    DependencyGraph,
    # メイン
    TaskDecomposer,
)

# =============================================================================
# 専門化Agent（FAQなど）
# =============================================================================
from agentflow.agents import (
    FAQAgent,
    FAQAgentConfig,
    SalesAgent,
    SalesAgentConfig,
)


__all__ = [
    # ==========================================================================
    # 1. DeepAgent Pattern（推奨）
    # ==========================================================================
    # メインCoordinator
    "DeepAgentCoordinator",
    # Agent管理
    "AgentPool",
    "BaseAgent",
    "ResearchAgent",
    "AnalysisAgent",
    "PlanningAgent",
    "ExecutionAgent",
    "ReviewAgent",
    "ReportAgent",
    # データモデル
    "AgentMessage",
    "AgentType",
    "CognitiveAnalysis",
    "CompactionStrategy",
    "CompactionResult",
    "EvolutionRecord",
    "MemoryTier",
    "MessageType",
    "ParallelGroup",
    "QualityDimension",
    "QualityReview",
    "TaskStatus",
    "TodoItem",
    # ストレージ
    "EvolutionStore",
    "MemoryEvolutionStore",
    "MemoryRuntimeStore",
    "RuntimeStore",
    # コンテキスト管理
    "ContextCompressor",
    # 進捗管理
    "ProgressManager",
    "ProgressEvent",
    # 進化
    "SelfEvolver",
    # ==========================================================================
    # 2. Reflexion Pattern（NEW - 失敗学習）
    # ==========================================================================
    # データモデル
    "Reflection",
    "ReflectionType",
    "Severity",
    "FailurePattern",
    "LearningOutcome",
    # 生成器
    "ReflectionGenerator",
    "LLMReflectionGenerator",
    # メイン
    "ReflectiveEvolver",
    # ==========================================================================
    # 3. Reflection Pattern（自己改善ループ）
    # ==========================================================================
    "ReflectionWorkflow",
    "ReflectionLoop",
    "ReflectionResult",
    "ReflectorAgent",
    "ImproverAgent",
    # ==========================================================================
    # 4. AgentPipeline
    # ==========================================================================
    "AgentPipeline",
    "AgentConfig",
    "AgentProtocol",
    "PipelineConfig",
    "RevisionRequest",
    # ==========================================================================
    # 内部使用（SSE進捗配信）
    # ==========================================================================
    "ProgressEmitter",
    "AgentMeta",
    # ==========================================================================
    # 共有コンテキスト
    # ==========================================================================
    "SharedContext",
    # ==========================================================================
    # 基底クラス
    # ==========================================================================
    "CoordinatorBase",
    "CoordinationPattern",
    # ==========================================================================
    # 5. AgentComposer（NEW - 標準Agent組合）
    # ==========================================================================
    "AgentComposer",
    "CompositionPattern",
    "CompositionConfig",
    "CompositionResult",
    "AgentNode",
    "AgentRole",
    "TaskAssignment",
    "AgentRouter",
    "CapabilityBasedRouter",
    "RoundRobinRouter",
    # ==========================================================================
    # 6. TaskDecomposer（高度なタスク分解）
    # ==========================================================================
    "DecomposedTask",
    "DecompositionConfig",
    "DecompositionPlan",
    "TaskGranularity",
    "TaskPriority",
    "DependencyGraph",
    "TaskDecomposer",
    # ==========================================================================
    # 専門化Agent
    # ==========================================================================
    "FAQAgent",
    "FAQAgentConfig",
    "SalesAgent",
    "SalesAgentConfig",
]

