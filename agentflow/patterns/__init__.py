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
# 4. AgentPipeline - 順次実行パイプライン
# =============================================================================
from agentflow.patterns.agent_pipeline import (
    AgentConfig,
    AgentPipeline,
    AgentProtocol,
    PipelineConfig,
    RevisionRequest,
)

# =============================================================================
# 5. AgentComposer（NEW）- 標準Agent組合パターン
# =============================================================================
from agentflow.patterns.composer import (
    AgentComposer,
    AgentNode,
    AgentRole,
    AgentRouter,
    CapabilityBasedRouter,
    CompositionConfig,
    CompositionPattern,
    CompositionResult,
    RoundRobinRouter,
    TaskAssignment,
)

# =============================================================================
# 基底クラス（協調器基底）
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
    # 5. AgentComposer（NEW - 標準Agent組合）
    # ==========================================================================
    "AgentComposer",
    "AgentConfig",
    # データモデル
    "AgentMessage",
    "AgentMeta",
    "AgentNode",
    # ==========================================================================
    # 4. AgentPipeline
    # ==========================================================================
    "AgentPipeline",
    # Agent管理
    "AgentPool",
    "AgentProtocol",
    "AgentRole",
    "AgentRouter",
    "AgentType",
    "AnalysisAgent",
    "BaseAgent",
    "CapabilityBasedRouter",
    "CognitiveAnalysis",
    "CompactionResult",
    "CompactionStrategy",
    "CompositionConfig",
    "CompositionPattern",
    "CompositionResult",
    # コンテキスト管理
    "ContextCompressor",
    "CoordinationPattern",
    # ==========================================================================
    # 基底クラス
    # ==========================================================================
    "CoordinatorBase",
    # ==========================================================================
    # 6. TaskDecomposer（高度なタスク分解）
    # ==========================================================================
    "DecomposedTask",
    "DecompositionConfig",
    "DecompositionPlan",
    # ==========================================================================
    # 1. DeepAgent Pattern（推奨）
    # ==========================================================================
    # メインCoordinator
    "DeepAgentCoordinator",
    "DependencyGraph",
    "EvolutionRecord",
    # ストレージ
    "EvolutionStore",
    "ExecutionAgent",
    # ==========================================================================
    # 専門化Agent
    # ==========================================================================
    "FAQAgent",
    "FAQAgentConfig",
    "FailurePattern",
    "ImproverAgent",
    "LLMReflectionGenerator",
    "LearningOutcome",
    "MemoryEvolutionStore",
    "MemoryRuntimeStore",
    "MemoryTier",
    "MessageType",
    "ParallelGroup",
    "PipelineConfig",
    "PlanningAgent",
    # ==========================================================================
    # 内部使用（SSE進捗配信）
    # ==========================================================================
    "ProgressEmitter",
    "ProgressEvent",
    # 進捗管理
    "ProgressManager",
    "QualityDimension",
    "QualityReview",
    # ==========================================================================
    # 2. Reflexion Pattern（NEW - 失敗学習）
    # ==========================================================================
    # データモデル
    "Reflection",
    # 生成器
    "ReflectionGenerator",
    "ReflectionLoop",
    "ReflectionResult",
    "ReflectionType",
    # ==========================================================================
    # 3. Reflection Pattern（自己改善ループ）
    # ==========================================================================
    "ReflectionWorkflow",
    # メイン
    "ReflectiveEvolver",
    "ReflectorAgent",
    "ReportAgent",
    "ResearchAgent",
    "ReviewAgent",
    "RevisionRequest",
    "RoundRobinRouter",
    "RuntimeStore",
    "SalesAgent",
    "SalesAgentConfig",
    # 進化
    "SelfEvolver",
    "Severity",
    # ==========================================================================
    # 共有コンテキスト
    # ==========================================================================
    "SharedContext",
    "TaskAssignment",
    "TaskDecomposer",
    "TaskGranularity",
    "TaskPriority",
    "TaskStatus",
    "TodoItem",
]
