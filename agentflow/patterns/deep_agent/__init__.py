# -*- coding: utf-8 -*-
"""DeepAgent パターン - 高度なマルチエージェント協調システム.

DeepAgentsフレームワーク（arXiv:2504.04755）に基づく実装。
複雑なタスクを自律的に分解・実行・検証する。

モジュール構成:
- da_models: データモデル（TodoItem, CognitiveAnalysis, QualityReview等）
- da_stores: ストレージ抽象（RuntimeStore, EvolutionStore）
- da_compressor: コンテキスト圧縮（MemGPT/Letta風階層メモリ）
- da_pool: Agent池（5-6個の基本Agent）
- da_progress: 進捗管理（リアルタイム追跡）
- da_evolver: 自己進化（パターン学習）
- da_coordinator: メイン協調器

使用例:
    >>> from agentflow.patterns.deep_agent import DeepAgentCoordinator
    >>> coordinator = DeepAgentCoordinator(llm_client=my_llm)
    >>> result = await coordinator.execute("市場調査レポートを作成して")

設計原則:
1. 認知分析: タスクの意図・複雑度を理解
2. タスク分解: TodoListへの分解
3. 並行実行: 依存関係に基づく効率的な実行
4. 品質評審: 多次元品質評価
5. 自己進化: 成功パターンの学習と再利用
"""

from agentflow.patterns.deep_agent.da_compressor import ContextCompressor
from agentflow.patterns.deep_agent.da_coordinator import DeepAgentCoordinator
from agentflow.patterns.deep_agent.da_evolver import SelfEvolver
from agentflow.patterns.deep_agent.da_models import (
    AgentMessage,
    AgentType,
    CognitiveAnalysis,
    CompactionResult,
    CompactionStrategy,
    EvolutionRecord,
    MemoryTier,
    MessageType,
    ParallelGroup,
    QualityDimension,
    QualityReview,
    TaskStatus,
    TodoItem,
)
from agentflow.patterns.deep_agent.da_pool import (
    AgentPool,
    AnalysisAgent,
    BaseAgent,
    ExecutionAgent,
    PlanningAgent,
    ReportAgent,
    ResearchAgent,
    ReviewAgent,
)
from agentflow.patterns.deep_agent.da_progress import ProgressEvent, ProgressManager
from agentflow.patterns.deep_agent.da_stores import (
    EvolutionStore,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    RuntimeStore,
)

__all__ = [
    # メイン協調器
    "DeepAgentCoordinator",
    # データモデル
    "TaskStatus",
    "AgentType",
    "TodoItem",
    "CognitiveAnalysis",
    "QualityDimension",
    "QualityReview",
    "EvolutionRecord",
    "MessageType",
    "AgentMessage",
    "ParallelGroup",
    "CompactionStrategy",
    "MemoryTier",
    "CompactionResult",
    # ストレージ
    "RuntimeStore",
    "EvolutionStore",
    "MemoryRuntimeStore",
    "MemoryEvolutionStore",
    # Agent池
    "BaseAgent",
    "ResearchAgent",
    "AnalysisAgent",
    "PlanningAgent",
    "ExecutionAgent",
    "ReviewAgent",
    "ReportAgent",
    "AgentPool",
    # 進捗管理
    "ProgressEvent",
    "ProgressManager",
    # コンテキスト圧縮
    "ContextCompressor",
    # 自己進化
    "SelfEvolver",
]

