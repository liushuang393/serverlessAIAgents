"""
AI Blocksのアーキテクチャパターン

このモジュールは、コアコンポーネントを組み合わせた代表的なアーキテクチャパターンを提供します。
各パターンは特定のユースケースに最適化されています。
"""

from .agent_router import AgentRouter

# アーキテクチャパターン
from .augmented_llm import AugmentedLLM

# 結果モデル
# 基本クラス
from .base import Agent, ChainResult, ResultAggregator
from .evaluator_optimizer import EvaluatorOptimizer, OptimizationStrategy
from .memory_centric import MemoryCentricAgent, SearchStrategy
from .orchestrator_worker import (
    DataProcessingWorker,
    LLMWorker,
    OrchestratorWorker,
    WorkerAgent,
)
from .parallel_agents import ParallelAgents
from .prompt_chaining import PromptChain
from .tool_calling import FunctionCallingAgent, ToolCallingAgent

__all__ = [
    # 基本クラス
    "Agent",
    "ResultAggregator",
    # アーキテクチャパターン
    "AugmentedLLM",
    "PromptChain",
    "AgentRouter",
    "ParallelAgents",
    "OrchestratorWorker",
    "WorkerAgent",
    "LLMWorker",
    "DataProcessingWorker",
    "EvaluatorOptimizer",
    "OptimizationStrategy",
    "ToolCallingAgent",
    "FunctionCallingAgent",
    "MemoryCentricAgent",
    "SearchStrategy",
    # 結果モデル
    "ChainResult",
]
