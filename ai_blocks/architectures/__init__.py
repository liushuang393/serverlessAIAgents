"""
AI Blocksのアーキテクチャパターン

このモジュールは、コアコンポーネントを組み合わせた代表的なアーキテクチャパターンを提供します。
各パターンは特定のユースケースに最適化されています。
"""

# 基本クラス
from .base import Agent, ResultAggregator

# アーキテクチャパターン
from .augmented_llm import AugmentedLLM
from .prompt_chaining import PromptChain
from .agent_router import AgentRouter
from .parallel_agents import ParallelAgents
from .orchestrator_worker import OrchestratorWorker, WorkerAgent, LLMWorker, DataProcessingWorker
from .evaluator_optimizer import EvaluatorOptimizer, OptimizationStrategy
from .tool_calling import ToolCallingAgent, FunctionCallingAgent
from .memory_centric import MemoryCentricAgent, SearchStrategy

# 結果モデル
from .base import ChainResult

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
