"""deep_agent パッケージ — DeepAgent パターンの統一エントリポイント."""

from kernel.patterns.deep_agent.da_compressor import ContextCompressor
from kernel.patterns.deep_agent.da_coordinator import DeepAgentCoordinator
from kernel.patterns.deep_agent.da_dynamic import ConversationManager, DynamicAgent, Evolver
from kernel.patterns.deep_agent.da_evolver import SelfEvolver
from kernel.patterns.deep_agent.da_models import (
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
)
from kernel.patterns.deep_agent.da_pool import AgentPool
from kernel.patterns.deep_agent.da_progress import ProgressManager
from kernel.patterns.deep_agent.da_stores import MemoryEvolutionStore, MemoryRuntimeStore

__all__ = [
    "AgentMessage",
    "AgentPool",
    "AgentType",
    "CognitiveAnalysis",
    "CompactionStrategy",
    "ContextCompressor",
    "ConversationManager",
    "DeepAgentCoordinator",
    "DynamicAgent",
    "EvolutionRecord",
    "Evolver",
    "MemoryEvolutionStore",
    "MemoryRuntimeStore",
    "MessageType",
    "ParallelGroup",
    "ProgressManager",
    "QualityDimension",
    "QualityReview",
    "SelfEvolver",
    "TaskStatus",
    "TodoItem",
]
