"""AgentFlow Patterns - Agentic AI Design Patterns.

このモジュールは業界最佳実践に基づいた Agentic AI デザインパターンを提供します:
- Reflection: 自己評価と改善の反復
- Multi-Agent: 複数エージェント協調
- Coordinator: 協調パターン基類
- Supervisor: 監督者パターン
- Hierarchical: 階層パターン

設計原則:
- 簡単: AgentBlock ベース、理解しやすい
- 柔軟: WorkflowConfig で組み合わせ
- 健壮: エラーハンドリングと fallback
- 独立: 外部フレームワーク不要

参考（思想のみ吸収）:
- Analytics Vidhya: Reflection Pattern
- Azure Architecture: AI Agent Orchestration Patterns
- Anthropic: Building Effective Agents
- LangGraph: Multi-agent Supervisor / Hierarchical Teams
"""

# Coordinator Base
from agentflow.patterns.coordinator import (
    CoordinationPattern,
    CoordinatorBase,
    CoordinatorRegistry,
)

# Reflection Pattern
from agentflow.patterns.reflection import (
    ImproverAgent,
    ReflectionLoop,
    ReflectionResult,
    ReflectionWorkflow,
    ReflectorAgent,
)

# Multi-Agent Pattern
from agentflow.patterns.multi_agent import (
    AgentCoordinator,
    AgentRouter,
    MultiAgentWorkflow,
    SharedContext,
)

# Supervisor Pattern
from agentflow.patterns.supervisor import (
    SupervisorCoordinator,
    SupervisorDecision,
)

# Hierarchical Pattern
from agentflow.patterns.hierarchical import (
    HierarchicalCoordinator,
    SubTask,
)

# Progress Emitter
from agentflow.patterns.progress_emitter import (
    AgentMeta,
    ProgressEmitter,
)

# Agent Pipeline
from agentflow.patterns.agent_pipeline import (
    AgentConfig,
    AgentPipeline,
    AgentProtocol,
    PipelineConfig,
    RevisionRequest,
)

__all__ = [
    # Coordinator Base
    "CoordinationPattern",
    "CoordinatorBase",
    "CoordinatorRegistry",
    # Reflection Pattern
    "ImproverAgent",
    "ReflectionLoop",
    "ReflectionResult",
    "ReflectionWorkflow",
    "ReflectorAgent",
    # Multi-Agent Pattern
    "AgentCoordinator",
    "AgentRouter",
    "MultiAgentWorkflow",
    "SharedContext",
    # Supervisor Pattern
    "SupervisorCoordinator",
    "SupervisorDecision",
    # Hierarchical Pattern
    "HierarchicalCoordinator",
    "SubTask",
    # Progress Emitter
    "AgentMeta",
    "ProgressEmitter",
    # Agent Pipeline
    "AgentConfig",
    "AgentPipeline",
    "AgentProtocol",
    "PipelineConfig",
    "RevisionRequest",
]

