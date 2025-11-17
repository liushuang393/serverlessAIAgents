"""AgentFlow Patterns - Agentic AI Design Patterns.

このモジュールは業界最佳実践に基づいた Agentic AI デザインパターンを提供します:
- Reflection: 自己評価と改善の反復
- Multi-Agent: 複数エージェント協調

設計原則:
- 簡単: AgentBlock ベース、理解しやすい
- 柔軟: WorkflowConfig で組み合わせ
- 健壮: エラーハンドリングと fallback
- 独立: 外部フレームワーク不要

参考（思想のみ吸収）:
- Analytics Vidhya: Reflection Pattern
- Azure Architecture: AI Agent Orchestration Patterns
- Anthropic: Building Effective Agents
"""

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

__all__ = [
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
]

