"""PEV (Plan-Execute-Verify) アーキテクチャ.

Hassabisの「計画・実行・検証の分離」に基づく設計：
- HierarchicalPlanner: 階層的な計画立案
- MonitoredExecutor: 監視付き実行
- ResultVerifier: 結果の検証と自己修正

設計原則:
- 計画と実行と検証を明確に分離
- 長期計画と動的再計画をサポート
- 失敗からの自動回復

使用例:
    >>> from agentflow.pev import PEVEngine
    >>>
    >>> engine = PEVEngine(
    ...     agents=[AnalyzerAgent, PlannerAgent, ExecutorAgent],
    ...     world_model=my_world_model,
    ...     max_replans=3,
    ... )
    >>>
    >>> async for event in engine.run_stream({"goal": "レポート作成"}):
    ...     if event["type"] == "plan_created":
    ...         print(f"計画: {event['plan']}")
    ...     elif event["type"] == "step_completed":
    ...         print(f"完了: {event['step']}")
    ...     elif event["type"] == "verification_failed":
    ...         print(f"検証失敗、再計画: {event['reason']}")
"""

from agentflow.pev.hierarchical_planner import (
    HierarchicalPlan,
    HierarchicalPlanner,
    PlanLevel,
    SubGoal,
)
from agentflow.pev.monitored_executor import (
    ExecutionMonitor,
    ExecutionResult,
    MonitoredExecutor,
)
from agentflow.pev.pev_engine import PEVEngine, PEVEngineConfig
from agentflow.pev.result_verifier import (
    ResultVerifier,
    VerificationResult,
    VerificationStrategy,
)


__all__ = [
    "ExecutionMonitor",
    "ExecutionResult",
    "HierarchicalPlan",
    # Hierarchical Planner
    "HierarchicalPlanner",
    # Monitored Executor
    "MonitoredExecutor",
    # PEV Engine
    "PEVEngine",
    "PEVEngineConfig",
    "PlanLevel",
    # Result Verifier
    "ResultVerifier",
    "SubGoal",
    "VerificationResult",
    "VerificationStrategy",
]

