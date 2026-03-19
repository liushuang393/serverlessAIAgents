"""Layer 3 の kernel 公開 API.

循環インポートを回避するため遅延インポートを使用。
"""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from kernel.events import EventSink, EventType, NoOpEventSink
    from kernel.executor import ExecutorAgent, ExecutorConfig, StepResult
    from kernel.flow import (
        AgentNode,
        Flow,
        FlowBuilder,
        FlowContext,
        FlowNode,
        GateNode,
        ParallelNode,
        ProgressTracker,
        ReviewNode,
        create_flow,
    )
    from kernel.planner import ExecutionPlan, PlannerAgent, PlannerConfig, PlanStep, StepStatus, StepType
    from kernel.plugins import KernelPluginRegistry
    from kernel.reporter import (
        ChartData,
        ExecutiveSummary,
        OutputFormat,
        ReportBuilder,
        ReportSection,
        SectionedReportBuilder,
        SimpleReportBuilder,
        create_bar_chart,
        create_line_chart,
        create_pie_chart,
    )
    from kernel.reviewer import ResultVerifier, VerificationResult, VerificationStrategy
    from kernel.router import Intent, IntentCategory, IntentRouter, RouterConfig
    from kernel.runtime import RuntimeContext, get_runtime_context, set_runtime_context, use_runtime_context
    from kernel.agents.resilient_agent import ResilientAgent
    from control_plane.api.websocket_hub import WebSocketHub
    from kernel.tools import KernelToolExecutor


__all__ = [
    # agents
    "ResilientAgent",
    # websocket
    "WebSocketHub",
    # events
    "EventSink",
    "EventType",
    "NoOpEventSink",
    # executor
    "ExecutorAgent",
    "ExecutorConfig",
    "StepResult",
    # flow
    "AgentNode",
    "Flow",
    "FlowWrapper",
    "FlowBuilder",
    "FlowContext",
    "FlowNode",
    "GateNode",
    "ParallelNode",
    "ProgressTracker",
    "ReviewNode",
    "create_flow",
    # planner
    "ExecutionPlan",
    "PlannerAgent",
    "PlannerConfig",
    "PlanStep",
    "StepStatus",
    "StepType",
    # plugins
    "KernelPluginRegistry",
    # reporter
    "ChartData",
    "ExecutiveSummary",
    "OutputFormat",
    "ReportBuilder",
    "ReportSection",
    "SectionedReportBuilder",
    "SimpleReportBuilder",
    "create_bar_chart",
    "create_line_chart",
    "create_pie_chart",
    # reviewer
    "ResultVerifier",
    "VerificationResult",
    "VerificationStrategy",
    # router
    "Intent",
    "IntentCategory",
    "IntentRouter",
    "RouterConfig",
    # runtime
    "RuntimeContext",
    "get_runtime_context",
    "set_runtime_context",
    "use_runtime_context",
    # tools
    "KernelToolExecutor",
]


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    _module_map = {
        "EventSink": "kernel.events",
        "EventType": "kernel.events",
        "NoOpEventSink": "kernel.events",
        "ExecutorAgent": "kernel.executor",
        "ExecutorConfig": "kernel.executor",
        "StepResult": "kernel.executor",
        "AgentNode": "kernel.flow",
        "Flow": "kernel.flow",
        "FlowWrapper": "kernel.flow",
        "FlowBuilder": "kernel.flow",
        "FlowContext": "kernel.flow",
        "FlowNode": "kernel.flow",
        "GateNode": "kernel.flow",
        "ParallelNode": "kernel.flow",
        "ProgressTracker": "kernel.flow",
        "ReviewNode": "kernel.flow",
        "create_flow": "kernel.flow",
        "ExecutionPlan": "kernel.planner",
        "PlannerAgent": "kernel.planner",
        "PlannerConfig": "kernel.planner",
        "PlanStep": "kernel.planner",
        "StepStatus": "kernel.planner",
        "StepType": "kernel.planner",
        "KernelPluginRegistry": "kernel.plugins",
        "ChartData": "kernel.reporter",
        "ExecutiveSummary": "kernel.reporter",
        "OutputFormat": "kernel.reporter",
        "ReportBuilder": "kernel.reporter",
        "ReportSection": "kernel.reporter",
        "SectionedReportBuilder": "kernel.reporter",
        "SimpleReportBuilder": "kernel.reporter",
        "create_bar_chart": "kernel.reporter",
        "create_line_chart": "kernel.reporter",
        "create_pie_chart": "kernel.reporter",
        "ResultVerifier": "kernel.reviewer",
        "VerificationResult": "kernel.reviewer",
        "VerificationStrategy": "kernel.reviewer",
        "Intent": "kernel.router",
        "IntentCategory": "kernel.router",
        "IntentRouter": "kernel.router",
        "RouterConfig": "kernel.router",
        "RuntimeContext": "kernel.runtime",
        "get_runtime_context": "kernel.runtime",
        "set_runtime_context": "kernel.runtime",
        "use_runtime_context": "kernel.runtime",
        "KernelToolExecutor": "kernel.tools",
        # agents
        "ResilientAgent": "kernel.agents.resilient_agent",
        "WebSocketHub": "control_plane.api.websocket_hub",
        # LLM ファクトリ
        "get_llm": "infrastructure.providers.llm_provider",
        # デコレーター
        "agent": "kernel.agent_decorator",
        "AgentClient": "kernel.agent_decorator",
        "RegisteredAgent": "kernel.agent_decorator",
        "get_skill": "kernel.agent_decorator",
        "list_skills": "kernel.agent_decorator",
        # tool デコレーター（infrastructure経由）
        "tool": "infrastructure.providers.tool_provider",
        # ツール定義
        "ToolDefinition": "kernel.tools.tool_definition",
        "ToolSource": "kernel.tools.tool_definition",
        "ToolRegistry": "kernel.tools.tool_registry",
        "ToolDiscoveryService": "kernel.tools.tool_discovery",
        "ToolBinder": "kernel.tools.tool_binding",
        "BoundTools": "kernel.tools.tool_binding",
        # 能力仕様
        "AgentCapabilitySpec": "kernel.core.capability_spec",
        "CapabilityRequirement": "kernel.core.capability_spec",
        # Agent レジストリ
        "AgentRegistry": "kernel.agents.agent_registry",
        "get_global_agent_registry": "kernel.agents.agent_registry",
        "get_global_tool_registry": "kernel.tools.tool_registry",
    }
    if name in _module_map:
        mod = importlib.import_module(_module_map[name])
        return getattr(mod, name)
    msg = f"module 'kernel' has no attribute {name!r}"
    raise AttributeError(msg)
