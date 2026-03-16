"""Layer 3 の kernel 公開 API.

循環インポートを回避するため遅延インポートを使用。
"""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from kernel.events import EventSink, NoOpEventSink
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
    from kernel.plugins import KernelPluginRegistry
    from kernel.runtime import RuntimeContext, get_runtime_context, set_runtime_context, use_runtime_context
    from kernel.tools import KernelToolExecutor


__all__ = [
    "AgentNode",
    "EventSink",
    "Flow",
    "FlowBuilder",
    "FlowContext",
    "FlowNode",
    "GateNode",
    "KernelPluginRegistry",
    "KernelToolExecutor",
    "NoOpEventSink",
    "ParallelNode",
    "ProgressTracker",
    "ReviewNode",
    "RuntimeContext",
    "create_flow",
    "get_runtime_context",
    "set_runtime_context",
    "use_runtime_context",
]


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    _module_map = {
        "EventSink": "kernel.events",
        "NoOpEventSink": "kernel.events",
        "AgentNode": "kernel.flow",
        "Flow": "kernel.flow",
        "FlowBuilder": "kernel.flow",
        "FlowContext": "kernel.flow",
        "FlowNode": "kernel.flow",
        "GateNode": "kernel.flow",
        "ParallelNode": "kernel.flow",
        "ProgressTracker": "kernel.flow",
        "ReviewNode": "kernel.flow",
        "create_flow": "kernel.flow",
        "KernelPluginRegistry": "kernel.plugins",
        "RuntimeContext": "kernel.runtime",
        "get_runtime_context": "kernel.runtime",
        "set_runtime_context": "kernel.runtime",
        "use_runtime_context": "kernel.runtime",
        "KernelToolExecutor": "kernel.tools",
    }
    if name in _module_map:
        mod = importlib.import_module(_module_map[name])
        return getattr(mod, name)
    msg = f"module 'kernel' has no attribute {name!r}"
    raise AttributeError(msg)
