"""Layer 3 (kernel) の Flow モジュール — 実装本体.

フロー編成フレームワークのコアロジックを提供する。
legacy flow surface は本モジュールからの re-export スタブ。
"""

# コア型
from kernel.flow.builder import FlowBuilder, create_flow
from kernel.flow.context import FlowContext, FlowMetadata
from kernel.flow.executor import FlowExecutor
from kernel.flow.flow import Flow, MemoryAccessor


# 後方互換エイリアス
FlowWrapper = Flow
from kernel.flow.graph import FlowGraph
from kernel.flow.nodes import (
    AgentNode,
    FlowNode,
    GateNode,
    ParallelNode,
    ReviewNode,
)
from kernel.flow.progress import ProgressTracker
from kernel.flow.sandbox_node import (
    SandboxNode,
    WorkspaceNode,
)
from kernel.flow.service_nodes import (
    ChartNode,
    FAQNode,
    RAGNode,
    ServiceNode,
    ServiceNodeType,
    SuggestionNode,
    Text2SQLNode,
    get_all_service_node_definitions,
)
from kernel.flow.types import (
    AgentProtocol,
    CleanableAgent,
    FlowConfig,
    InitializableAgent,
    NextAction,
    NodeResult,
    NodeType,
    ReviewVerdict,
)


__all__ = [
    "AgentNode",
    "AgentProtocol",
    "ChartNode",
    "CleanableAgent",
    "FAQNode",
    "Flow",
    "FlowBuilder",
    "FlowConfig",
    "FlowContext",
    "FlowExecutor",
    "FlowGraph",
    "FlowMetadata",
    "FlowNode",
    "FlowWrapper",
    "GateNode",
    "InitializableAgent",
    "MemoryAccessor",
    "NextAction",
    "NodeResult",
    "NodeType",
    "ParallelNode",
    "ProgressTracker",
    "RAGNode",
    "ReviewNode",
    "ReviewVerdict",
    "SandboxNode",
    "ServiceNode",
    "ServiceNodeType",
    "SuggestionNode",
    "Text2SQLNode",
    "WorkspaceNode",
    "create_flow",
    "get_all_service_node_definitions",
]
