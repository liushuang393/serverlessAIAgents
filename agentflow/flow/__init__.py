"""AgentFlowフロー編成フレームワーク.

シンプルなチェーンAPIを提供し、複雑なAgentフローを構築・実行する。

コア機能:
- チェーン構築：gate/then/parallel/reviewを使用してフローを構築
- 条件分岐：Gateノードが条件インターセプトをサポート
- ロールバック機構：ReviewノードがREVISEロールバックをサポート
- 進捗追跡：AG-UIプロトコルイベントを自動発行

Quick Start:
    >>> from agentflow.flow import create_flow
    >>>
    >>> # シンプルなフロー
    >>> flow = (
    ...     create_flow("simple-flow")
    ...     .then(MyAgent1, MyAgent2)
    ...     .build()
    ... )
    >>> result = await flow.run({"input": "..."})

    >>> # 複雑なフロー（GateとReview付き）
    >>> flow = (
    ...     create_flow("decision-engine")
    ...     .gate(GatekeeperAgent, check=lambda r: r["is_acceptable"])
    ...     .then(ClarificationAgent)
    ...     .then(DaoAgent, FaAgent, ShuAgent, QiAgent)
    ...     .review(
    ...         ReviewAgent,
    ...         retry_from="dao",
    ...         max_revisions=2,
    ...         on_pass=lambda ctx: generate_report(ctx)
    ...     )
    ...     .build()
    ... )
    >>>
    >>> # ストリーム実行
    >>> async for event in flow.run_stream(inputs):
    ...     print(f"{event['type']}: {event.get('node_name', '')}")

モジュール構造:
- types.py: 型定義
- context.py: 実行コンテキスト
- nodes.py: ノード型
- graph.py: フローグラフ
- executor.py: 実行エンジン
- progress.py: 進捗追跡
- builder.py: チェーンビルダー
- flow.py: Flow実行クラス
"""

# コア型
# ビルダーとFlow
from agentflow.flow.builder import FlowBuilder, create_flow

# コンテキストと実行
from agentflow.flow.context import FlowContext
from agentflow.flow.executor import FlowExecutor
from agentflow.flow.flow import Flow, MemoryAccessor
from agentflow.flow.graph import FlowGraph

# ノード型
from agentflow.flow.nodes import (
    AgentNode,
    FlowNode,
    GateNode,
    ParallelNode,
    ReviewNode,
)
from agentflow.flow.progress import ProgressTracker

# サンドボックスノード（Daytonaスタイル）
from agentflow.flow.sandbox_node import (
    SandboxNode,
    WorkspaceNode,
)

# サービスノード（Studio統合用）
from agentflow.flow.service_nodes import (
    ChartNode,
    FAQNode,
    RAGNode,
    ServiceNode,
    ServiceNodeType,
    SuggestionNode,
    Text2SQLNode,
    get_all_service_node_definitions,
)
from agentflow.flow.types import (
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
    # コアクラス
    "Flow",
    "FlowBuilder",
    "FlowConfig",
    "FlowContext",
    "FlowExecutor",
    # 高度な用法
    "FlowGraph",
    # ノード型
    "FlowNode",
    "GateNode",
    "InitializableAgent",
    "MemoryAccessor",
    "NextAction",
    "NodeResult",
    # 型
    "NodeType",
    "ParallelNode",
    "ProgressTracker",
    "RAGNode",
    "ReviewNode",
    "ReviewVerdict",
    # サンドボックスノード
    "SandboxNode",
    "ServiceNode",
    # サービスノード
    "ServiceNodeType",
    "SuggestionNode",
    "Text2SQLNode",
    "WorkspaceNode",
    # エントリ関数
    "create_flow",
    "get_all_service_node_definitions",
]
