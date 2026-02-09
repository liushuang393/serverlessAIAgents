"""RLM (Recursive Language Model) モジュール.

長いコンテキスト（>15K tokens）を外部変数として扱い、
選択的読み取りで効率的に処理するRLM機能を提供。

設計思想:
- Context as Variable: 長文を変数としてLLMの外に保持
- Selective Reading: 必要な部分だけを選択的に読み取り
- Budget-First: Token予算を厳格に管理
- Augment, Don't Replace: 既存のRAG/要約機能を拡張

コンポーネント:
- RLMConfig: 設定パラメータ
- RLMController: メインオーケストレーションループ
- TaskRouter: RLM発動判定
- ContextStore: コンテキスト外部保存
- ContextOps: コンテキスト操作ツール
- SubCallManager: ネストLLM呼び出し管理
- Workspace: 中間結果の可変空間

使用例:
    >>> from agentflow.context.rlm import RLMConfig, RLMController, TaskRouter
    >>>
    >>> # 設定
    >>> config = RLMConfig(
    ...     activation_threshold=15_000,
    ...     max_iterations=20,
    ... )
    >>>
    >>> # 発動判定
    >>> router = TaskRouter(config)
    >>> decision = router.should_activate(
    ...     query="認証要件は？",
    ...     long_inputs=[large_document],
    ... )
    >>>
    >>> if decision.should_activate:
    ...     # RLM実行
    ...     controller = RLMController(config, llm_client)
    ...     result = await controller.run(
    ...         query="認証要件は？",
    ...         long_inputs=[large_document],
    ...     )
    ...     print(result.answer)
"""

# Configuration
from agentflow.context.rlm.config import (
    ActionType,
    RLMConfig,
    StopReason,
    SubCallBudget,
)

# Context Operations
from agentflow.context.rlm.context_ops import (
    ContextOps,
    create_context_ops_prompt,
)

# Context Store
from agentflow.context.rlm.context_store import (
    ContextHandle,
    ContextStore,
    StructureInfo,
)

# Controller
from agentflow.context.rlm.controller import (
    RLMController,
    RLMResult,
)

# Events
from agentflow.context.rlm.events import (
    RLMActionEvent,
    RLMCompleteEvent,
    RLMErrorEvent,
    RLMEvent,
    RLMEventEmitter,
    RLMEventType,
    RLMIterationEvent,
    RLMStartEvent,
    RLMSubcallEvent,
    RLMWorkspaceUpdateEvent,
)

# SubCall Manager
from agentflow.context.rlm.subcall import (
    LLMClientProtocol,
    SubCallManager,
    SubCallResult,
)

# Task Router
from agentflow.context.rlm.task_router import (
    ActivationDecision,
    TaskRouter,
)

# Workspace
from agentflow.context.rlm.workspace import (
    VariableType,
    Workspace,
    WorkspaceVariable,
)


__all__ = [
    "ActionType",
    "ActivationDecision",
    "ContextHandle",
    # =========================================================================
    # Context Operations
    # =========================================================================
    "ContextOps",
    # =========================================================================
    # Context Store
    # =========================================================================
    "ContextStore",
    "LLMClientProtocol",
    "RLMActionEvent",
    "RLMCompleteEvent",
    # =========================================================================
    # Configuration
    # =========================================================================
    "RLMConfig",
    # =========================================================================
    # Main Components
    # =========================================================================
    "RLMController",
    "RLMErrorEvent",
    # =========================================================================
    # Events
    # =========================================================================
    "RLMEvent",
    "RLMEventEmitter",
    "RLMEventType",
    "RLMIterationEvent",
    "RLMResult",
    "RLMStartEvent",
    "RLMSubcallEvent",
    "RLMWorkspaceUpdateEvent",
    "StopReason",
    "StructureInfo",
    "SubCallBudget",
    # =========================================================================
    # SubCall Manager
    # =========================================================================
    "SubCallManager",
    "SubCallResult",
    "TaskRouter",
    "VariableType",
    # =========================================================================
    # Workspace
    # =========================================================================
    "Workspace",
    "WorkspaceVariable",
    "create_context_ops_prompt",
]
