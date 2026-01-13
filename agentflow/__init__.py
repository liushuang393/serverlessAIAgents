"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow はAIエージェント開発のための軽量フレームワークです。
MCP、A2A、AG-UI プロトコルをネイティブサポートしています。

=============================================================================
公開API（推奨）
=============================================================================

Engine Pattern（メインAPI - 4種類の予定義パターン）:
    >>> from agentflow import SimpleEngine, PipelineEngine, GateEngine, RAGEngine
    >>>
    >>> # 1. SimpleEngine - 単一Agent質問応答
    >>> engine = SimpleEngine(agent=MyAgent)
    >>> result = await engine.run({"question": "こんにちは"})
    >>>
    >>> # 2. PipelineEngine - マルチステップフロー
    >>> engine = PipelineEngine(
    ...     stages=[
    ...         {"name": "gate", "agent": GateAgent, "gate": True},
    ...         {"name": "analysis", "agents": [Agent1, Agent2]},
    ...         {"name": "review", "agent": ReviewAgent, "review": True},
    ...     ],
    ...     max_revisions=2,
    ... )
    >>> async for event in engine.run_stream(inputs):
    ...     print(event)

Decorator API（最も簡単）:
    >>> from agentflow import agent, tool, AgentClient
    >>> @agent
    ... class MyAgent:
    ...     system_prompt = "親切なアシスタント"
    ...     @tool
    ...     def search(self, query: str) -> list:
    ...         return []
    >>> result = await AgentClient.get("MyAgent").invoke({"question": "..."})

松耦合Provider（推奨）:
    >>> from agentflow import get_llm, get_db, get_vectordb, get_embedding
    >>> llm = get_llm()  # 環境変数から自動検出
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])

=============================================================================
内部API（上級者向け - 直接使用は非推奨）
=============================================================================

- agentflow.flow: 低レベルフロー構築API（create_flow等）
- agentflow.patterns: デザインパターン実装
- agentflow.core: 基底クラスとユーティリティ
- agentflow.integrations: フレームワーク統合

環境変数:
    フレームワーク初期化時に .env ファイルを自動読み込み。
"""

# =============================================================================
# 環境変数の自動読み込み
# =============================================================================
from pathlib import Path

from dotenv import load_dotenv

_cwd = Path.cwd()
_env_candidates = [_cwd / ".env"] + [p / ".env" for p in _cwd.parents]
for _env_path in _env_candidates:
    if _env_path.exists():
        load_dotenv(_env_path)
        break
else:
    load_dotenv()

del _cwd, _env_candidates, _env_path

# =============================================================================
# 公開API: Engine Pattern（メインAPI）
# =============================================================================
from agentflow.engines import (
    BaseEngine,
    EngineConfig,
    SimpleEngine,
    GateEngine,
    PipelineEngine,
    RAGEngine,
)

# =============================================================================
# 公開API: Decorator API
# =============================================================================
from agentflow.agent_decorator import agent, AgentClient, get_skill, list_skills
from agentflow.providers.tool_provider import tool

# =============================================================================
# 公開API: 松耦合Provider
# =============================================================================
from agentflow.providers import (
    get_llm, reset_llm, LLMProvider,
    get_db, reset_db, DBProvider,
    get_vectordb, reset_vectordb, VectorDBProvider,
    get_embedding, reset_embedding, EmbeddingProvider,
)

# =============================================================================
# 公開API: Agent基底クラス
# =============================================================================
from agentflow.core.agent_block import AgentBlock
from agentflow.core.resilient_agent import (
    BaseDecisionAgent,
    ResilientAgent,
)

# =============================================================================
# 公開API: 例外クラス
# =============================================================================
from agentflow.core.exceptions import (
    AgentExecutionError,
    AgentFlowError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)

# =============================================================================
# 公開API: Observability
# =============================================================================
from agentflow.observability import (
    setup_logging,
    get_logger,
    LogLevel,
    setup_observability,
)

# =============================================================================
# 公開API: SSE/AG-UI（FastAPI統合用）
# =============================================================================
from agentflow.integrations.fastapi_integration import create_sse_response

# =============================================================================
# 公開API: Human-in-the-Loop (HITL)
# =============================================================================
from agentflow.hitl import (
    ApprovalManager,
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    Checkpointer,
    CheckpointData,
    Command,
    CommandType,
    HITLConfig,
    InterruptError,
    InterruptSignal,
    MemoryCheckpointer,
    create_hitl_router,
    get_checkpointer,
    interrupt,
)
from agentflow.engines.base import HITLEngineConfig

# =============================================================================
# 後方互換性のため維持（非推奨 - 将来削除予定）
# =============================================================================
from agentflow.core.engine import AgentFlowEngine  # 非推奨: enginesを使用してください
from agentflow.core.types import AgentMetadata, WorkflowConfig

# Flow API - 後方互換（内部APIとして維持）
from agentflow.flow import (
    create_flow,
    Flow,
    FlowBuilder,
    FlowContext,
    MemoryAccessor,
    FlowNode,
    AgentNode,
    GateNode,
    ParallelNode,
    ReviewNode,
    NodeType,
    NextAction,
    ReviewVerdict,
    NodeResult,
    FlowConfig,
    AgentProtocol,
)

# MCP Tool（後方互換）
from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)


__version__ = "0.2.0"

# =============================================================================
# 公開シンボル定義
# =============================================================================
__all__ = [
    # =========================================================================
    # Engine Pattern（メインAPI - 推奨）
    # =========================================================================
    "BaseEngine",
    "EngineConfig",
    "SimpleEngine",
    "GateEngine",
    "PipelineEngine",
    "RAGEngine",

    # =========================================================================
    # Decorator API（推奨）
    # =========================================================================
    "agent",
    "tool",
    "AgentClient",
    "get_skill",
    "list_skills",

    # =========================================================================
    # 松耦合Provider（推奨）
    # =========================================================================
    "get_llm",
    "reset_llm",
    "LLMProvider",
    "get_db",
    "reset_db",
    "DBProvider",
    "get_vectordb",
    "reset_vectordb",
    "VectorDBProvider",
    "get_embedding",
    "reset_embedding",
    "EmbeddingProvider",

    # =========================================================================
    # Agent基底クラス
    # =========================================================================
    "AgentBlock",
    "ResilientAgent",
    "BaseDecisionAgent",

    # =========================================================================
    # 例外クラス
    # =========================================================================
    "AgentFlowError",
    "AgentExecutionError",
    "AgentTimeoutError",
    "AgentRetryExhaustedError",
    "ProtocolError",
    "WorkflowError",
    "WorkflowNotFoundError",

    # =========================================================================
    # Observability
    # =========================================================================
    "setup_logging",
    "get_logger",
    "LogLevel",
    "setup_observability",

    # =========================================================================
    # SSE/AG-UI
    # =========================================================================
    "create_sse_response",

    # =========================================================================
    # Human-in-the-Loop (HITL)
    # =========================================================================
    "interrupt",
    "InterruptError",
    "InterruptSignal",
    "ApprovalRequest",
    "ApprovalResponse",
    "ApprovalStatus",
    "ApprovalManager",
    "Command",
    "CommandType",
    "Checkpointer",
    "CheckpointData",
    "MemoryCheckpointer",
    "get_checkpointer",
    "HITLConfig",
    "HITLEngineConfig",
    "create_hitl_router",

    # =========================================================================
    # 後方互換（非推奨）
    # =========================================================================
    "AgentFlowEngine",  # 非推奨
    "AgentMetadata",
    "WorkflowConfig",
    # Flow API（内部API - 上級者向け）
    "create_flow",
    "Flow",
    "FlowBuilder",
    "FlowContext",
    "MemoryAccessor",
    "FlowNode",
    "AgentNode",
    "GateNode",
    "ParallelNode",
    "ReviewNode",
    "NodeType",
    "NextAction",
    "ReviewVerdict",
    "NodeResult",
    "FlowConfig",
    "AgentProtocol",
    # MCP Tool
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
]
