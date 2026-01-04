"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow is a lightweight framework for building AI agents with native support
for MCP, A2A, and AG-UI protocols. Built on top of PocketFlow, it provides a
simple yet powerful way to create, compose, and deploy AI agents.

環境変数:
    フレームワーク初期化時に .env ファイルを自動読み込み。
    プロジェクトルートに .env を配置してください。

Quick Start (推奨):
    # 方式1: デコレータ（最も簡単）
    >>> from agentflow import agent, tool, AgentClient
    >>> @agent
    ... class MyAgent:
    ...     system_prompt = "親切なアシスタント"
    ...     @tool
    ...     def search(self, query: str) -> list:
    ...         return []
    >>> result = await AgentClient.get("MyAgent").invoke({"question": "..."})

    # 方式2: Flow（複数Agent協調）
    >>> from agentflow import create_flow, Flow
    >>> flow = create_flow([Agent1(), Agent2()])
    >>> result = await flow.run({"task": "..."})

    # ストリーム実行（SSE用）
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)

    # 記憶システム
    >>> flow.memory.remember("key", "value")
    >>> value = flow.memory.recall("key")

松耦合アクセス（推奨）:
    >>> from agentflow import get_llm, get_db, get_vectordb, get_embedding
    >>>
    >>> # LLM（プロバイダー/モデル不明でOK - 環境変数から自動検出）
    >>> llm = get_llm()
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])
    >>>
    >>> # DB（Supabase/PostgreSQL/SQLite 自動検出）
    >>> db = get_db()
    >>> users = await db.select("users", filters={"active": True})
    >>>
    >>> # VectorDB（Pinecone/Qdrant/ChromaDB 自動検出）
    >>> vdb = get_vectordb()
    >>> results = await vdb.search("query text", top_k=5)
    >>>
    >>> # Embedding（OpenAI/SentenceTransformer 自動検出）
    >>> emb = get_embedding()
    >>> vector = await emb.embed_text("Hello world")

Advanced:
    >>> from agentflow import AgentFlowEngine
    >>> engine = AgentFlowEngine()
    >>> result = await engine.execute("my-workflow", {"input": "test"})

New Modules (v0.3.0):
    >>> # Knowledge Base / RAG
    >>> from agentflow.knowledge import RAGPipeline, use_vector_search, use_rag
    >>>
    >>> # Observability
    >>> from agentflow.observability import setup_observability, get_logger, get_tracer
    >>>
    >>> # Security
    >>> from agentflow.security import APIKeyManager, RateLimiter, create_auth_middleware
    >>>
    >>> # Testing
    >>> from agentflow.testing import MockLLMProvider, AgentTestCase
    >>>
    >>> # Deploy
    >>> from agentflow.deploy import generate_all, generate_dockerfile
"""

# =============================================================================
# 環境変数の自動読み込み（フレームワーク初期化時に1回だけ実行）
# =============================================================================
from pathlib import Path

from dotenv import load_dotenv

# .env ファイルを検索して読み込み
# 優先順位: カレントディレクトリ → 上位ディレクトリを順に検索
_cwd = Path.cwd()
_env_candidates = [_cwd / ".env"] + [p / ".env" for p in _cwd.parents]
for _env_path in _env_candidates:
    if _env_path.exists():
        load_dotenv(_env_path)
        break
else:
    # 見つからない場合はデフォルト動作（python-dotenv が自動検索）
    load_dotenv()

del _cwd, _env_candidates, _env_path  # クリーンアップ
# =============================================================================

from agentflow.core.agent_block import AgentBlock
from agentflow.core.engine import AgentFlowEngine
from agentflow.core.exceptions import (
    AgentExecutionError,
    AgentFlowError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)
from agentflow.core.resilient_agent import (
    BaseDecisionAgent,
    ResilientAgent,
)
from agentflow.core.types import AgentMetadata, WorkflowConfig

# MCP Tool (v0.3.0)
from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)

# Quick API - 統一入口（推奨）
from agentflow.quick import Flow, FlowWrapper, create_api_endpoint, create_flow

# Decorator API（最も簡単）
from agentflow.agent_decorator import agent, AgentClient, get_skill, list_skills
from agentflow.providers.tool_provider import tool

# 松耦合 Provider（推奨）
from agentflow.providers import (
    # LLM
    get_llm, reset_llm, LLMProvider,
    # DB
    get_db, reset_db, DBProvider,
    # VectorDB
    get_vectordb, reset_vectordb, VectorDBProvider,
    # Embedding
    get_embedding, reset_embedding, EmbeddingProvider,
)

# SSE/AG-UI サポート
from agentflow.integrations.fastapi_integration import create_sse_response

# Observability（日志・监控）
from agentflow.observability import (
    setup_logging,
    get_logger,
    LogLevel,
    setup_observability,
)


__version__ = "0.2.0"
__all__ = [
    # 松耦合アクセス（推奨）
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
    # Decorator API（最も簡単）
    "agent",
    "tool",
    "AgentClient",
    # Skills API（Claude Code Skills 互換）
    "get_skill",
    "list_skills",
    # Quick API
    "create_flow",
    "Flow",
    "FlowWrapper",  # 後方互換
    "create_api_endpoint",
    # Agent 基底クラス（v0.3.0）
    "AgentBlock",
    "ResilientAgent",
    "BaseDecisionAgent",  # 後方互換
    # MCP Tool（v0.3.0）
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
    # Core API
    "AgentFlowEngine",
    "AgentFlowError",
    "AgentExecutionError",
    "AgentTimeoutError",
    "AgentRetryExhaustedError",
    "AgentMetadata",
    "ProtocolError",
    "WorkflowConfig",
    "WorkflowError",
    "WorkflowNotFoundError",
    # SSE/AG-UI
    "create_sse_response",
    # Observability（日志・监控）
    "setup_logging",
    "get_logger",
    "LogLevel",
    "setup_observability",
]
