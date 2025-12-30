"""AgentFlow - Lightweight AI Agent Development Framework.

AgentFlow is a lightweight framework for building AI agents with native support
for MCP, A2A, and AG-UI protocols. Built on top of PocketFlow, it provides a
simple yet powerful way to create, compose, and deploy AI agents.

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
"""

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.exceptions import (
    AgentFlowError,
    ProtocolError,
    WorkflowError,
    WorkflowNotFoundError,
)
from agentflow.core.types import AgentMetadata, WorkflowConfig

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
    # Core API
    "AgentFlowEngine",
    "AgentFlowError",
    "AgentMetadata",
    "ProtocolError",
    "WorkflowConfig",
    "WorkflowError",
    "WorkflowNotFoundError",
]
