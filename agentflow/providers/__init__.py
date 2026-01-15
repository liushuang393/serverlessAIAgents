# -*- coding: utf-8 -*-
"""統一Provider Layer（松耦合設計）.

このモジュールは、LLM/DB/VectorDB/Embedding の統一アクセスインターフェースを提供します。
Agent/サービスは具体的なプロバイダーやモデルを意識する必要がありません。

設計原則:
- 高内聚: 各Providerは関連ロジックを集約
- 松耦合: 呼び出し側は get_xxx() 関数のみを知ればよい
- 環境変数優先: 設定は環境変数から自動取得
- フォールバック: 複数プロバイダーの自動切り替え

使用例（推奨）:
    >>> from agentflow import get_llm, get_db, get_vectordb, get_embedding
    >>>
    >>> # LLM（プロバイダー不明でOK）
    >>> llm = get_llm()
    >>> response = await llm.chat([{"role": "user", "content": "こんにちは"}])
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
"""

from agentflow.providers.llm_provider import LLMProvider, get_llm, reset_llm
from agentflow.providers.db_provider import DBProvider, get_db, reset_db
from agentflow.providers.vectordb_provider import VectorDBProvider, get_vectordb, reset_vectordb
from agentflow.providers.embedding_provider import EmbeddingProvider, get_embedding, reset_embedding
from agentflow.providers.tool_provider import ToolProvider, RegisteredTool, tool
from agentflow.providers.data_provider import DataProvider
from agentflow.providers.event_provider import EventProvider

# NEW: Tool Executor（OpenAI Function Calling 互換）
from agentflow.providers.tool_executor import (
    ToolCall,
    FunctionCall,
    ToolResult,
    ToolCallStatus,
    BatchResult,
    FallbackStrategy,
    SimpleFallbackStrategy,
    SemanticFallbackStrategy,
    RetryConfig,
    RetryStrategy,
    ToolExecutor,
)

# NEW: 統一ツールプロバイダー（Skills/MCP/Builtin統合）
from agentflow.providers.unified_tool import (
    UnifiedToolProvider,
    ToolType,
    ToolStatus,
    ToolResult as UnifiedToolResult,
    ToolDefinition,
)

__all__ = [
    # LLM（推奨: get_llm() を使用）
    "get_llm",
    "reset_llm",
    "LLMProvider",
    # DB（推奨: get_db() を使用）
    "get_db",
    "reset_db",
    "DBProvider",
    # VectorDB（推奨: get_vectordb() を使用）
    "get_vectordb",
    "reset_vectordb",
    "VectorDBProvider",
    # Embedding（推奨: get_embedding() を使用）
    "get_embedding",
    "reset_embedding",
    "EmbeddingProvider",
    # Tool Provider
    "ToolProvider",
    "RegisteredTool",
    "tool",
    # NEW: Tool Executor（OpenAI互換並行実行）
    "ToolCall",
    "FunctionCall",
    "ToolResult",
    "ToolCallStatus",
    "BatchResult",
    "FallbackStrategy",
    "SimpleFallbackStrategy",
    "SemanticFallbackStrategy",
    "RetryConfig",
    "RetryStrategy",
    "ToolExecutor",
    # その他のプロバイダー
    "DataProvider",
    "EventProvider",
    # ==========================================================================
    # NEW: 統一ツールプロバイダー
    # ==========================================================================
    "UnifiedToolProvider",
    "ToolType",
    "ToolStatus",
    "UnifiedToolResult",
    "ToolDefinition",
]

