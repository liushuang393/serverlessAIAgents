"""RAGEngine - RAG拡張Agentパターン (UnifiedRAGService版).

ナレッジベース検索拡張付きのEngine Pattern。
UnifiedRAGService を利用して検索とコンテキスト構築を行います。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, cast

from kernel.engines.base import BaseEngine, EngineConfig

if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable

    from platform.services.unified_rag import UnifiedRAGService

class RAGEngine(BaseEngine):
    """RAG拡張Agentエンジン."""

    def __init__(
        self,
        agent: type | Any,
        *,
        vector_store: str | Any | None = None,
        top_k: int = 5,
        config: EngineConfig | None = None,
    ) -> None:
        """初期化."""
        super().__init__(config)
        self._agent_cls = agent
        self._agent_instance: Any = None
        self._collection_name = vector_store if isinstance(vector_store, str) else "default"
        self._top_k = top_k
        self.rag_service = UnifiedRAGService(collection_name=self._collection_name)
        self._logger = logging.getLogger("agentflow.engines.rag")

    async def _initialize(self) -> None:
        """初期化."""
        # Agentを初期化
        if isinstance(self._agent_cls, type):
            self._agent_instance = self._agent_cls()
        else:
            self._agent_instance = self._agent_cls

        if hasattr(self._agent_instance, "initialize"):
            await self._agent_instance.initialize()
            
        await self.rag_service._ensure_connected()
        self._logger.info("RAGEngine initialized")

    def _format_context(self, documents: list[dict[str, Any]]) -> str:
        """コンテキストフォーマット."""
        context_parts = []
        for i, doc in enumerate(documents, 1):
            content = doc.get("document", str(doc))
            score = doc.get("score", "N/A")
            context_parts.append(f"[{i}] (Score: {score})\n{content}")
        return "\n\n".join(context_parts)

    async def _run_agent(self, agent: Any, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agent実行（A2AHub 経由統一呼び出し）."""
        return await self.call_agent(agent, inputs)

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """RAG実行."""
        query = inputs.get("question") or inputs.get("query") or str(inputs)
        
        # 1. 検索
        docs = await self.rag_service.retrieve(query, top_k=self._top_k)
        
        # 2. フォーマット
        context = self._format_context(docs)
        
        # 3. Agent実行
        augmented_input = {
            **inputs,
            "context": context,
            "documents": docs,
        }
        
        result = await self._run_agent(self._agent_instance, augmented_input)
        
        return {
            "answer": result.get("answer", result.get("result", result)),
            "sources": docs,
            "query": query,
        }

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        query = inputs.get("question") or inputs.get("query") or str(inputs)
        
        # 検索
        docs = await self.rag_service.retrieve(query, top_k=self._top_k)
        yield {"type": "retrieval_complete", "data": {"count": len(docs)}}
        
        context = self._format_context(docs)
        augmented_input = {**inputs, "context": context, "documents": docs}
        
        # Agent実行
        result = await self._run_agent(self._agent_instance, augmented_input)
        yield {"type": "result", "data": result}
