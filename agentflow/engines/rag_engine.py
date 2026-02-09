"""RAGEngine - RAG拡張Agentパターン.

ナレッジベース検索拡張付きのEngine Pattern、以下に適用：
- 企業ナレッジベース質問応答
- ドキュメント検索 + AI回答
- コンテキスト拡張インテリジェントアシスタント

フロー: Query → RAG検索 → Agent（コンテキスト付き）→ Response

使用例:
    >>> from agentflow.engines import RAGEngine
    >>>
    >>> engine = RAGEngine(
    ...     agent=KnowledgeAgent,
    ...     vector_store="company_docs",
    ...     top_k=5,
    ... )
    >>> result = await engine.run({"question": "会社の休暇ポリシーは何ですか？"})
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.engines.base import BaseEngine, EngineConfig


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable


class RAGEngine(BaseEngine):
    """RAG拡張Agentエンジン.

    特徴：
    - 自動RAG検索
    - 検索結果をAgentコンテキストに注入
    - 複数のベクトルデータベースをサポート

    Attributes:
        agent: メインAgent（拡張後のクエリを処理）
        vector_store: ベクトルストア名またはインスタンス
        top_k: 検索Top-K結果
        retriever: カスタムリトリーバー
    """

    def __init__(
        self,
        agent: type | Any,
        *,
        vector_store: str | Any | None = None,
        top_k: int = 5,
        chunk_size: int = 500,
        retriever: Callable[[str], list[dict[str, Any]]] | None = None,
        context_template: str | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        """RAGEngineを初期化.

        Args:
            agent: Agentクラスまたはインスタンス
            vector_store: ベクトルストア名またはインスタンス
            top_k: 検索数
            chunk_size: チャンクサイズ
            retriever: カスタム検索関数
            context_template: コンテキストテンプレート
            config: Engine設定
        """
        super().__init__(config)
        self._agent_cls = agent
        self._agent_instance: Any = None
        self._vector_store = vector_store
        self._top_k = top_k
        self._chunk_size = chunk_size
        self._retriever = retriever
        self._context_template = context_template or self._default_template()
        self._rag_pipeline: Any = None
        self._logger = logging.getLogger("agentflow.engines.rag")

    def _default_template(self) -> str:
        """デフォルトコンテキストテンプレート."""
        return """以下の参考資料に基づいて質問に回答してください：

参考資料：
{context}

質問：{question}

参考資料に基づいて正確な回答を提供してください。参考資料に関連情報がない場合は、その旨を説明してください。"""

    async def _initialize(self) -> None:
        """AgentとRAGコンポーネントを初期化."""
        # Agentを初期化
        if isinstance(self._agent_cls, type):
            self._agent_instance = self._agent_cls()
        else:
            self._agent_instance = self._agent_cls

        if hasattr(self._agent_instance, "initialize"):
            await self._agent_instance.initialize()

        # RAG Pipelineを初期化（カスタムretrieverがない場合）
        if not self._retriever and self._vector_store:
            try:
                from agentflow.knowledge.rag_pipeline import RAGPipeline
                self._rag_pipeline = RAGPipeline(
                    vector_store=self._vector_store,
                    top_k=self._top_k,
                )
            except ImportError:
                self._logger.warning("RAGPipeline not available, using mock retriever")
                self._retriever = self._mock_retriever

        self._logger.info("RAGEngine initialized")

    def _mock_retriever(self, query: str) -> list[dict[str, Any]]:
        """Mockリトリーバー（テスト用）."""
        return [{"content": f"Mock result for: {query}", "score": 0.9}]

    async def _retrieve(self, query: str) -> list[dict[str, Any]]:
        """検索を実行."""
        if self._retriever:
            result = self._retriever(query)
            # 同期と非同期retrieverをサポート
            if hasattr(result, "__await__"):
                return await result
            return result
        if self._rag_pipeline:
            return await self._rag_pipeline.retrieve(query)
        return self._mock_retriever(query)

    def _format_context(self, documents: list[dict[str, Any]]) -> str:
        """検索結果をコンテキストとしてフォーマット."""
        context_parts = []
        for i, doc in enumerate(documents, 1):
            content = doc.get("content", doc.get("text", str(doc)))
            score = doc.get("score", "N/A")
            context_parts.append(f"[{i}] (関連度: {score})\n{content}")
        return "\n\n".join(context_parts)

    async def _run_agent(self, agent: Any, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agentを実行."""
        if hasattr(agent, "run"):
            result = await agent.run(inputs)
        elif hasattr(agent, "invoke"):
            result = await agent.invoke(inputs)
        elif hasattr(agent, "process"):
            result = await agent.process(inputs)
        else:
            msg = f"Agent {agent} has no run/invoke/process method"
            raise AttributeError(msg)

        if isinstance(result, dict):
            return result
        if hasattr(result, "model_dump"):
            return result.model_dump()
        return {"result": result}

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """RAG + Agentフローを実行."""
        # クエリを取得
        query = inputs.get("question") or inputs.get("query") or str(inputs)

        # Step 1: RAG検索
        self._logger.info(f"Retrieving for query: {query[:50]}...")
        documents = await self._retrieve(query)
        self._logger.info(f"Retrieved {len(documents)} documents")

        # Step 2: コンテキストをフォーマット
        context = self._format_context(documents)

        # Step 3: 拡張入力を構築
        augmented_input = {
            **inputs,
            "context": context,
            "documents": documents,
            "augmented_prompt": self._context_template.format(
                context=context, question=query
            ),
        }

        # Step 4: Agentを実行
        result = await self._run_agent(self._agent_instance, augmented_input)

        return {
            "answer": result.get("answer", result.get("result", result)),
            "sources": documents,
            "query": query,
        }

    async def _execute_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """RAG + Agentをストリーム実行."""
        query = inputs.get("question") or inputs.get("query") or str(inputs)

        # 検索開始
        if event := self._emit_node_start("rag_retrieval"):
            yield event

        documents = await self._retrieve(query)

        # 検索完了
        if event := self._emit_node_complete("rag_retrieval", {"count": len(documents)}):
            yield event

        yield {
            "type": "retrieval_complete",
            "data": {"document_count": len(documents)},
        }

        # コンテキストをフォーマット
        context = self._format_context(documents)
        augmented_input = {
            **inputs,
            "context": context,
            "documents": documents,
            "augmented_prompt": self._context_template.format(
                context=context, question=query
            ),
        }

        # Agent実行
        agent_name = getattr(
            self._agent_instance, "name", self._agent_instance.__class__.__name__
        )
        if event := self._emit_node_start(agent_name):
            yield event

        result = await self._run_agent(self._agent_instance, augmented_input)

        if event := self._emit_node_complete(agent_name, result):
            yield event

        yield {
            "type": "result",
            "data": {
                "answer": result.get("answer", result.get("result", result)),
                "sources": documents,
                "query": query,
            },
        }

