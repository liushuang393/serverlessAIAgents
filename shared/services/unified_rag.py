"""UnifiedRAGService - 統合RAGサービス.

既存の RagService と RAGPipeline を統合し、
ハイブリッド検索、リランキング、コンテキスト管理を統一インターフェースで提供します。
"""

import logging
from typing import Any

from infrastructure.llm.providers import (
    EmbeddingProvider,
    LLMProvider,
    VectorDBProvider,
    get_embedding,
    get_llm,
    get_vectordb,
)


logger = logging.getLogger(__name__)

from enum import StrEnum


class RAGPattern(StrEnum):
    """RAG 実行パターン."""

    BASIC = "basic"  # 単純なベクトル検索
    HYBRID = "hybrid"  # ベクトル + キーワード (BM25)
    ADVANCED = "advanced"  # クエリ書き換え + ハイブリッド + リランキング


class UnifiedRAGService:
    """統合RAGサービスクラス."""

    def __init__(
        self,
        collection_name: str = "default",
        embedding_provider: EmbeddingProvider | None = None,
        vector_db: VectorDBProvider | None = None,
        llm: LLMProvider | None = None,
        pattern: RAGPattern = RAGPattern.HYBRID,
    ) -> None:
        """初期化."""
        self.collection_name = collection_name
        self.embedding = embedding_provider or get_embedding()
        self.vector_db = vector_db or get_vectordb(collection=collection_name)
        self.llm = llm or get_llm()
        self.pattern = pattern
        self._initialized = False

    async def _ensure_connected(self) -> None:
        """接続を確認."""
        if not self._initialized:
            await self.vector_db.connect()
            self._initialized = True

    async def _rewrite_query(self, query: str) -> str:
        """クエリを検索用に最適化（Advanced パターン用）."""
        prompt = (
            "与えられた質問を、検索エンジンの精度が高まるような検索クエリに変換してください。\n"
            "単語の羅列や、より具体的なキーワードを含めてください。回答はクエリのみを出力してください。\n\n"
            f"質問: {query}"
        )
        try:
            response = await self.llm.chat(messages=[{"role": "user", "content": prompt}])
            rewritten = response.get("content", query)
            logger.debug("Query rewritten: %s -> %s", query, rewritten)
            return str(rewritten).strip()
        except Exception as e:
            logger.warning("Query rewriting failed, using original query: %s", e)
            return query

    async def add_documents(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> list[str]:
        """ドキュメントを追加（埋め込み自動生成）."""
        await self._ensure_connected()
        embeddings = await self.embedding.embed_documents(documents)
        return await self.vector_db.add_documents(
            documents=documents, ids=ids, embeddings=embeddings, metadatas=metadatas
        )

    async def retrieve(
        self,
        query: str,
        top_k: int = 5,
        filter: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """ドキュメントを検索."""
        await self._ensure_connected()

        search_query = query
        if self.pattern == RAGPattern.ADVANCED:
            search_query = await self._rewrite_query(query)

        query_embedding = await self.embedding.embed_query(search_query)

        # パターンに応じた検索実行
        # 現状の VectorDBProvider は内部でハイブリッドを吸収している想定だが、
        # 必要に応じてここで明示的に切り替えるロジックを追加可能。
        results = await self.vector_db.similarity_search(
            query=search_query, query_embedding=query_embedding, k=top_k, filter=filter
        )

        # Advanced パターンの場合はリランキングを検討（将来拡張用）
        if self.pattern == RAGPattern.ADVANCED and len(results) > 1:
            logger.debug("Advanced pattern: scores could be further refined with Reranker")

        return results

    async def query(
        self,
        query: str,
        top_k: int = 5,
        filter: dict[str, Any] | None = None,
        system_prompt: str | None = None,
    ) -> str:
        """RAGクエリ（検索 + 生成）."""
        # 1. 検索
        docs = await self.retrieve(query, top_k=top_k, filter=filter)

        # 2. コンテキスト構築
        context_parts = []
        for i, d in enumerate(docs):
            source = f"[Source {i + 1}]"
            content = d.get("document", d.get("content", ""))
            context_parts.append(f"{source}:\n{content}")

        context = "\n\n".join(context_parts)

        # 3. LLM生成
        default_prompt = (
            "以下のコンテキスト（参考情報）を基に、ユーザーの質問に正確に回答してください。\n"
            "不明な場合は「わかりません」と回答し、推測で答えないでください。\n"
            "回答には、どのソースを参考にしたか可能な限り言及してください（例: [Source 1] によれば...）。\n\n"
            f"コンテキスト:\n{context}"
        )

        final_system_prompt = f"{system_prompt}\n\n{default_prompt}" if system_prompt else default_prompt

        response = await self.llm.chat(
            messages=[{"role": "system", "content": final_system_prompt}, {"role": "user", "content": query}]
        )
        return str(response.get("content", str(response)))

    async def close(self) -> None:
        """リソースを解放."""
        if self._initialized:
            # 実際の実装に合わせて修正 (disconnect がない場合がある)
            if hasattr(self.vector_db, "disconnect") and callable(self.vector_db.disconnect):
                await self.vector_db.disconnect()
            self._initialized = False
