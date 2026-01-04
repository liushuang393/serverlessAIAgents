# -*- coding: utf-8 -*-
"""Knowledge Hooks モジュール.

React Hooks スタイルの API を提供します：
- use_vector_search: ベクトル検索フック
- use_rag: RAG クエリフック

設計原則:
- 関数型: 状態を内部管理、シンプルな API
- 松耦合: プロバイダーは環境変数から自動検出
- 再利用可能: 複数の Agent で共有可能
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Callable, Coroutine

from agentflow.providers import get_embedding, get_vectordb

if TYPE_CHECKING:
    from agentflow.providers.embedding_provider import EmbeddingProvider
    from agentflow.providers.vectordb_provider import VectorDBProvider


logger = logging.getLogger(__name__)


@dataclass
class VectorSearchResult:
    """ベクトル検索結果.

    Attributes:
        id: ドキュメント ID
        content: ドキュメント内容
        similarity: 類似度スコア（0.0-1.0）
        metadata: メタデータ
    """

    id: str
    content: str
    similarity: float
    metadata: dict[str, Any]


class VectorSearchHook:
    """ベクトル検索フック.

    Example:
        >>> search = VectorSearchHook(collection="my-docs")
        >>> await search.connect()
        >>> results = await search("類似検索クエリ", top_k=5)
        >>> for result in results:
        ...     print(f"{result.similarity:.2f}: {result.content[:50]}...")
    """

    def __init__(
        self,
        collection: str = "default",
        min_similarity: float = 0.3,
    ) -> None:
        """初期化.

        Args:
            collection: コレクション名
            min_similarity: 最小類似度閾値
        """
        self._collection = collection
        self._min_similarity = min_similarity
        self._vectordb: "VectorDBProvider | None" = None
        self._embedding: "EmbeddingProvider | None" = None
        self._connected = False
        self._logger = logging.getLogger(__name__)

    async def connect(self) -> None:
        """接続."""
        if self._connected:
            return

        self._vectordb = get_vectordb(collection=self._collection)
        self._embedding = get_embedding()
        await self._vectordb.connect()
        self._connected = True
        self._logger.info(f"VectorSearchHook connected: {self._collection}")

    async def disconnect(self) -> None:
        """切断."""
        if not self._connected:
            return

        if self._vectordb:
            await self._vectordb.disconnect()
        self._connected = False
        self._logger.info(f"VectorSearchHook disconnected: {self._collection}")

    async def __call__(
        self,
        query: str,
        top_k: int = 5,
        min_similarity: float | None = None,
        filters: dict[str, Any] | None = None,
    ) -> list[VectorSearchResult]:
        """ベクトル検索を実行.

        Args:
            query: 検索クエリ
            top_k: 返却する上位 K 件
            min_similarity: 最小類似度閾値
            filters: メタデータフィルタ

        Returns:
            VectorSearchResult のリスト
        """
        if not self._connected:
            await self.connect()

        min_sim = min_similarity or self._min_similarity

        # クエリの埋め込みを生成
        query_embedding = await self._embedding.embed_text(query)

        # 検索実行
        results = await self._vectordb.search(
            query=query,
            query_embedding=query_embedding,
            top_k=top_k,
            filter_metadata=filters,
        )

        # 結果を変換
        search_results = []
        for result in results:
            distance = result.get("distance", 1.0)
            similarity = 1.0 - distance

            if similarity >= min_sim:
                search_results.append(
                    VectorSearchResult(
                        id=result.get("id", ""),
                        content=result.get("document", ""),
                        similarity=similarity,
                        metadata=result.get("metadata", {}),
                    )
                )

        return search_results

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加.

        Args:
            documents: ドキュメントリスト
            ids: ID リスト
            metadatas: メタデータリスト
        """
        if not self._connected:
            await self.connect()

        # 埋め込みを生成
        embeddings = await self._embedding.embed_batch(documents)

        # VectorDB に追加
        await self._vectordb.add(
            documents=documents,
            ids=ids,
            embeddings=embeddings,
            metadatas=metadatas,
        )

        self._logger.debug(f"Added {len(documents)} documents")

    async def delete(self, ids: list[str]) -> int:
        """ドキュメントを削除.

        Args:
            ids: 削除する ID リスト

        Returns:
            削除した数
        """
        if not self._connected:
            await self.connect()

        return await self._vectordb.delete(ids)

    async def __aenter__(self) -> "VectorSearchHook":
        """非同期コンテキストマネージャーのエントリー."""
        await self.connect()
        return self

    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.disconnect()


def use_vector_search(
    collection: str = "default",
    min_similarity: float = 0.3,
) -> VectorSearchHook:
    """ベクトル検索フックを作成（React Hooks スタイル）.

    Example:
        >>> search = use_vector_search(collection="my-docs")
        >>> results = await search("検索クエリ")

    Args:
        collection: コレクション名
        min_similarity: 最小類似度閾値

    Returns:
        VectorSearchHook インスタンス
    """
    return VectorSearchHook(collection=collection, min_similarity=min_similarity)


@dataclass
class RAGQueryResult:
    """RAG クエリ結果.

    Attributes:
        answer: 生成された回答
        sources: 使用したソース
        query: 元のクエリ
    """

    answer: str
    sources: list[VectorSearchResult]
    query: str


class RAGHook:
    """RAG クエリフック.

    Example:
        >>> rag = RAGHook(collection="knowledge-base")
        >>> await rag.start()
        >>> result = await rag("AgentFlow とは？")
        >>> print(result.answer)
    """

    def __init__(
        self,
        collection: str = "default",
        top_k: int = 5,
        min_similarity: float = 0.3,
        system_prompt: str | None = None,
    ) -> None:
        """初期化.

        Args:
            collection: コレクション名
            top_k: 検索結果の上位 K 件
            min_similarity: 最小類似度閾値
            system_prompt: システムプロンプト
        """
        self._collection = collection
        self._top_k = top_k
        self._min_similarity = min_similarity
        self._system_prompt = (
            system_prompt
            or """あなたは知識ベースに基づいて質問に答えるアシスタントです。
提供された参考情報のみを使用して回答してください。"""
        )

        self._search_hook = VectorSearchHook(collection, min_similarity)
        self._llm: Any = None
        self._started = False
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """開始."""
        if self._started:
            return

        from agentflow.providers import get_llm

        await self._search_hook.connect()
        self._llm = get_llm()
        self._started = True
        self._logger.info(f"RAGHook started: {self._collection}")

    async def stop(self) -> None:
        """停止."""
        if not self._started:
            return

        await self._search_hook.disconnect()
        self._started = False
        self._logger.info(f"RAGHook stopped: {self._collection}")

    async def __call__(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> RAGQueryResult:
        """RAG クエリを実行.

        Args:
            query: 質問
            top_k: 検索結果の上位 K 件
            filters: メタデータフィルタ

        Returns:
            RAGQueryResult
        """
        if not self._started:
            await self.start()

        top_k = top_k or self._top_k

        # 1. 関連ドキュメントを検索
        search_results = await self._search_hook(
            query, top_k=top_k, filters=filters
        )

        # 2. コンテキストを構築
        context_parts = []
        for i, result in enumerate(search_results):
            context_parts.append(f"[{i + 1}] {result.content}")

        context = "\n\n".join(context_parts) if context_parts else "関連情報なし"

        # 3. プロンプト作成
        user_prompt = f"""参考情報:
{context}

質問: {query}"""

        # 4. LLM で回答生成
        messages = [
            {"role": "system", "content": self._system_prompt},
            {"role": "user", "content": user_prompt},
        ]
        response = await self._llm.chat(messages)

        return RAGQueryResult(
            answer=response["content"],
            sources=search_results,
            query=query,
        )

    async def add_documents(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加.

        Args:
            documents: ドキュメントリスト
            ids: ID リスト
            metadatas: メタデータリスト
        """
        if not self._started:
            await self.start()

        await self._search_hook.add(documents, ids, metadatas)

    async def __aenter__(self) -> "RAGHook":
        """非同期コンテキストマネージャーのエントリー."""
        await self.start()
        return self

    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.stop()


def use_rag(
    collection: str = "default",
    top_k: int = 5,
    min_similarity: float = 0.3,
    system_prompt: str | None = None,
) -> RAGHook:
    """RAG フックを作成（React Hooks スタイル）.

    Example:
        >>> rag = use_rag(collection="knowledge-base")
        >>> result = await rag("質問")
        >>> print(result.answer)

    Args:
        collection: コレクション名
        top_k: 検索結果の上位 K 件
        min_similarity: 最小類似度閾値
        system_prompt: システムプロンプト

    Returns:
        RAGHook インスタンス
    """
    return RAGHook(
        collection=collection,
        top_k=top_k,
        min_similarity=min_similarity,
        system_prompt=system_prompt,
    )

