"""RAG パイプラインモジュール.

検索増強生成（Retrieval-Augmented Generation）の完全な実装を提供します。

機能:
- ドキュメントのインジェスト・インデックス
- セマンティック検索
- コンテキスト構築
- LLM による回答生成
- ソースの引用

設計原則:
- 松耦合: プロバイダーは環境変数から自動検出
- 拡張可能: カスタム埋め込み・検索戦略対応
- 本番対応: キャッシュ、バッチ処理、モニタリング
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.knowledge.document_loader import (
    ChunkingConfig,
    DocumentChunk,
    UniversalLoader,
)
from agentflow.providers import get_embedding, get_llm, get_vectordb


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.providers.embedding_provider import EmbeddingProvider
    from agentflow.providers.llm_provider import LLMProvider
    from agentflow.providers.vectordb_provider import VectorDBProvider


logger = logging.getLogger(__name__)


@dataclass
class RAGConfig:
    """RAG 設定.

    Attributes:
        collection_name: ベクトルストアのコレクション名
        top_k: 検索結果の上位 K 件
        min_similarity: 最小類似度閾値
        system_prompt: システムプロンプト
        context_template: コンテキストテンプレート
        max_context_length: 最大コンテキスト長
        include_sources: ソース情報を含めるか
        chunking_config: チャンキング設定
    """

    collection_name: str = "agentflow_rag"
    top_k: int = 5
    min_similarity: float = 0.3
    system_prompt: str = """あなたは知識ベースに基づいて質問に答えるアシスタントです。
提供された参考情報のみを使用して回答してください。
情報が不足している場合は、正直にその旨を伝えてください。"""
    context_template: str = """参考情報:
{context}

質問: {query}"""
    max_context_length: int = 4000
    include_sources: bool = True
    chunking_config: ChunkingConfig = field(default_factory=ChunkingConfig)


@dataclass
class RAGResult:
    """RAG 結果.

    Attributes:
        answer: LLM 生成回答
        sources: 使用したソース情報
        context_used: 使用したコンテキスト
        query: 元のクエリ
        search_results: 検索結果
        metadata: 追加メタデータ
    """

    answer: str
    sources: list[dict[str, Any]] = field(default_factory=list)
    context_used: str = ""
    query: str = ""
    search_results: list[dict[str, Any]] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


class RAGPipeline:
    """RAG パイプライン（松耦合設計）.

    知識ベースから関連情報を検索し、LLM で回答を生成します。
    全てのプロバイダーは環境変数から自動検出されます。

    Example:
        >>> # 初期化
        >>> rag = RAGPipeline()
        >>> await rag.start()
        >>>
        >>> # ドキュメント追加
        >>> await rag.add_documents("path/to/docs/")
        >>>
        >>> # 質問応答
        >>> result = await rag.query("AgentFlow とは何ですか？")
        >>> print(result.answer)
        >>>
        >>> # ストリーム回答
        >>> async for chunk in rag.stream("質問"):
        ...     print(chunk, end="")
    """

    def __init__(
        self,
        config: RAGConfig | None = None,
        *,
        llm_temperature: float | None = None,
    ) -> None:
        """初期化.

        Note:
            全てのプロバイダーは環境変数から自動検出されます（松耦合設計）。

        Args:
            config: RAG 設定
            llm_temperature: LLM 温度パラメータ（省略時はデフォルト）
        """
        self._config = config or RAGConfig()
        self._logger = logging.getLogger(__name__)
        self._started = False

        # プロバイダー（遅延初期化）
        self._llm: LLMProvider | None = None
        self._embedding: EmbeddingProvider | None = None
        self._vectordb: VectorDBProvider | None = None
        self._llm_temperature = llm_temperature

        # ドキュメントローダー
        self._loader = UniversalLoader(self._config.chunking_config)

        # 統計
        self._stats = {
            "documents_indexed": 0,
            "queries_processed": 0,
            "total_chunks": 0,
        }

    async def start(self) -> None:
        """RAG パイプラインを開始."""
        if self._started:
            return

        # プロバイダーを初期化
        self._llm = get_llm(temperature=self._llm_temperature)
        self._embedding = get_embedding()
        self._vectordb = get_vectordb(collection=self._config.collection_name)

        # VectorDB に接続
        await self._vectordb.connect()

        self._started = True
        self._logger.info("RAG Pipeline started")

    async def stop(self) -> None:
        """RAG パイプラインを停止."""
        if not self._started:
            return

        if self._vectordb:
            await self._vectordb.disconnect()

        self._started = False
        self._logger.info("RAG Pipeline stopped")

    async def add_document(
        self,
        content: str,
        metadata: dict[str, Any] | None = None,
        source: str = "",
    ) -> str:
        """単一ドキュメントを追加.

        Args:
            content: ドキュメント内容
            metadata: メタデータ
            source: ソース情報

        Returns:
            ドキュメント ID
        """
        self._ensure_started()

        # チャンキング
        chunks = self._loader._loaders[5]._chunk_text(content, source)  # TextLoader

        # メタデータを追加
        for chunk in chunks:
            if metadata:
                chunk.metadata.update(metadata)

        # インデックス
        await self._index_chunks(chunks)

        self._stats["documents_indexed"] += 1
        self._stats["total_chunks"] += len(chunks)

        return chunks[0].id if chunks else ""

    async def add_documents(
        self,
        source: str | Path,
        pattern: str = "*",
        recursive: bool = True,
    ) -> int:
        """ディレクトリからドキュメントを一括追加.

        Args:
            source: ファイルパスまたはディレクトリパス
            pattern: ファイルパターン（ディレクトリの場合）
            recursive: 再帰的に読み込むか

        Returns:
            追加したチャンク数
        """
        self._ensure_started()

        path = Path(source)

        if path.is_file():
            chunks = await self._loader.load(path)
        elif path.is_dir():
            chunks = await self._loader.load_directory(path, pattern, recursive)
        else:
            msg = f"Source not found: {source}"
            raise FileNotFoundError(msg)

        # インデックス
        await self._index_chunks(chunks)

        self._stats["documents_indexed"] += 1
        self._stats["total_chunks"] += len(chunks)

        return len(chunks)

    async def add_chunks(self, chunks: list[DocumentChunk]) -> int:
        """チャンクを直接追加.

        Args:
            chunks: DocumentChunk のリスト

        Returns:
            追加したチャンク数
        """
        self._ensure_started()
        await self._index_chunks(chunks)
        self._stats["total_chunks"] += len(chunks)
        return len(chunks)

    async def _index_chunks(self, chunks: list[DocumentChunk]) -> None:
        """チャンクをインデックス."""
        if not chunks:
            return

        # 埋め込みを生成
        contents = [c.content for c in chunks]
        embeddings = await self._embedding.embed_batch(contents)

        # VectorDB に追加
        await self._vectordb.add(
            documents=contents,
            ids=[c.id for c in chunks],
            embeddings=embeddings,
            metadatas=[c.metadata for c in chunks],
        )

        self._logger.debug(f"Indexed {len(chunks)} chunks")

    async def search(
        self,
        query: str,
        top_k: int | None = None,
        min_similarity: float | None = None,
        filters: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """セマンティック検索.

        Args:
            query: 検索クエリ
            top_k: 返却する上位 K 件
            min_similarity: 最小類似度閾値
            filters: メタデータフィルタ

        Returns:
            検索結果のリスト
        """
        self._ensure_started()

        top_k = top_k or self._config.top_k
        min_similarity = min_similarity or self._config.min_similarity

        # クエリの埋め込みを生成
        query_embedding = await self._embedding.embed_text(query)

        # 検索実行
        results = await self._vectordb.search(
            query=query,
            query_embedding=query_embedding,
            top_k=top_k,
            filter_metadata=filters,
        )

        # 類似度フィルタリング
        return [
            r for r in results if r.get("distance", 1.0) <= (1.0 - min_similarity)
        ]


    async def query(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> RAGResult:
        """RAG クエリを実行.

        Args:
            query: 質問
            top_k: 検索結果の上位 K 件
            filters: メタデータフィルタ

        Returns:
            RAGResult
        """
        self._ensure_started()
        self._stats["queries_processed"] += 1

        # 1. 関連ドキュメントを検索
        search_results = await self.search(query, top_k, filters=filters)

        # 2. コンテキストを構築
        context_parts = []
        sources = []
        current_length = 0

        for i, result in enumerate(search_results):
            doc = result.get("document", "")
            if current_length + len(doc) > self._config.max_context_length:
                break

            context_parts.append(f"[{i + 1}] {doc}")
            current_length += len(doc)

            sources.append(
                {
                    "index": i + 1,
                    "id": result.get("id", ""),
                    "content": doc[:200] + "..." if len(doc) > 200 else doc,
                    "metadata": result.get("metadata", {}),
                    "similarity": 1.0 - result.get("distance", 0.0),
                }
            )

        context = "\n\n".join(context_parts) if context_parts else "関連情報なし"

        # 3. プロンプト作成
        user_prompt = self._config.context_template.format(
            context=context,
            query=query,
        )

        # 4. LLM で回答生成
        messages = [
            {"role": "system", "content": self._config.system_prompt},
            {"role": "user", "content": user_prompt},
        ]
        response = await self._llm.chat(messages)

        return RAGResult(
            answer=response["content"],
            sources=sources if self._config.include_sources else [],
            context_used=context,
            query=query,
            search_results=search_results,
            metadata={
                "model": response.get("model", "unknown"),
                "timestamp": datetime.now(UTC).isoformat(),
            },
        )

    async def stream(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> AsyncIterator[str]:
        """RAG クエリをストリーム実行.

        Args:
            query: 質問
            top_k: 検索結果の上位 K 件
            filters: メタデータフィルタ

        Yields:
            回答のチャンク
        """
        self._ensure_started()
        self._stats["queries_processed"] += 1

        # 1. 関連ドキュメントを検索
        search_results = await self.search(query, top_k, filters=filters)

        # 2. コンテキストを構築
        context_parts = []
        for i, result in enumerate(search_results):
            doc = result.get("document", "")
            context_parts.append(f"[{i + 1}] {doc}")

        context = "\n\n".join(context_parts) if context_parts else "関連情報なし"

        # 3. プロンプト作成
        user_prompt = self._config.context_template.format(
            context=context,
            query=query,
        )

        # 4. LLM でストリーム生成
        messages = [
            {"role": "system", "content": self._config.system_prompt},
            {"role": "user", "content": user_prompt},
        ]

        async for chunk in self._llm.stream(messages):
            yield chunk

    async def clear(self) -> None:
        """全ドキュメントをクリア."""
        self._ensure_started()
        await self._vectordb.clear()
        self._stats = {
            "documents_indexed": 0,
            "queries_processed": 0,
            "total_chunks": 0,
        }
        self._logger.info("RAG Pipeline cleared")

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            **self._stats,
            "collection": self._config.collection_name,
            "started": self._started,
        }

    def _ensure_started(self) -> None:
        """パイプラインが開始されていることを確認."""
        if not self._started:
            msg = "RAG Pipeline not started. Call start() first."
            raise RuntimeError(msg)

    async def __aenter__(self) -> RAGPipeline:
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

