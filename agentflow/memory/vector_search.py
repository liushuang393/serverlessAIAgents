"""ベクトル検索モジュール.

意味的検索のためのベクトル埋め込みと類似度検索を提供します。
"""

import asyncio
import logging
import math
from typing import Any

from agentflow.memory.types import MemoryEntry


class VectorSearch:
    """ベクトル検索エンジン.

    機能:
    - テキストのベクトル埋め込み生成
    - コサイン類似度による意味的検索
    - キャッシュによる高速化

    Note:
        現在は簡易的なTF-IDF風の埋め込みを使用。
        本番環境では OpenAI Embeddings や Sentence Transformers を推奨。
    """

    def __init__(self, embedding_dim: int = 384) -> None:
        """初期化.

        Args:
            embedding_dim: 埋め込みベクトルの次元数
        """
        self._logger = logging.getLogger(__name__)
        self._embedding_dim = embedding_dim
        self._embedding_cache: dict[str, list[float]] = {}
        self._vocabulary: dict[str, int] = {}
        self._idf: dict[str, float] = {}

    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトル埋め込みに変換.

        Args:
            text: 入力テキスト

        Returns:
            埋め込みベクトル
        """
        # キャッシュチェック
        if text in self._embedding_cache:
            return self._embedding_cache[text]

        # 簡易的なTF-IDF風の埋め込み
        embedding = await self._simple_embedding(text)

        # キャッシュに保存
        self._embedding_cache[text] = embedding

        return embedding

    async def _simple_embedding(self, text: str) -> list[float]:
        """簡易的な埋め込み生成（TF-IDF風）.

        Args:
            text: 入力テキスト

        Returns:
            埋め込みベクトル
        """
        # トークン化（簡易版）
        tokens = text.lower().split()

        # 語彙の更新
        for token in tokens:
            if token not in self._vocabulary:
                self._vocabulary[token] = len(self._vocabulary)

        # TF計算
        tf: dict[str, float] = {}
        for token in tokens:
            tf[token] = tf.get(token, 0) + 1

        # 正規化
        max_tf = max(tf.values()) if tf else 1
        for token in tf:
            tf[token] = tf[token] / max_tf

        # ベクトル生成（固定次元）
        embedding = [0.0] * self._embedding_dim
        for token, value in tf.items():
            idx = self._vocabulary[token] % self._embedding_dim
            embedding[idx] += value

        # L2正規化
        norm = math.sqrt(sum(x * x for x in embedding))
        if norm > 0:
            embedding = [x / norm for x in embedding]

        return embedding

    async def cosine_similarity(self, vec1: list[float], vec2: list[float]) -> float:
        """コサイン類似度を計算.

        Args:
            vec1: ベクトル1
            vec2: ベクトル2

        Returns:
            コサイン類似度（0.0～1.0）
        """
        if len(vec1) != len(vec2):
            raise ValueError("Vectors must have the same dimension")

        dot_product = sum(a * b for a, b in zip(vec1, vec2))
        return max(0.0, min(1.0, dot_product))  # クリップ

    async def search_similar(
        self,
        query: str,
        memories: list[MemoryEntry],
        top_k: int = 10,
        min_similarity: float = 0.0,
    ) -> list[tuple[MemoryEntry, float]]:
        """意味的に類似した記憶を検索.

        Args:
            query: 検索クエリ
            memories: 検索対象の記憶リスト
            top_k: 返却する上位K件
            min_similarity: 最小類似度閾値

        Returns:
            (記憶, 類似度スコア) のリスト（類似度降順）
        """
        # クエリの埋め込み
        query_embedding = await self.embed_text(query)

        # 各記憶との類似度を計算
        similarities: list[tuple[MemoryEntry, float]] = []
        for memory in memories:
            memory_embedding = await self.embed_text(memory.content)
            similarity = await self.cosine_similarity(query_embedding, memory_embedding)

            if similarity >= min_similarity:
                similarities.append((memory, similarity))

        # 類似度降順でソート
        similarities.sort(key=lambda x: x[1], reverse=True)

        return similarities[:top_k]

    def clear_cache(self) -> None:
        """キャッシュをクリア."""
        self._embedding_cache.clear()
        self._logger.info("Vector search cache cleared")

