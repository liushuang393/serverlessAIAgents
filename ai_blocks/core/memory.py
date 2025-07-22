"""
Memory（記憶）コンポーネント

このモジュールは、ベクトル検索を伴う長期記憶のための抽象インターフェースと
具体的な実装を提供します。
"""

import time
import uuid
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

from ..config import get_settings
from ..utils.logging import get_logger
from .models import MemoryItem
from .registry import DeploymentStrategy, HealthCheckResult, register_component

logger = get_logger(__name__)


class MemoryInterface(ABC):
    """長期記憶のための抽象インターフェース"""

    @abstractmethod
    async def store(self, content: str, metadata: Dict[str, Any] = None) -> str:
        """
        コンテンツを記憶に保存する

        Args:
            content: 保存するテキストコンテンツ
            metadata: 関連するメタデータ

        Returns:
            str: 保存された記憶のID
        """
        pass

    @abstractmethod
    async def search(
        self, query: str, limit: int = 10, threshold: float = None
    ) -> List[MemoryItem]:
        """
        類似性検索を実行する

        Args:
            query: 検索クエリ
            limit: 返す結果の最大数
            threshold: 類似度の閾値（0.0-1.0）

        Returns:
            List[MemoryItem]: 類似度順にソートされた記憶アイテムのリスト
        """
        pass

    @abstractmethod
    async def get(self, memory_id: str) -> Optional[MemoryItem]:
        """
        IDで記憶を取得する

        Args:
            memory_id: 記憶のID

        Returns:
            Optional[MemoryItem]: 記憶アイテム、存在しない場合はNone
        """
        pass

    @abstractmethod
    async def delete(self, memory_id: str) -> bool:
        """
        記憶を削除する

        Args:
            memory_id: 削除する記憶のID

        Returns:
            bool: 削除が成功したかどうか
        """
        pass

    @abstractmethod
    async def clear(self) -> None:
        """全ての記憶を削除する"""
        pass

    @abstractmethod
    async def count(self) -> int:
        """
        記憶の総数を取得する

        Returns:
            int: 記憶の総数
        """
        pass


class InMemoryVectorStore:
    """シンプルなインメモリベクトルストア"""

    def __init__(self, embedding_dim: int = 1536):
        """
        インメモリベクトルストアを初期化する

        Args:
            embedding_dim: 埋め込みベクトルの次元数
        """
        self.embedding_dim = embedding_dim
        self.vectors: Dict[str, np.ndarray] = {}
        self.contents: Dict[str, str] = {}
        self.metadata: Dict[str, Dict[str, Any]] = {}
        self.created_at: Dict[str, datetime] = {}

    def add(
        self, id: str, vector: np.ndarray, content: str, metadata: Dict[str, Any] = None
    ) -> None:
        """
        ベクトルを追加する

        Args:
            id: ベクトルのID
            vector: 埋め込みベクトル
            content: 関連するコンテンツ
            metadata: 関連するメタデータ
        """
        self.vectors[id] = vector / np.linalg.norm(vector)  # 正規化
        self.contents[id] = content
        self.metadata[id] = metadata or {}
        self.created_at[id] = datetime.now()

    def search(
        self, query_vector: np.ndarray, limit: int = 10, threshold: float = 0.0
    ) -> List[Tuple[str, float]]:
        """
        類似ベクトルを検索する

        Args:
            query_vector: 検索クエリベクトル
            limit: 返す結果の最大数
            threshold: 類似度の閾値（0.0-1.0）

        Returns:
            List[Tuple[str, float]]: (ID, 類似度スコア)のリスト
        """
        if not self.vectors:
            return []

        # クエリベクトルを正規化
        query_vector = query_vector / np.linalg.norm(query_vector)

        # コサイン類似度を計算
        results = []
        for id, vector in self.vectors.items():
            similarity = np.dot(query_vector, vector)
            if similarity >= threshold:
                results.append((id, float(similarity)))

        # 類似度でソート（降順）
        results.sort(key=lambda x: x[1], reverse=True)
        return results[:limit]

    def get(
        self, id: str
    ) -> Optional[Tuple[np.ndarray, str, Dict[str, Any], datetime]]:
        """
        IDでベクトルを取得する

        Args:
            id: ベクトルのID

        Returns:
            Optional[Tuple]: (ベクトル, コンテンツ, メタデータ, 作成日時)のタプル
        """
        if id not in self.vectors:
            return None

        return (
            self.vectors[id],
            self.contents[id],
            self.metadata[id],
            self.created_at[id],
        )

    def delete(self, id: str) -> bool:
        """
        ベクトルを削除する

        Args:
            id: 削除するベクトルのID

        Returns:
            bool: 削除が成功したかどうか
        """
        if id not in self.vectors:
            return False

        del self.vectors[id]
        del self.contents[id]
        del self.metadata[id]
        del self.created_at[id]
        return True

    def clear(self) -> None:
        """全てのベクトルを削除する"""
        self.vectors.clear()
        self.contents.clear()
        self.metadata.clear()
        self.created_at.clear()

    def count(self) -> int:
        """
        ベクトルの総数を取得する

        Returns:
            int: ベクトルの総数
        """
        return len(self.vectors)


class MockEmbedder:
    """モックの埋め込みモデル（テスト用）"""

    def __init__(self, embedding_dim: int = 1536):
        """
        モック埋め込みモデルを初期化する

        Args:
            embedding_dim: 埋め込みベクトルの次元数
        """
        self.embedding_dim = embedding_dim

    async def embed(self, text: str) -> np.ndarray:
        """
        テキストを埋め込みベクトルに変換する

        Args:
            text: 埋め込むテキスト

        Returns:
            np.ndarray: 埋め込みベクトル
        """
        # 単純なハッシュベースの埋め込み（実際の類似性は反映しない）
        np.random.seed(hash(text) % 2**32)
        vector = np.random.randn(self.embedding_dim)
        return vector / np.linalg.norm(vector)  # 正規化


@register_component(
    component_type="memory",
    component_name="vector",
    version="1.0.0",
    deployment_strategy=DeploymentStrategy.BLUE_GREEN,
)
class VectorMemory(MemoryInterface):
    """ベクトル検索ベースの記憶実装"""

    def __init__(
        self,
        vector_store: Any = None,
        embedder: Any = None,
        max_items: int = None,
        similarity_threshold: float = None,
    ):
        """
        ベクトル記憶を初期化する

        Args:
            vector_store: ベクトルストア（Noneの場合はインメモリストアを使用）
            embedder: 埋め込みモデル（Noneの場合はモックを使用）
            max_items: 記憶の最大アイテム数
            similarity_threshold: デフォルトの類似度閾値
        """
        settings = get_settings()

        self.vector_store = vector_store or InMemoryVectorStore()
        self.embedder = embedder or MockEmbedder()
        self.max_items = max_items or settings.memory_max_items
        self.similarity_threshold = (
            similarity_threshold or settings.memory_similarity_threshold
        )

        logger.info(f"ベクトル記憶を初期化しました（最大アイテム数: {self.max_items}）")

    async def health_check(self) -> HealthCheckResult:
        """
        ヘルスチェックを実行する

        Returns:
            HealthCheckResult: ヘルスチェック結果
        """
        try:
            # 基本的な動作確認
            test_content = "ヘルスチェック用テストコンテンツ"
            test_id = await self.store(test_content, {"test": True})

            # 検索テスト
            results = await self.search("テスト", limit=1)

            # テストデータを削除
            if hasattr(self, "delete"):
                await self.delete(test_id)

            # メトリクス収集
            total_items = await self.count()

            return HealthCheckResult(
                healthy=True,
                message="メモリコンポーネントは正常に動作しています",
                metrics={
                    "total_items": total_items,
                    "max_items": self.max_items,
                    "similarity_threshold": self.similarity_threshold,
                    "search_test_results": len(results),
                },
            )

        except Exception as e:
            return HealthCheckResult(
                healthy=False,
                message=f"メモリコンポーネントでエラーが発生しました: {str(e)}",
                metrics={"error": str(e)},
            )

    async def store(self, content: str, metadata: Dict[str, Any] = None) -> str:
        """
        コンテンツを記憶に保存する

        Args:
            content: 保存するテキストコンテンツ
            metadata: 関連するメタデータ

        Returns:
            str: 保存された記憶のID
        """
        if not content.strip():
            raise ValueError("空のコンテンツは保存できません")

        # 一意のIDを生成
        memory_id = str(uuid.uuid4())

        # コンテンツを埋め込みベクトルに変換
        vector = await self.embedder.embed(content)

        # メタデータを準備
        metadata = metadata or {}
        metadata["timestamp"] = time.time()

        # ベクトルストアに追加
        self.vector_store.add(memory_id, vector, content, metadata)

        # 最大アイテム数を超えた場合、古いアイテムを削除
        if self.max_items > 0 and await self.count() > self.max_items:
            await self._remove_oldest_items()

        logger.debug(f"記憶を保存しました（ID: {memory_id[:8]}...）")
        return memory_id

    async def search(
        self, query: str, limit: int = 10, threshold: float = None
    ) -> List[MemoryItem]:
        """
        類似性検索を実行する

        Args:
            query: 検索クエリ
            limit: 返す結果の最大数
            threshold: 類似度の閾値（0.0-1.0）

        Returns:
            List[MemoryItem]: 類似度順にソートされた記憶アイテムのリスト
        """
        if not query.strip():
            raise ValueError("空のクエリでは検索できません")

        # 閾値を設定
        threshold = threshold if threshold is not None else self.similarity_threshold

        # クエリを埋め込みベクトルに変換
        query_vector = await self.embedder.embed(query)

        # 類似ベクトルを検索
        results = self.vector_store.search(query_vector, limit, threshold)

        # 結果をMemoryItemに変換
        memory_items = []
        for memory_id, similarity in results:
            item_data = self.vector_store.get(memory_id)
            if item_data:
                _, content, metadata, created_at = item_data
                memory_items.append(
                    MemoryItem(
                        id=memory_id,
                        content=content,
                        metadata=metadata,
                        similarity_score=similarity,
                        created_at=created_at,
                    )
                )

        logger.debug(f"検索結果: {len(memory_items)}件")
        return memory_items

    async def get(self, memory_id: str) -> Optional[MemoryItem]:
        """
        IDで記憶を取得する

        Args:
            memory_id: 記憶のID

        Returns:
            Optional[MemoryItem]: 記憶アイテム、存在しない場合はNone
        """
        item_data = self.vector_store.get(memory_id)
        if not item_data:
            return None

        _, content, metadata, created_at = item_data
        return MemoryItem(
            id=memory_id,
            content=content,
            metadata=metadata,
            similarity_score=None,
            created_at=created_at,
        )

    async def delete(self, memory_id: str) -> bool:
        """
        記憶を削除する

        Args:
            memory_id: 削除する記憶のID

        Returns:
            bool: 削除が成功したかどうか
        """
        return self.vector_store.delete(memory_id)

    async def clear(self) -> None:
        """全ての記憶を削除する"""
        self.vector_store.clear()
        logger.info("全ての記憶を削除しました")

    async def count(self) -> int:
        """
        記憶の総数を取得する

        Returns:
            int: 記憶の総数
        """
        return self.vector_store.count()

    async def _remove_oldest_items(self) -> None:
        """最も古いアイテムを削除する"""
        # 現在のアイテム数
        current_count = await self.count()

        # 削除する必要がない場合
        if current_count <= self.max_items:
            return

        # 削除するアイテム数
        items_to_remove = current_count - self.max_items

        # 全てのアイテムを取得し、作成日時でソート
        all_items = []
        for memory_id in list(self.vector_store.vectors.keys()):
            item_data = self.vector_store.get(memory_id)
            if item_data:
                _, _, _, created_at = item_data
                all_items.append((memory_id, created_at))

        # 作成日時でソート（古い順）
        all_items.sort(key=lambda x: x[1])

        # 古いアイテムから削除
        for i in range(items_to_remove):
            if i < len(all_items):
                memory_id, _ = all_items[i]
                await self.delete(memory_id)

        logger.debug(f"{items_to_remove}件の古い記憶を削除しました")
