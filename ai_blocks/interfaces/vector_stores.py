"""
ベクトルストアインターフェース

このモジュールは、様々なベクトルデータベース（ChromaDB、Pinecone等）との
統一されたインターフェースを提供します。
"""

from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

from ..config import get_settings
from ..utils.logging import get_logger

logger = get_logger(__name__)


class VectorStore(ABC):
    """ベクトルストアの抽象基底クラス"""

    def __init__(self, collection_name: str = "default", **kwargs):
        """
        ベクトルストアを初期化する

        Args:
            collection_name: コレクション名
            **kwargs: ストア固有の設定
        """
        self.collection_name = collection_name
        self.config = kwargs
        self.settings = get_settings()

    @abstractmethod
    async def add(
        self, id: str, vector: np.ndarray, content: str, metadata: Dict[str, Any] = None
    ) -> None:
        """
        ベクトルを追加する

        Args:
            id: ベクトルの一意識別子
            vector: 埋め込みベクトル
            content: 関連するコンテンツ
            metadata: 関連するメタデータ
        """
        pass

    @abstractmethod
    async def search(
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
        pass

    @abstractmethod
    async def get(self, id: str) -> Optional[Tuple[np.ndarray, str, Dict[str, Any]]]:
        """
        IDでベクトルを取得する

        Args:
            id: ベクトルのID

        Returns:
            Optional[Tuple[np.ndarray, str, Dict[str, Any]]]: (ベクトル, コンテンツ, メタデータ)
        """
        pass

    @abstractmethod
    async def delete(self, id: str) -> bool:
        """
        ベクトルを削除する

        Args:
            id: 削除するベクトルのID

        Returns:
            bool: 削除が成功したかどうか
        """
        pass

    @abstractmethod
    async def clear(self) -> None:
        """全てのベクトルを削除する"""
        pass

    @abstractmethod
    async def count(self) -> int:
        """
        ベクトルの総数を取得する

        Returns:
            int: ベクトルの総数
        """
        pass


class ChromaDBStore(VectorStore):
    """ChromaDBベクトルストア"""

    def __init__(
        self, collection_name: str = "default", persist_directory: str = None, **kwargs
    ):
        """
        ChromaDBストアを初期化する

        Args:
            collection_name: コレクション名
            persist_directory: 永続化ディレクトリ
            **kwargs: その他の設定
        """
        super().__init__(collection_name, **kwargs)

        self.persist_directory = (
            persist_directory or self.settings.chroma_persist_directory
        )
        self._client: Any = None
        self._collection: Any = None

        logger.info(f"ChromaDBStore を初期化しました (collection: {collection_name})")

    def _get_client(self):
        """ChromaDBクライアントを取得する（遅延初期化）"""
        if self._client is None:
            try:
                import chromadb
                from chromadb.config import Settings

                self._client = chromadb.PersistentClient(
                    path=self.persist_directory,
                    settings=Settings(anonymized_telemetry=False),
                )

                # コレクションを取得または作成
                try:
                    self._collection = self._client.get_collection(
                        name=self.collection_name
                    )
                except ValueError:
                    self._collection = self._client.create_collection(
                        name=self.collection_name
                    )

            except ImportError:
                raise ImportError("chromadbパッケージがインストールされていません: pip install chromadb")

        if self._client is None or self._collection is None:
            raise RuntimeError("ChromaDBクライアントの初期化に失敗しました")

        return self._client, self._collection

    async def add(
        self, id: str, vector: np.ndarray, content: str, metadata: Dict[str, Any] = None
    ) -> None:
        """ChromaDBにベクトルを追加する"""
        _, collection = self._get_client()

        try:
            # メタデータにコンテンツとタイムスタンプを追加
            full_metadata = metadata or {}
            full_metadata.update(
                {"content": content, "created_at": datetime.now().isoformat()}
            )

            collection.add(
                ids=[id],
                embeddings=[vector.tolist()],
                documents=[content],
                metadatas=[full_metadata],
            )

            logger.debug(f"ChromaDBにベクトルを追加しました: {id}")

        except Exception as e:
            logger.error(f"ChromaDBへの追加エラー: {e}")
            raise

    async def search(
        self, query_vector: np.ndarray, limit: int = 10, threshold: float = 0.0
    ) -> List[Tuple[str, float]]:
        """ChromaDBで類似ベクトルを検索する"""
        _, collection = self._get_client()

        try:
            results = collection.query(
                query_embeddings=[query_vector.tolist()], n_results=limit
            )

            # 結果を処理
            search_results = []
            if results["ids"] and results["distances"]:
                for id, distance in zip(results["ids"][0], results["distances"][0]):
                    # ChromaDBは距離を返すので、類似度に変換
                    similarity = 1.0 - distance
                    if similarity >= threshold:
                        search_results.append((id, similarity))

            return search_results

        except Exception as e:
            logger.error(f"ChromaDB検索エラー: {e}")
            raise

    async def get(self, id: str) -> Optional[Tuple[np.ndarray, str, Dict[str, Any]]]:
        """ChromaDBからベクトルを取得する"""
        _, collection = self._get_client()

        try:
            results = collection.get(
                ids=[id], include=["embeddings", "documents", "metadatas"]
            )

            if not results["ids"]:
                return None

            vector = np.array(results["embeddings"][0])
            content = results["documents"][0]
            metadata = results["metadatas"][0]

            # メタデータからcontentとcreated_atを除去
            clean_metadata = {
                k: v for k, v in metadata.items() if k not in ["content", "created_at"]
            }

            return vector, content, clean_metadata

        except Exception as e:
            logger.error(f"ChromaDB取得エラー: {e}")
            raise

    async def delete(self, id: str) -> bool:
        """ChromaDBからベクトルを削除する"""
        _, collection = self._get_client()

        try:
            collection.delete(ids=[id])
            logger.debug(f"ChromaDBからベクトルを削除しました: {id}")
            return True

        except Exception as e:
            logger.error(f"ChromaDB削除エラー: {e}")
            return False

    async def clear(self) -> None:
        """ChromaDBの全てのベクトルを削除する"""
        client, _ = self._get_client()

        try:
            # コレクションを削除して再作成
            client.delete_collection(name=self.collection_name)
            self._collection = client.create_collection(name=self.collection_name)

            logger.info(f"ChromaDBコレクション '{self.collection_name}' をクリアしました")

        except Exception as e:
            logger.error(f"ChromaDBクリアエラー: {e}")
            raise

    async def count(self) -> int:
        """ChromaDBのベクトル総数を取得する"""
        _, collection = self._get_client()

        try:
            count = collection.count()
            return int(count) if count is not None else 0

        except Exception as e:
            logger.error(f"ChromaDBカウントエラー: {e}")
            return 0


class PineconeStore(VectorStore):
    """Pineconeベクトルストア"""

    def __init__(
        self,
        collection_name: str = "default",
        api_key: str = None,
        environment: str = None,
        dimension: int = 1536,
        **kwargs,
    ):
        """
        Pineconeストアを初期化する

        Args:
            collection_name: インデックス名
            api_key: Pinecone APIキー
            environment: Pinecone環境
            dimension: ベクトルの次元数
            **kwargs: その他の設定
        """
        super().__init__(collection_name, **kwargs)

        self.api_key = api_key or self.settings.pinecone_api_key
        self.environment = environment or self.settings.pinecone_environment
        self.dimension = dimension

        if not self.api_key or not self.environment:
            raise ValueError("Pinecone APIキーと環境が設定されていません")

        self._client = None
        self._index = None

        logger.info(f"PineconeStore を初期化しました (index: {collection_name})")

    def _get_client(self):
        """Pineconeクライアントを取得する（遅延初期化）"""
        if self._client is None:
            try:
                import pinecone

                pinecone.init(api_key=self.api_key, environment=self.environment)

                # インデックスを取得または作成
                if self.collection_name not in pinecone.list_indexes():
                    pinecone.create_index(
                        name=self.collection_name,
                        dimension=self.dimension,
                        metric="cosine",
                    )

                self._index = pinecone.Index(self.collection_name)

            except ImportError:
                raise ImportError("pinecone-clientパッケージがインストールされていません")

        return self._index

    async def add(
        self, id: str, vector: np.ndarray, content: str, metadata: Dict[str, Any] = None
    ) -> None:
        """Pineconeにベクトルを追加する"""
        index = self._get_client()

        try:
            # メタデータにコンテンツとタイムスタンプを追加
            full_metadata = metadata or {}
            full_metadata.update(
                {"content": content, "created_at": datetime.now().isoformat()}
            )

            index.upsert([(id, vector.tolist(), full_metadata)])

            logger.debug(f"Pineconeにベクトルを追加しました: {id}")

        except Exception as e:
            logger.error(f"Pineconeへの追加エラー: {e}")
            raise

    async def search(
        self, query_vector: np.ndarray, limit: int = 10, threshold: float = 0.0
    ) -> List[Tuple[str, float]]:
        """Pineconeで類似ベクトルを検索する"""
        index = self._get_client()

        try:
            results = index.query(
                vector=query_vector.tolist(), top_k=limit, include_metadata=True
            )

            search_results = []
            for match in results["matches"]:
                if match["score"] >= threshold:
                    search_results.append((match["id"], match["score"]))

            return search_results

        except Exception as e:
            logger.error(f"Pinecone検索エラー: {e}")
            raise

    async def get(self, id: str) -> Optional[Tuple[np.ndarray, str, Dict[str, Any]]]:
        """Pineconeからベクトルを取得する"""
        index = self._get_client()

        try:
            results = index.fetch([id])

            if id not in results["vectors"]:
                return None

            vector_data = results["vectors"][id]
            vector = np.array(vector_data["values"])
            metadata = vector_data.get("metadata", {})
            content = metadata.pop("content", "")
            metadata.pop("created_at", None)

            return vector, content, metadata

        except Exception as e:
            logger.error(f"Pinecone取得エラー: {e}")
            raise

    async def delete(self, id: str) -> bool:
        """Pineconeからベクトルを削除する"""
        index = self._get_client()

        try:
            index.delete([id])
            logger.debug(f"Pineconeからベクトルを削除しました: {id}")
            return True

        except Exception as e:
            logger.error(f"Pinecone削除エラー: {e}")
            return False

    async def clear(self) -> None:
        """Pineconeの全てのベクトルを削除する"""
        index = self._get_client()

        try:
            index.delete(delete_all=True)
            logger.info(f"Pineconeインデックス '{self.collection_name}' をクリアしました")

        except Exception as e:
            logger.error(f"Pineconeクリアエラー: {e}")
            raise

    async def count(self) -> int:
        """Pineconeのベクトル総数を取得する"""
        index = self._get_client()

        try:
            stats = index.describe_index_stats()
            count = stats.get("total_vector_count", 0)
            return int(count) if count is not None else 0

        except Exception as e:
            logger.error(f"Pineconeカウントエラー: {e}")
            return 0
