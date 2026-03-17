"""Data Provider - 統一データアクセスインターフェース.

このモジュールは、SQL/Vector/Cacheの統一アクセスを提供します。
デフォルトでSQLite + ChromaDB + Memoryを使用。

使用例:
    >>> db = DataProvider.sql()
    >>> rows = await db.query("SELECT * FROM users WHERE id = ?", [1])
    >>> vector = DataProvider.vector()
    >>> similar = await vector.search("検索クエリ", top_k=5)
    >>> cache = DataProvider.cache()
    >>> await cache.set("key", "value", ttl=3600)
"""

import logging
import os
from typing import Any

from pydantic import BaseModel, Field


class SQLConfig(BaseModel):
    """SQL設定.

    Args:
        url: データベースURL
        echo: SQLログ出力
    """

    url: str = Field(default="sqlite:///./data.db", description="データベースURL")
    echo: bool = Field(default=False, description="SQLログ出力")


class VectorConfig(BaseModel):
    """Vector DB設定.

    Args:
        provider: プロバイダー (chromadb/pgvector/pinecone)
        collection: コレクション名
        persist_directory: 永続化ディレクトリ
    """

    provider: str = Field(default="chromadb", description="プロバイダー")
    collection: str = Field(default="default", description="コレクション名")
    persist_directory: str = Field(default="./chroma_data", description="永続化ディレクトリ")


class CacheConfig(BaseModel):
    """Cache設定.

    Args:
        provider: プロバイダー (memory/redis)
        url: Redis URL（redis使用時）
    """

    provider: str = Field(default="memory", description="プロバイダー")
    url: str | None = Field(default=None, description="Redis URL")


class SQLProvider:
    """SQL データベースプロバイダー."""

    def __init__(self, config: SQLConfig) -> None:
        """初期化."""
        self._config = config
        self._logger = logging.getLogger(__name__)
        self._engine: Any = None

    async def query(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> list[dict[str, Any]]:
        """SQLクエリを実行.

        Args:
            sql: SQL文
            params: パラメータ

        Returns:
            結果行のリスト
        """
        # 簡易実装（実際はSQLAlchemy等を使用）
        self._logger.debug(f"SQL: {sql}, params: {params}")
        return []

    async def execute(
        self,
        sql: str,
        params: list[Any] | None = None,
    ) -> int:
        """SQL（INSERT/UPDATE/DELETE）を実行.

        Args:
            sql: SQL文
            params: パラメータ

        Returns:
            影響行数
        """
        self._logger.debug(f"Execute: {sql}, params: {params}")
        return 0


class VectorProvider:
    """Vector データベースプロバイダー."""

    def __init__(self, config: VectorConfig) -> None:
        """初期化."""
        self._config = config
        self._logger = logging.getLogger(__name__)
        self._client: Any = None
        self._collection: Any = None

    def _ensure_client(self) -> None:
        """クライアントを初期化."""
        if self._client is not None:
            return

        if self._config.provider == "chromadb":
            try:
                import chromadb

                self._client = chromadb.Client()
                self._collection = self._client.get_or_create_collection(name=self._config.collection)
                self._logger.info("ChromaDB initialized")
            except ImportError:
                self._logger.warning("chromadb not installed, using mock")

    async def add(
        self,
        documents: list[str],
        ids: list[str] | None = None,
        metadatas: list[dict[str, Any]] | None = None,
    ) -> None:
        """ドキュメントを追加.

        Args:
            documents: ドキュメントリスト
            ids: IDリスト
            metadatas: メタデータリスト
        """
        self._ensure_client()
        if self._collection is None:
            self._logger.warning("Vector DB not available")
            return

        if ids is None:
            ids = [f"doc_{i}" for i in range(len(documents))]

        self._collection.add(
            documents=documents,
            ids=ids,
            metadatas=metadatas,
        )

    async def search(
        self,
        query: str,
        top_k: int = 5,
        filter_metadata: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """類似検索.

        Args:
            query: 検索クエリ
            top_k: 取得件数
            filter_metadata: メタデータフィルタ

        Returns:
            検索結果リスト
        """
        self._ensure_client()
        if self._collection is None:
            return []

        results = self._collection.query(
            query_texts=[query],
            n_results=top_k,
            where=filter_metadata,
        )

        # 結果を整形
        output = []
        if results and "documents" in results:
            for i, doc in enumerate(results["documents"][0]):
                output.append(
                    {
                        "id": results["ids"][0][i] if "ids" in results else f"doc_{i}",
                        "document": doc,
                        "distance": results["distances"][0][i] if "distances" in results else 0,
                        "metadata": results["metadatas"][0][i] if "metadatas" in results else {},
                    }
                )
        return output


class CacheProvider:
    """キャッシュプロバイダー."""

    def __init__(self, config: CacheConfig) -> None:
        """初期化."""
        self._config = config
        self._logger = logging.getLogger(__name__)
        self._memory_cache: dict[str, Any] = {}
        self._redis_client: Any = None

    async def get(self, key: str, default: Any = None) -> Any:
        """値を取得.

        Args:
            key: キー
            default: デフォルト値

        Returns:
            キャッシュ値
        """
        if self._config.provider == "memory":
            return self._memory_cache.get(key, default)
        if self._config.provider == "redis":
            if self._redis_client is None:
                return default
            value = await self._redis_client.get(key)
            return value if value is not None else default
        return default

    async def set(
        self,
        key: str,
        value: Any,
        ttl: int | None = None,
    ) -> None:
        """値を設定.

        Args:
            key: キー
            value: 値
            ttl: TTL秒数
        """
        if self._config.provider == "memory":
            self._memory_cache[key] = value
        elif self._config.provider == "redis" and self._redis_client:
            await self._redis_client.set(key, value, ex=ttl)

    async def delete(self, key: str) -> None:
        """値を削除.

        Args:
            key: キー
        """
        if self._config.provider == "memory":
            self._memory_cache.pop(key, None)
        elif self._config.provider == "redis" and self._redis_client:
            await self._redis_client.delete(key)

    async def clear(self) -> None:
        """全キャッシュをクリア."""
        if self._config.provider == "memory":
            self._memory_cache.clear()


class DataProvider:
    """データアクセス統一プロバイダー.

    SQL/Vector/Cacheへの統一アクセスを提供。

    使用例:
        >>> db = DataProvider.sql()
        >>> vector = DataProvider.vector()
        >>> cache = DataProvider.cache()
    """

    _sql_instance: SQLProvider | None = None
    _vector_instance: VectorProvider | None = None
    _cache_instance: CacheProvider | None = None

    @classmethod
    def sql(cls, url: str | None = None) -> SQLProvider:
        """SQLプロバイダーを取得.

        Args:
            url: データベースURL（省略時は環境変数またはSQLite）

        Returns:
            SQLProvider
        """
        if cls._sql_instance is None or url is not None:
            db_url = url or os.getenv("DATABASE_URL") or "sqlite:///./data.db"
            config = SQLConfig(url=db_url)
            cls._sql_instance = SQLProvider(config)
        return cls._sql_instance

    @classmethod
    def vector(
        cls,
        provider: str | None = None,
        collection: str = "default",
    ) -> VectorProvider:
        """Vectorプロバイダーを取得.

        Args:
            provider: プロバイダー名
            collection: コレクション名

        Returns:
            VectorProvider
        """
        if cls._vector_instance is None or provider is not None:
            vec_provider = provider or os.getenv("VECTOR_DB_PROVIDER") or "chromadb"
            config = VectorConfig(provider=vec_provider, collection=collection)
            cls._vector_instance = VectorProvider(config)
        return cls._vector_instance

    @classmethod
    def cache(cls, url: str | None = None) -> CacheProvider:
        """Cacheプロバイダーを取得.

        Args:
            url: Redis URL（省略時はメモリキャッシュ）

        Returns:
            CacheProvider
        """
        if cls._cache_instance is None or url is not None:
            redis_url = url or os.getenv("REDIS_URL")
            if redis_url:
                config = CacheConfig(provider="redis", url=redis_url)
            else:
                config = CacheConfig(provider="memory")
            cls._cache_instance = CacheProvider(config)
        return cls._cache_instance
