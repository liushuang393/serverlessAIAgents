"""ベクトルデータベースのインターフェース定義."""

from abc import ABC, abstractmethod
from typing import Any

from agentflow.memory.types import MemoryEntry


class VectorDatabase(ABC):
    """ベクトルデータベースの抽象基底クラス.

    目的:
    - ベクトル埋め込みの保存と検索のインターフェースを定義
    - Pinecone/Weaviate/Qdrant/その他のDBを統一的に扱う

    実装必須メソッド:
    - connect(): データベースへの接続
    - disconnect(): データベースからの切断
    - upsert(): ベクトルの挿入/更新
    - search(): ベクトル類似度検索
    - delete(): ベクトルの削除
    - clear(): 全ベクトルの削除
    """

    @abstractmethod
    async def connect(self) -> None:
        """データベースに接続.

        Raises:
            ConnectionError: 接続に失敗した場合
        """

    @abstractmethod
    async def disconnect(self) -> None:
        """データベースから切断."""

    @abstractmethod
    async def upsert(
        self,
        entry: MemoryEntry,
        embedding: list[float],
    ) -> None:
        """ベクトルを挿入/更新.

        Args:
            entry: 記憶エントリ
            embedding: 埋め込みベクトル

        Raises:
            ValueError: 無効なエントリまたはベクトルの場合
            IOError: 挿入に失敗した場合
        """

    @abstractmethod
    async def search(
        self,
        query_embedding: list[float],
        limit: int = 10,
        min_similarity: float = 0.0,
        topic: str | None = None,
    ) -> list[tuple[MemoryEntry, float]]:
        """ベクトル類似度検索.

        Args:
            query_embedding: クエリベクトル
            limit: 最大取得数
            min_similarity: 最小類似度
            topic: トピック名（Noneの場合は全て）

        Returns:
            (記憶エントリ, 類似度スコア) のリスト

        Raises:
            IOError: 検索に失敗した場合
        """

    @abstractmethod
    async def delete(self, entry_id: str) -> bool:
        """ベクトルを削除.

        Args:
            entry_id: 記憶エントリID

        Returns:
            削除成功の場合True

        Raises:
            IOError: 削除に失敗した場合
        """

    @abstractmethod
    async def clear(self, topic: str | None = None) -> int:
        """ベクトルをクリア.

        Args:
            topic: トピック名（Noneの場合は全て）

        Returns:
            削除したベクトルの数

        Raises:
            IOError: クリアに失敗した場合
        """

    @abstractmethod
    def get_status(self) -> dict[str, Any]:
        """データベースの状態を取得.

        Returns:
            状態情報
        """
