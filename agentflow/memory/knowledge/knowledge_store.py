"""知識ストア - 抽象インターフェース.

長期知識記憶の統一インターフェース定義。
Spring Boot風の設計：インターフェース分離、実装切替可能。

設計原則:
- Protocol（構造的サブタイピング）による柔軟な実装
- 非同期対応
- 検索機能の標準化
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Protocol, runtime_checkable


if TYPE_CHECKING:
    from agentflow.memory.knowledge.types import (
        KnowledgeEntry,
        SearchResult,
        SearchType,
    )


@runtime_checkable
class KnowledgeStore(Protocol):
    """知識ストアの抽象インターフェース.

    長期知識記憶の保存・検索・更新・削除を定義。
    Memvid、インメモリ、その他のカスタム実装が可能。

    使用例:
        >>> store: KnowledgeStore = get_knowledge_store()
        >>> await store.connect()
        >>> await store.store(entry)
        >>> results = await store.search("質問", top_k=5)
        >>> await store.disconnect()
    """

    async def connect(self) -> None:
        """ストアに接続.

        ファイルの作成/オープン、バックエンドへの接続を実行。

        Raises:
            ConnectionError: 接続に失敗した場合
        """
        ...

    async def disconnect(self) -> None:
        """ストアから切断.

        リソースを解放し、必要に応じてデータを永続化。
        """
        ...

    async def store(self, entry: KnowledgeEntry) -> str:
        """知識エントリを保存.

        Args:
            entry: 保存する知識エントリ

        Returns:
            保存されたエントリのID

        Raises:
            ValueError: 無効なエントリの場合
            IOError: 保存に失敗した場合
        """
        ...

    async def store_batch(self, entries: list[KnowledgeEntry]) -> list[str]:
        """知識エントリを一括保存.

        Args:
            entries: 保存する知識エントリのリスト

        Returns:
            保存されたエントリのIDリスト

        Raises:
            ValueError: 無効なエントリが含まれる場合
            IOError: 保存に失敗した場合
        """
        ...

    async def search(
        self,
        query: str,
        top_k: int = 5,
        search_type: SearchType | None = None,
        tags: list[str] | None = None,
        min_score: float = 0.0,
    ) -> list[SearchResult]:
        """知識を検索.

        Args:
            query: 検索クエリ
            top_k: 最大取得件数
            search_type: 検索タイプ（None=デフォルト）
            tags: タグフィルター
            min_score: 最小スコア閾値

        Returns:
            検索結果のリスト（スコア降順）

        Raises:
            IOError: 検索に失敗した場合
        """
        ...

    async def get(self, entry_id: str) -> KnowledgeEntry | None:
        """IDで知識エントリを取得.

        Args:
            entry_id: エントリID

        Returns:
            知識エントリ（存在しない場合はNone）
        """
        ...

    async def update(self, entry: KnowledgeEntry) -> bool:
        """知識エントリを更新.

        Args:
            entry: 更新する知識エントリ（IDで特定）

        Returns:
            更新成功の場合True

        Raises:
            ValueError: エントリが存在しない場合
            IOError: 更新に失敗した場合
        """
        ...

    async def delete(self, entry_id: str) -> bool:
        """知識エントリを削除.

        Args:
            entry_id: 削除するエントリID

        Returns:
            削除成功の場合True

        Raises:
            IOError: 削除に失敗した場合
        """
        ...

    async def clear(self) -> int:
        """全知識エントリをクリア.

        Returns:
            削除したエントリ数

        Raises:
            IOError: クリアに失敗した場合
        """
        ...

    async def count(self) -> int:
        """知識エントリ数を取得.

        Returns:
            エントリ数
        """
        ...

    def get_provider_name(self) -> str:
        """プロバイダー名を取得.

        Returns:
            プロバイダー名（例：memvid, memory）
        """
        ...
