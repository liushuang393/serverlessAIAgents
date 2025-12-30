"""分散記憶バックエンドのインターフェース定義."""

from abc import ABC, abstractmethod
from typing import Any

from agentflow.memory.types import MemoryEntry


class MemoryBackend(ABC):
    """記憶バックエンドの抽象基底クラス.

    目的:
    - 記憶の永続化と取得のインターフェースを定義
    - Redis/PostgreSQL/その他のバックエンドを統一的に扱う

    実装必須メソッド:
    - connect(): バックエンドへの接続
    - disconnect(): バックエンドからの切断
    - save(): 記憶の保存
    - load(): 記憶の読み込み
    - delete(): 記憶の削除
    - search(): 記憶の検索
    - exists(): 記憶の存在確認
    """

    @abstractmethod
    async def connect(self) -> None:
        """バックエンドに接続.

        Raises:
            ConnectionError: 接続に失敗した場合
        """
        pass

    @abstractmethod
    async def disconnect(self) -> None:
        """バックエンドから切断."""
        pass

    @abstractmethod
    async def save(self, entry: MemoryEntry) -> None:
        """記憶を保存.

        Args:
            entry: 記憶エントリ

        Raises:
            ValueError: 無効なエントリの場合
            IOError: 保存に失敗した場合
        """
        pass

    @abstractmethod
    async def load(self, entry_id: str) -> MemoryEntry | None:
        """記憶を読み込み.

        Args:
            entry_id: 記憶エントリID

        Returns:
            記憶エントリ（存在しない場合はNone）

        Raises:
            IOError: 読み込みに失敗した場合
        """
        pass

    @abstractmethod
    async def delete(self, entry_id: str) -> bool:
        """記憶を削除.

        Args:
            entry_id: 記憶エントリID

        Returns:
            削除成功の場合True

        Raises:
            IOError: 削除に失敗した場合
        """
        pass

    @abstractmethod
    async def search(
        self,
        topic: str | None = None,
        limit: int = 10,
        min_importance: float = 0.0,
    ) -> list[MemoryEntry]:
        """記憶を検索.

        Args:
            topic: トピック名（Noneの場合は全て）
            limit: 最大取得数
            min_importance: 最小重要度

        Returns:
            記憶エントリのリスト

        Raises:
            IOError: 検索に失敗した場合
        """
        pass

    @abstractmethod
    async def exists(self, entry_id: str) -> bool:
        """記憶の存在を確認.

        Args:
            entry_id: 記憶エントリID

        Returns:
            存在する場合True

        Raises:
            IOError: 確認に失敗した場合
        """
        pass

    @abstractmethod
    async def count(self, topic: str | None = None) -> int:
        """記憶の数を取得.

        Args:
            topic: トピック名（Noneの場合は全て）

        Returns:
            記憶の数

        Raises:
            IOError: 取得に失敗した場合
        """
        pass

    @abstractmethod
    async def clear(self, topic: str | None = None) -> int:
        """記憶をクリア.

        Args:
            topic: トピック名（Noneの場合は全て）

        Returns:
            削除した記憶の数

        Raises:
            IOError: クリアに失敗した場合
        """
        pass

    @abstractmethod
    def get_status(self) -> dict[str, Any]:
        """バックエンドの状態を取得.

        Returns:
            状態情報
        """
        pass

