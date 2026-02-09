"""DataConnector - データコネクタ抽象基底クラス.

全てのデータソースコネクタが実装すべき統一インターフェースを定義。

設計原則:
    - 統一インターフェース: 全データソースに共通のAPI
    - プラグイン式: コネクタの追加が容易
    - 認証分離: 認証ロジックは外部注入可能
    - ストリーミング: 大容量データの段階読込
"""

from abc import ABC, abstractmethod
from collections.abc import AsyncIterator
from typing import TYPE_CHECKING

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from agentflow.datalake.core import DataItem, ReadResult


class ConnectorConfig(BaseModel):
    """コネクタ設定基底クラス.

    各コネクタは独自の設定を継承して定義する。

    Attributes:
        timeout: 接続タイムアウト（秒）
        retry_count: リトライ回数
        retry_delay: リトライ間隔（秒）
    """

    timeout: int = Field(default=30, description="接続タイムアウト（秒）")
    retry_count: int = Field(default=3, description="リトライ回数")
    retry_delay: float = Field(default=1.0, description="リトライ間隔（秒）")


class DataConnector(ABC):
    """データコネクタ抽象基底クラス.

    全てのデータソースコネクタが実装すべきインターフェース。

    実装例:
        >>> class MyConnector(DataConnector):
        ...     @property
        ...     def scheme(self) -> str:
        ...         return "myscheme"
        ...
        ...     async def list(self, path: str, **kwargs) -> list[DataItem]:
        ...         # 実装
        ...         pass
    """

    @property
    @abstractmethod
    def scheme(self) -> str:
        """URIスキームを返す.

        Returns:
            スキーム文字列（例: 's3', 'file', 'onedrive'）
        """
        ...

    @property
    def is_connected(self) -> bool:
        """接続状態を返す.

        Returns:
            接続済みの場合True
        """
        return True

    @abstractmethod
    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list["DataItem"]:
        """ディレクトリ/バケット内のアイテム一覧を取得.

        Args:
            path: パス（バケット名/ディレクトリパス）
            recursive: 再帰的に取得するか
            pattern: フィルタパターン（glob形式）
            limit: 最大取得件数

        Returns:
            DataItemのリスト
        """
        ...

    @abstractmethod
    async def read(self, path: str) -> "ReadResult":
        """ファイル/オブジェクトを読み取り.

        Args:
            path: パス

        Returns:
            ReadResult
        """
        ...

    @abstractmethod
    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> "DataItem":
        """ファイル/オブジェクトを書き込み.

        Args:
            path: パス
            content: 書き込み内容
            content_type: Content-Type
            metadata: メタデータ

        Returns:
            作成されたDataItem
        """
        ...

    @abstractmethod
    async def exists(self, path: str) -> bool:
        """パスの存在を確認.

        Args:
            path: パス

        Returns:
            存在する場合True
        """
        ...

    @abstractmethod
    async def delete(self, path: str) -> bool:
        """ファイル/オブジェクトを削除.

        Args:
            path: パス

        Returns:
            削除成功の場合True
        """
        ...

    async def stream(
        self,
        path: str,
        chunk_size: int = 8192,
    ) -> AsyncIterator[bytes]:
        """ストリーミング読み取り（大容量ファイル用）.

        デフォルト実装は read() を呼び出してチャンク分割。
        大容量対応が必要なコネクタはオーバーライドすること。

        Args:
            path: パス
            chunk_size: チャンクサイズ

        Yields:
            バイトチャンク
        """
        result = await self.read(path)
        content = (
            result.content
            if isinstance(result.content, bytes)
            else str(result.content).encode("utf-8")
        )
        for i in range(0, len(content), chunk_size):
            yield content[i : i + chunk_size]

