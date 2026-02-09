"""DataLake Core - 統一データアクセスAPI.

全てのデータソースへの統一アクセスを提供するコアモジュール。

使用例:
    >>> from agentflow.datalake import DataLake, get_datalake
    >>>
    >>> # シングルトン取得
    >>> dl = get_datalake()
    >>>
    >>> # ファイル一覧
    >>> items = await dl.list("s3://my-bucket/data/")
    >>>
    >>> # ファイル読み取り
    >>> result = await dl.read("file:///data/report.csv")
    >>> print(result.content)  # パース済みデータ
    >>>
    >>> # ファイル書き込み
    >>> await dl.write("s3://my-bucket/output.json", {"key": "value"})
"""

import logging
from collections.abc import AsyncIterator
from datetime import datetime
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

from pydantic import BaseModel, Field

from agentflow.datalake.auth import AuthProvider
from agentflow.datalake.connector import DataConnector
from agentflow.datalake.format_handlers import get_format_handler


logger = logging.getLogger(__name__)

# グローバルシングルトン
_datalake_instance: "DataLake | None" = None


class DataItem(BaseModel):
    """データアイテム.

    Attributes:
        uri: 完全URI
        name: ファイル/オブジェクト名
        size: サイズ（バイト）
        modified_at: 最終更新日時
        content_type: Content-Type
        metadata: メタデータ
        is_directory: ディレクトリかどうか
    """

    uri: str = Field(description="完全URI")
    name: str = Field(description="ファイル/オブジェクト名")
    size: int | None = Field(default=None, description="サイズ（バイト）")
    modified_at: datetime | None = Field(default=None, description="最終更新日時")
    content_type: str | None = Field(default=None, description="Content-Type")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    is_directory: bool = Field(default=False, description="ディレクトリかどうか")


class ReadResult(BaseModel):
    """読み取り結果.

    Attributes:
        uri: 読み取り元URI
        content: コンテンツ（バイナリ/文字列/パース済みデータ）
        content_type: Content-Type
        size: サイズ（バイト）
        metadata: メタデータ
    """

    uri: str = Field(description="読み取り元URI")
    content: bytes | str | dict | list | Any = Field(description="コンテンツ")
    content_type: str = Field(description="Content-Type")
    size: int = Field(description="サイズ（バイト）")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class WriteResult(BaseModel):
    """書き込み結果.

    Attributes:
        uri: 書き込み先URI
        size: サイズ（バイト）
        etag: ETag（あれば）
    """

    uri: str = Field(description="書き込み先URI")
    size: int = Field(description="サイズ（バイト）")
    etag: str | None = Field(default=None, description="ETag")


class DataLake:
    """データレイク統一アクセス.

    全てのデータソースへの統一アクセスを提供。

    使用例:
        >>> dl = DataLake()
        >>> dl.register_connector(LocalFileConnector())
        >>> dl.register_connector(S3Connector())
        >>> items = await dl.list("s3://bucket/path/")
    """

    def __init__(
        self,
        auth_provider: AuthProvider | None = None,
        auto_parse: bool = True,
        cache_ttl: int = 300,
    ) -> None:
        """初期化.

        Args:
            auth_provider: 認証プロバイダ（外部注入）
            auto_parse: 自動パースを有効にするか
            cache_ttl: キャッシュTTL（秒）
        """
        self._connectors: dict[str, DataConnector] = {}
        self._auth_provider = auth_provider
        self._auto_parse = auto_parse
        self._cache_ttl = cache_ttl
        self._cache: dict[str, tuple[Any, float]] = {}

    def register_connector(self, connector: DataConnector) -> None:
        """コネクタを登録.

        Args:
            connector: 登録するコネクタ
        """
        self._connectors[connector.scheme] = connector
        logger.info(f"Registered connector: {connector.scheme}://")

    def get_connector(self, scheme: str) -> DataConnector | None:
        """コネクタを取得.

        Args:
            scheme: URIスキーム

        Returns:
            DataConnector または None
        """
        return self._connectors.get(scheme)

    @property
    def registered_schemes(self) -> list[str]:
        """登録済みスキーム一覧.

        Returns:
            スキームのリスト
        """
        return list(self._connectors.keys())

    async def list(
        self,
        uri: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """URIのアイテム一覧を取得.

        Args:
            uri: 対象URI
            recursive: 再帰的に取得するか
            pattern: フィルタパターン
            limit: 最大取得件数

        Returns:
            DataItemのリスト
        """
        connector, path = self._resolve(uri)
        return await connector.list(
            path, recursive=recursive, pattern=pattern, limit=limit
        )

    async def read(
        self,
        uri: str,
        parse: bool | None = None,
        **kwargs: Any,
    ) -> ReadResult:
        """URIを読み取り.

        Args:
            uri: 対象URI
            parse: パースするか（Noneでauto_parse設定を使用）
            **kwargs: パースオプション

        Returns:
            ReadResult
        """
        connector, path = self._resolve(uri)
        result = await connector.read(path)

        should_parse = parse if parse is not None else self._auto_parse

        if should_parse:
            ext = Path(path).suffix.lower()
            handler = get_format_handler(ext)
            if handler and isinstance(result.content, bytes):
                try:
                    result.content = await handler.parse(result.content, **kwargs)
                except Exception as e:
                    logger.warning(f"Failed to parse {uri}: {e}")

        return result

    async def write(
        self,
        uri: str,
        content: Any,
        content_type: str | None = None,
        **kwargs: Any,
    ) -> WriteResult:
        """URIに書き込み.

        Args:
            uri: 対象URI
            content: 書き込み内容
            content_type: Content-Type
            **kwargs: シリアライズオプション

        Returns:
            WriteResult
        """
        connector, path = self._resolve(uri)

        # 非バイナリデータはシリアライズ
        if not isinstance(content, (bytes, str)):
            ext = Path(path).suffix.lower()
            handler = get_format_handler(ext)
            if handler:
                content = await handler.serialize(content, **kwargs)
            else:
                # デフォルトでJSON
                import json
                content = json.dumps(content, ensure_ascii=False, indent=2).encode()

        item = await connector.write(path, content, content_type=content_type)
        return WriteResult(uri=uri, size=item.size or 0, etag=item.metadata.get("etag"))

    async def exists(self, uri: str) -> bool:
        """URIの存在を確認.

        Args:
            uri: 対象URI

        Returns:
            存在する場合True
        """
        connector, path = self._resolve(uri)
        return await connector.exists(path)

    async def delete(self, uri: str) -> bool:
        """URIを削除.

        Args:
            uri: 対象URI

        Returns:
            削除成功の場合True
        """
        connector, path = self._resolve(uri)
        return await connector.delete(path)

    async def stream(
        self,
        uri: str,
        chunk_size: int = 8192,
    ) -> AsyncIterator[bytes]:
        """ストリーミング読み取り.

        Args:
            uri: 対象URI
            chunk_size: チャンクサイズ

        Yields:
            バイトチャンク
        """
        connector, path = self._resolve(uri)
        async for chunk in connector.stream(path, chunk_size=chunk_size):
            yield chunk

    async def copy(
        self,
        src_uri: str,
        dst_uri: str,
        **kwargs: Any,
    ) -> WriteResult:
        """URIをコピー.

        Args:
            src_uri: コピー元URI
            dst_uri: コピー先URI
            **kwargs: 追加オプション

        Returns:
            WriteResult
        """
        result = await self.read(src_uri, parse=False)
        return await self.write(dst_uri, result.content, **kwargs)

    def _resolve(self, uri: str) -> tuple[DataConnector, str]:
        """URIからコネクタとパスを解決.

        Args:
            uri: URI

        Returns:
            (コネクタ, パス) のタプル

        Raises:
            ValueError: 不明なスキームの場合
        """
        parsed = urlparse(uri)
        scheme = parsed.scheme or "file"

        if scheme not in self._connectors:
            available = ", ".join(self._connectors.keys()) or "none"
            msg = f"Unknown scheme: {scheme}. Available: {available}"
            raise ValueError(
                msg
            )

        # パス構築
        path = parsed.path
        if parsed.netloc:
            path = f"{parsed.netloc}{path}"

        return self._connectors[scheme], path


def get_datalake() -> DataLake:
    """DataLakeシングルトンを取得.

    Returns:
        DataLake インスタンス
    """
    global _datalake_instance

    if _datalake_instance is None:
        _datalake_instance = DataLake()
        logger.info("Created DataLake instance")

        # デフォルトコネクタを登録
        _register_default_connectors(_datalake_instance)

    return _datalake_instance


def reset_datalake() -> None:
    """DataLakeシングルトンをリセット（テスト用）."""
    global _datalake_instance
    _datalake_instance = None


def _register_default_connectors(datalake: DataLake) -> None:
    """デフォルトコネクタを登録.

    Args:
        datalake: DataLakeインスタンス
    """
    # LocalFileConnector（常に登録）
    try:
        from agentflow.datalake.connectors.local import LocalFileConnector
        datalake.register_connector(LocalFileConnector())
    except ImportError:
        logger.debug("LocalFileConnector not available")

    # S3Connector（boto3がある場合）
    try:
        from agentflow.datalake.connectors.s3 import S3Connector
        datalake.register_connector(S3Connector())
    except ImportError:
        logger.debug("S3Connector not available (boto3 not installed)")

    # RestAPIConnector（常に登録）
    try:
        from agentflow.datalake.connectors.rest import RestAPIConnector
        datalake.register_connector(RestAPIConnector())
    except ImportError:
        logger.debug("RestAPIConnector not available")

