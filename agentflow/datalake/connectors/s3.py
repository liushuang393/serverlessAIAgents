from __future__ import annotations

"""S3Connector - AWS S3 / MinIO 互換コネクタ.

S3互換ストレージへのアクセスを提供。
AWS S3、MinIO、その他S3互換ストレージをサポート。

使用例:
    >>> connector = S3Connector()
    >>> items = await connector.list("my-bucket/data/")
    >>> result = await connector.read("my-bucket/data/file.csv")

環境変数:
    AWS_ACCESS_KEY_ID: アクセスキー
    AWS_SECRET_ACCESS_KEY: シークレットキー
    AWS_REGION: リージョン（デフォルト: us-east-1）
    S3_ENDPOINT_URL: エンドポイントURL（MinIO用）
"""

import logging
import mimetypes
import os
from collections.abc import AsyncIterator
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from pydantic import Field

from agentflow.datalake.connector import ConnectorConfig, DataConnector
from agentflow.datalake.core import DataItem, ReadResult


if TYPE_CHECKING:
    from types_aiobotocore_s3 import S3Client

logger = logging.getLogger(__name__)


class S3Config(ConnectorConfig):
    """S3コネクタ設定.

    Attributes:
        endpoint_url: エンドポイントURL（MinIO用）
        region: リージョン
        access_key_id: アクセスキー
        secret_access_key: シークレットキー
        use_ssl: SSLを使用するか
    """

    endpoint_url: str | None = Field(default=None, description="エンドポイントURL（MinIO用）")
    region: str = Field(default="us-east-1", description="リージョン")
    access_key_id: str | None = Field(default=None, description="アクセスキー")
    secret_access_key: str | None = Field(default=None, description="シークレットキー")
    use_ssl: bool = Field(default=True, description="SSLを使用")


class S3Connector(DataConnector):
    """AWS S3 / MinIO 互換コネクタ.

    s3:// スキームでS3互換ストレージにアクセス。

    使用例:
        >>> # AWS S3
        >>> connector = S3Connector()
        >>>
        >>> # MinIO
        >>> connector = S3Connector(S3Config(
        ...     endpoint_url="http://localhost:9000",
        ...     access_key_id="minioadmin",
        ...     secret_access_key="minioadmin",
        ... ))
    """

    def __init__(self, config: S3Config | None = None) -> None:
        """初期化.

        Args:
            config: コネクタ設定
        """
        self._config = config or S3Config()
        self._client: S3Client | None = None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "s3"

    def _parse_path(self, path: str) -> tuple[str, str]:
        """パスをバケット名とキーに分割.

        Args:
            path: パス（bucket/key/to/object）

        Returns:
            (bucket, key) のタプル
        """
        parts = path.strip("/").split("/", 1)
        bucket = parts[0]
        key = parts[1] if len(parts) > 1 else ""
        return bucket, key

    async def _get_client(self) -> "S3Client":
        """S3クライアントを取得.

        Returns:
            S3Client

        Raises:
            ImportError: aiobotocore がインストールされていない場合
        """
        if self._client is None:
            try:
                from aiobotocore.session import get_session
            except ImportError as e:
                msg = "aiobotocore is required for S3 support. Install with: pip install aiobotocore"
                raise ImportError(msg) from e

            session = get_session()

            # 認証情報
            access_key = self._config.access_key_id or os.getenv("AWS_ACCESS_KEY_ID")
            secret_key = self._config.secret_access_key or os.getenv("AWS_SECRET_ACCESS_KEY")
            region = self._config.region or os.getenv("AWS_REGION", "us-east-1")
            endpoint = self._config.endpoint_url or os.getenv("S3_ENDPOINT_URL")

            client_config = {
                "region_name": region,
            }
            if access_key and secret_key:
                client_config["aws_access_key_id"] = access_key
                client_config["aws_secret_access_key"] = secret_key
            if endpoint:
                client_config["endpoint_url"] = endpoint

            ctx = session.create_client("s3", **client_config)
            self._client = await ctx.__aenter__()

        return self._client

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """バケット/プレフィックス内のオブジェクト一覧.

        Args:
            path: パス（bucket/prefix）
            recursive: 再帰的に取得
            pattern: フィルタパターン
            limit: 最大件数

        Returns:
            DataItemのリスト
        """
        import fnmatch

        client = await self._get_client()
        bucket, prefix = self._parse_path(path)
        items: list[DataItem] = []

        paginator = client.get_paginator("list_objects_v2")
        params: dict[str, Any] = {"Bucket": bucket}

        if prefix:
            params["Prefix"] = prefix
        if not recursive:
            params["Delimiter"] = "/"

        async for page in paginator.paginate(**params):
            # ディレクトリ（CommonPrefixes）
            for cp in page.get("CommonPrefixes", []):
                prefix_path = cp["Prefix"]
                name = prefix_path.rstrip("/").split("/")[-1]
                items.append(
                    DataItem(
                        uri=f"s3://{bucket}/{prefix_path}",
                        name=name,
                        is_directory=True,
                    )
                )

            # ファイル（Contents）
            for obj in page.get("Contents", []):
                key = obj["Key"]
                name = key.split("/")[-1]

                if pattern and not fnmatch.fnmatch(name, pattern):
                    continue

                items.append(
                    DataItem(
                        uri=f"s3://{bucket}/{key}",
                        name=name,
                        size=obj.get("Size"),
                        modified_at=obj.get("LastModified"),
                        content_type=mimetypes.guess_type(key)[0],
                        metadata={"etag": obj.get("ETag", "").strip('"')},
                    )
                )

                if limit and len(items) >= limit:
                    break

            if limit and len(items) >= limit:
                break

        return items[:limit] if limit else items

    async def read(self, path: str) -> ReadResult:
        """オブジェクト読み取り.

        Args:
            path: パス（bucket/key）

        Returns:
            ReadResult
        """
        client = await self._get_client()
        bucket, key = self._parse_path(path)

        response = await client.get_object(Bucket=bucket, Key=key)

        async with response["Body"] as stream:
            content = await stream.read()

        return ReadResult(
            uri=f"s3://{bucket}/{key}",
            content=content,
            content_type=response.get("ContentType", "application/octet-stream"),
            size=response.get("ContentLength", len(content)),
            metadata={
                "etag": response.get("ETag", "").strip('"'),
                "last_modified": str(response.get("LastModified", "")),
            },
        )

    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """オブジェクト書き込み.

        Args:
            path: パス（bucket/key）
            content: 書き込み内容
            content_type: Content-Type
            metadata: メタデータ

        Returns:
            DataItem
        """
        client = await self._get_client()
        bucket, key = self._parse_path(path)

        if isinstance(content, str):
            content = content.encode("utf-8")

        params: dict[str, Any] = {
            "Bucket": bucket,
            "Key": key,
            "Body": content,
        }
        if content_type:
            params["ContentType"] = content_type
        elif ct := mimetypes.guess_type(key)[0]:
            params["ContentType"] = ct
        if metadata:
            params["Metadata"] = metadata

        response = await client.put_object(**params)

        logger.debug(f"Written {len(content)} bytes to s3://{bucket}/{key}")

        return DataItem(
            uri=f"s3://{bucket}/{key}",
            name=key.split("/")[-1],
            size=len(content),
            modified_at=datetime.now(UTC),
            content_type=content_type,
            metadata={"etag": response.get("ETag", "").strip('"')},
        )

    async def exists(self, path: str) -> bool:
        """オブジェクト存在確認.

        Args:
            path: パス

        Returns:
            存在する場合True
        """
        client = await self._get_client()
        bucket, key = self._parse_path(path)

        try:
            await client.head_object(Bucket=bucket, Key=key)
            return True
        except client.exceptions.ClientError:
            return False

    async def delete(self, path: str) -> bool:
        """オブジェクト削除.

        Args:
            path: パス

        Returns:
            削除成功の場合True
        """
        client = await self._get_client()
        bucket, key = self._parse_path(path)

        try:
            await client.delete_object(Bucket=bucket, Key=key)
            logger.debug(f"Deleted s3://{bucket}/{key}")
            return True
        except Exception as e:
            logger.warning(f"Failed to delete s3://{bucket}/{key}: {e}")
            return False

    async def stream(
        self,
        path: str,
        chunk_size: int = 8192,
    ) -> AsyncIterator[bytes]:
        """ストリーミング読み取り.

        Args:
            path: パス
            chunk_size: チャンクサイズ

        Yields:
            バイトチャンク
        """
        client = await self._get_client()
        bucket, key = self._parse_path(path)

        response = await client.get_object(Bucket=bucket, Key=key)

        async with response["Body"] as stream:
            async for chunk in stream.iter_chunks(chunk_size):
                yield chunk

    async def close(self) -> None:
        """クライアントをクローズ."""
        if self._client:
            await self._client.close()
            self._client = None
