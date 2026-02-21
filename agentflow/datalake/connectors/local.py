"""LocalFileConnector - ローカルファイルシステムコネクタ.

ローカルファイルシステムへのアクセスを提供。

使用例:
    >>> connector = LocalFileConnector()
    >>> items = await connector.list("/data/reports")
    >>> result = await connector.read("/data/reports/q4.csv")
"""

import fnmatch
import logging
import mimetypes
from collections.abc import AsyncIterator
from datetime import UTC, datetime
from pathlib import Path

from pydantic import Field

from agentflow.datalake.connector import ConnectorConfig, DataConnector
from agentflow.datalake.core import DataItem, ReadResult


logger = logging.getLogger(__name__)


class LocalFileConfig(ConnectorConfig):
    """ローカルファイルコネクタ設定.

    Attributes:
        base_path: ベースパス（セキュリティ制限）
        follow_symlinks: シンボリックリンクを追跡するか
        encoding: デフォルトエンコーディング
    """

    base_path: str | None = Field(default=None, description="ベースパス（Noneで制限なし）")
    follow_symlinks: bool = Field(default=True, description="シンボリックリンクを追跡")
    encoding: str = Field(default="utf-8", description="デフォルトエンコーディング")


class LocalFileConnector(DataConnector):
    """ローカルファイルシステムコネクタ.

    file:// スキームでローカルファイルにアクセス。

    使用例:
        >>> connector = LocalFileConnector()
        >>> items = await connector.list("/data")
        >>> result = await connector.read("/data/file.txt")
    """

    def __init__(self, config: LocalFileConfig | None = None) -> None:
        """初期化.

        Args:
            config: コネクタ設定
        """
        self._config = config or LocalFileConfig()
        self._base_path = Path(self._config.base_path) if self._config.base_path else None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "file"

    def _resolve_path(self, path: str) -> Path:
        """パスを解決.

        Args:
            path: パス文字列

        Returns:
            解決済みPath

        Raises:
            ValueError: ベースパス外へのアクセス
        """
        # 先頭の / を除去（Windows対応）
        if path.startswith("/") and len(path) > 2 and path[2] == ":":
            path = path[1:]

        resolved = Path(path).resolve()

        # ベースパス制限
        if self._base_path:
            try:
                resolved.relative_to(self._base_path)
            except ValueError as e:
                msg = f"Access denied: {path} is outside base path {self._base_path}"
                raise ValueError(msg) from e

        return resolved

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """ディレクトリ内のファイル一覧.

        Args:
            path: ディレクトリパス
            recursive: 再帰的に取得
            pattern: glob パターン
            limit: 最大件数

        Returns:
            DataItemのリスト
        """
        resolved = self._resolve_path(path)
        items: list[DataItem] = []

        if not resolved.exists():
            logger.warning(f"Path does not exist: {resolved}")
            return items

        if not resolved.is_dir():
            logger.warning(f"Path is not a directory: {resolved}")
            return items

        iterator = resolved.rglob("*") if recursive else resolved.iterdir()

        for entry in iterator:
            if limit and len(items) >= limit:
                break

            # パターンフィルタ
            if pattern and not fnmatch.fnmatch(entry.name, pattern):
                continue

            # シンボリックリンク処理
            if entry.is_symlink() and not self._config.follow_symlinks:
                continue

            try:
                stat = entry.stat(follow_symlinks=self._config.follow_symlinks)
                items.append(
                    DataItem(
                        uri=f"file://{entry}",
                        name=entry.name,
                        size=stat.st_size if entry.is_file() else None,
                        modified_at=datetime.fromtimestamp(stat.st_mtime, tz=UTC),
                        content_type=mimetypes.guess_type(str(entry))[0],
                        is_directory=entry.is_dir(),
                    )
                )
            except (OSError, PermissionError) as e:
                logger.warning(f"Cannot access {entry}: {e}")

        return items

    async def read(self, path: str) -> ReadResult:
        """ファイル読み取り.

        Args:
            path: ファイルパス

        Returns:
            ReadResult
        """
        resolved = self._resolve_path(path)

        if not resolved.exists():
            msg = f"File not found: {path}"
            raise FileNotFoundError(msg)

        if resolved.is_dir():
            msg = f"Cannot read directory: {path}"
            raise IsADirectoryError(msg)

        content = resolved.read_bytes()
        content_type = mimetypes.guess_type(str(resolved))[0] or "application/octet-stream"

        return ReadResult(
            uri=f"file://{resolved}",
            content=content,
            content_type=content_type,
            size=len(content),
        )

    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """ファイル書き込み.

        Args:
            path: ファイルパス
            content: 書き込み内容
            content_type: Content-Type
            metadata: メタデータ（ローカルでは無視）

        Returns:
            DataItem
        """
        resolved = self._resolve_path(path)

        # 親ディレクトリ作成
        resolved.parent.mkdir(parents=True, exist_ok=True)

        # 書き込み
        if isinstance(content, str):
            resolved.write_text(content, encoding=self._config.encoding)
            size = len(content.encode(self._config.encoding))
        else:
            resolved.write_bytes(content)
            size = len(content)

        logger.debug(f"Written {size} bytes to {resolved}")

        return DataItem(
            uri=f"file://{resolved}",
            name=resolved.name,
            size=size,
            modified_at=datetime.now(UTC),
            content_type=content_type or mimetypes.guess_type(str(resolved))[0],
        )

    async def exists(self, path: str) -> bool:
        """ファイル存在確認.

        Args:
            path: パス

        Returns:
            存在する場合True
        """
        try:
            resolved = self._resolve_path(path)
            return resolved.exists()
        except ValueError:
            return False

    async def delete(self, path: str) -> bool:
        """ファイル削除.

        Args:
            path: パス

        Returns:
            削除成功の場合True
        """
        resolved = self._resolve_path(path)

        if not resolved.exists():
            return False

        if resolved.is_dir():
            import shutil

            shutil.rmtree(resolved)
        else:
            resolved.unlink()

        logger.debug(f"Deleted {resolved}")
        return True

    async def stream(
        self,
        path: str,
        chunk_size: int = 8192,
    ) -> AsyncIterator[bytes]:
        """ストリーミング読み取り.

        Args:
            path: ファイルパス
            chunk_size: チャンクサイズ

        Yields:
            バイトチャンク
        """
        resolved = self._resolve_path(path)

        if not resolved.exists():
            msg = f"File not found: {path}"
            raise FileNotFoundError(msg)

        try:
            import aiofiles

            async with aiofiles.open(resolved, "rb") as f:
                while True:
                    chunk = await f.read(chunk_size)
                    if not chunk:
                        break
                    yield chunk
        except ImportError:
            # aiofiles がない場合は同期読み取り
            with open(resolved, "rb") as f:
                while True:
                    chunk = f.read(chunk_size)
                    if not chunk:
                        break
                    yield chunk
