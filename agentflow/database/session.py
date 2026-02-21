"""統一データベースセッション管理.

目的:
    SQLAlchemy エンジン/セッションファクトリのライフサイクルを管理する。
    各アプリは DatabaseManager インスタンスを作成するだけで、
    エンジン作成・セッション提供・クローズを自動化できる。

使用例:
    >>> from agentflow.database import DatabaseConfig, DatabaseManager
    >>> from my_app.models import Base
    >>>
    >>> db = DatabaseManager(
    ...     config=DatabaseConfig(url="sqlite+aiosqlite:///./app.db"),
    ...     base=Base,
    ... )
    >>> await db.init()
    >>> async with db.session() as session:
    ...     result = await session.execute(query)
    >>> await db.close()
"""

from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING, Any

from sqlalchemy import MetaData, create_engine
from sqlalchemy.ext.asyncio import (
    AsyncEngine,
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)
from sqlalchemy.orm import Session, sessionmaker

from agentflow.database.url_utils import is_sqlite, to_async_url, to_sync_url


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator

    from sqlalchemy.engine import Engine

    from agentflow.database.config import DatabaseConfig


_logger = logging.getLogger(__name__)


class DatabaseManager:
    """統一データベースセッション管理.

    async/sync 両モードに対応し、SQLite/PostgreSQL/MySQL を統一的に扱う。
    """

    def __init__(
        self,
        config: DatabaseConfig,
        metadata: MetaData,
        *,
        force_sync: bool = False,
    ) -> None:
        """初期化.

        Args:
            config: データベース設定
            metadata: SQLAlchemy MetaData（Base.metadata）
            force_sync: 強制同期モード（SQLite 用）
        """
        self._config = config
        self._metadata = metadata
        self._force_sync = force_sync

        # 内部状態
        self._async_engine: AsyncEngine | None = None
        self._async_session_factory: async_sessionmaker[AsyncSession] | None = None
        self._sync_engine: Engine | None = None
        self._sync_session_factory: sessionmaker[Session] | None = None
        self._initialized = False

    @property
    def is_sync_mode(self) -> bool:
        """同期モードかどうか."""
        url = self._config.resolve_url()
        return self._force_sync and is_sqlite(url)

    @property
    def is_initialized(self) -> bool:
        """初期化済みかどうか."""
        return self._initialized

    @property
    def resolved_url(self) -> str:
        """環境変数解決済みの DB URL を取得."""
        return self._config.resolve_url()

    async def init(self) -> None:
        """エンジン/セッションファクトリを初期化."""
        if self._initialized:
            return

        url = self._config.resolve_url()
        echo = self._config.resolve_echo()

        if self.is_sync_mode:
            self._init_sync(to_sync_url(url), echo)
        else:
            self._init_async(to_async_url(url), echo)

        self._initialized = True
        _logger.info("DatabaseManager 初期化完了: %s", url[:50])

    def _init_async(self, url: str, echo: bool) -> None:
        """非同期エンジンを初期化."""
        kwargs: dict[str, Any] = {"echo": echo}
        if not is_sqlite(url):
            kwargs["pool_size"] = self._config.pool_size
            kwargs["max_overflow"] = self._config.max_overflow
            kwargs["pool_pre_ping"] = self._config.pool_pre_ping

        self._async_engine = create_async_engine(url, **kwargs)
        self._async_session_factory = async_sessionmaker(
            bind=self._async_engine,
            class_=AsyncSession,
            expire_on_commit=self._config.expire_on_commit,
        )

    def _init_sync(self, url: str, echo: bool) -> None:
        """同期エンジンを初期化（SQLite 用）."""
        connect_args = self._config.connect_args.copy()
        if is_sqlite(url) and "check_same_thread" not in connect_args:
            connect_args["check_same_thread"] = False

        self._sync_engine = create_engine(
            url,
            echo=echo,
            connect_args=connect_args,
            future=True,
        )
        self._sync_session_factory = sessionmaker(
            bind=self._sync_engine,
            expire_on_commit=self._config.expire_on_commit,
        )

    async def close(self) -> None:
        """接続をクローズ."""
        if self._async_engine:
            await self._async_engine.dispose()
            self._async_engine = None
            self._async_session_factory = None

        if self._sync_engine:
            self._sync_engine.dispose()
            self._sync_engine = None
            self._sync_session_factory = None

        self._initialized = False
        _logger.info("DatabaseManager クローズ完了")

    @asynccontextmanager
    async def session(self) -> AsyncGenerator[AsyncSession]:
        """非同期セッションを取得（コンテキストマネージャ）.

        Yields:
            AsyncSession インスタンス

        Raises:
            RuntimeError: 初期化前または同期モードの場合
        """
        if not self._initialized:
            await self.init()

        if self._async_session_factory is None:
            msg = "非同期セッションファクトリが未初期化（同期モード?）"
            raise RuntimeError(msg)

        async with self._async_session_factory() as session:
            try:
                yield session
                await session.commit()
            except Exception:
                await session.rollback()
                raise

    def session_sync(self) -> Session:
        """同期セッションを取得（SQLite 同期モード用）.

        Returns:
            Session インスタンス

        Raises:
            RuntimeError: 初期化前または非同期モードの場合
        """
        if self._sync_session_factory is None:
            msg = "同期セッションファクトリが未初期化（非同期モード?）"
            raise RuntimeError(msg)
        return self._sync_session_factory()

    async def create_all_tables(self) -> None:
        """テーブルを直接作成（開発/テスト用）.

        Alembic マイグレーション推奨。開発時の初期セットアップ用。
        """
        if not self._initialized:
            await self.init()

        if self._sync_engine is not None:
            self._metadata.create_all(bind=self._sync_engine)
            _logger.info("テーブル作成完了（同期モード）")
            return

        if self._async_engine is not None:
            async with self._async_engine.begin() as conn:
                await conn.run_sync(self._metadata.create_all)
            _logger.info("テーブル作成完了（非同期モード）")
            return

        msg = "エンジンが未初期化"
        raise RuntimeError(msg)

    def get_sync_engine(self) -> Engine:
        """同期エンジンを取得（Alembic 等の外部ツール用）.

        Returns:
            同期 Engine

        Raises:
            RuntimeError: 同期エンジンが未初期化の場合
        """
        if self._sync_engine is not None:
            return self._sync_engine
        msg = "同期エンジンが未初期化"
        raise RuntimeError(msg)

    def get_async_engine(self) -> AsyncEngine:
        """非同期エンジンを取得.

        Returns:
            非同期 AsyncEngine

        Raises:
            RuntimeError: 非同期エンジンが未初期化の場合
        """
        if self._async_engine is not None:
            return self._async_engine
        msg = "非同期エンジンが未初期化"
        raise RuntimeError(msg)
