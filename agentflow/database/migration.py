"""統一 Alembic マイグレーション環境.

目的:
    各アプリの migrations/env.py で重複していたロジック（URL 解決、
    offline/online モード分岐、既存 DB の auto-stamp）を一元管理する。
    アプリ側の env.py は MigrationEnv を呼び出すだけの薄いラッパーとなる。

使用例（アプリの env.py）:
    >>> from my_app.models import Base
    >>> from agentflow.database.migration import MigrationEnv
    >>>
    >>> MigrationEnv(
    ...     target_metadata=Base.metadata,
    ...     db_url_env="FAQ_DATABASE_URL",
    ...     initial_revision="20260213_0001",
    ...     initial_tables=frozenset({"user_accounts", "auth_sessions"}),
    ... ).run()
"""

from __future__ import annotations

import asyncio
import logging
import os

from alembic import context
from sqlalchemy import MetaData, create_engine, pool, text
from sqlalchemy import inspect as sa_inspect
from sqlalchemy.engine import Connection
from sqlalchemy.ext.asyncio import create_async_engine

from agentflow.database.url_utils import is_sqlite, to_async_url, to_sync_url


_logger = logging.getLogger("alembic.env")


class MigrationEnv:
    """Alembic マイグレーション環境の統一実装.

    機能:
    - 環境変数 / alembic.ini からの URL 解決
    - sync/async ドライバ自動切替
    - 既存 DB の auto-stamp（create_all() 済み DB 対応）
    - offline/online モード自動判定
    """

    def __init__(
        self,
        target_metadata: MetaData,
        *,
        db_url_env: str = "DATABASE_URL",
        db_url_default: str | None = None,
        initial_revision: str | None = None,
        initial_tables: frozenset[str] | None = None,
        compare_type: bool = True,
    ) -> None:
        """初期化.

        Args:
            target_metadata: SQLAlchemy Base.metadata
            db_url_env: DB URL 環境変数名
            db_url_default: デフォルト URL（環境変数未設定時に alembic.ini から取得）
            initial_revision: 初期マイグレーションリビジョン ID（auto-stamp 用）
            initial_tables: 初期マイグレーションで作成されるテーブル名の集合
            compare_type: autogenerate 時にカラム型を比較するか
        """
        self._target_metadata = target_metadata
        self._db_url_env = db_url_env
        self._db_url_default = db_url_default
        self._initial_revision = initial_revision
        self._initial_tables = initial_tables or frozenset()
        self._compare_type = compare_type

    def get_url(self) -> str:
        """DB URL を解決.

        優先順位: 環境変数 > db_url_default > alembic.ini
        """
        url = os.getenv(self._db_url_env)
        if url:
            return url

        if self._db_url_default:
            return self._db_url_default

        alembic_url = context.config.get_main_option("sqlalchemy.url")
        if alembic_url:
            return alembic_url

        msg = (
            f"DB URL が未設定: 環境変数 {self._db_url_env} 、"
            "db_url_default、alembic.ini のいずれかを設定してください"
        )
        raise RuntimeError(msg)

    def auto_stamp_existing_db(self, connection: Connection) -> None:
        """既存 DB に Alembic 履歴が無い場合、初期リビジョンを自動 stamp する.

        検知ケース:
        1. alembic_version テーブルが存在しない + アプリテーブルあり
        2. alembic_version テーブルが空 + アプリテーブルあり

        Args:
            connection: 同期 DB コネクション
        """
        if not self._initial_revision or not self._initial_tables:
            return

        inspector = sa_inspect(connection)
        existing = set(inspector.get_table_names())
        has_app_tables = bool(existing & self._initial_tables)
        if not has_app_tables:
            # アプリテーブルなし → 新規 DB。通常マイグレーションに任せる
            return

        has_alembic = "alembic_version" in existing
        if has_alembic:
            row = connection.execute(
                text("SELECT version_num FROM alembic_version LIMIT 1"),
            ).fetchone()
            if row is not None:
                # 既にリビジョン記録済み → 通常フローに任せる
                return
            _logger.info(
                "alembic_version が空。初期リビジョン %s を自動 stamp",
                self._initial_revision,
            )
            connection.execute(
                text("INSERT INTO alembic_version (version_num) VALUES (:rev)"),
                {"rev": self._initial_revision},
            )
        else:
            _logger.info(
                "既存テーブル検出（alembic_version なし）。初期リビジョン %s を自動 stamp",
                self._initial_revision,
            )
            connection.execute(
                text(
                    "CREATE TABLE alembic_version (version_num VARCHAR(32) NOT NULL)",
                ),
            )
            connection.execute(
                text("INSERT INTO alembic_version (version_num) VALUES (:rev)"),
                {"rev": self._initial_revision},
            )
        connection.commit()

    def _do_run_migrations(self, connection: Connection) -> None:
        """コネクション上でマイグレーション実行（共通内部処理）."""
        self.auto_stamp_existing_db(connection)
        context.configure(
            connection=connection,
            target_metadata=self._target_metadata,
            compare_type=self._compare_type,
        )
        with context.begin_transaction():
            context.run_migrations()

    def _run_offline(self) -> None:
        """オフラインモード（SQL 出力のみ）."""
        url = self.get_url()
        context.configure(
            url=to_sync_url(url),
            target_metadata=self._target_metadata,
            literal_binds=True,
            dialect_opts={"paramstyle": "named"},
            compare_type=self._compare_type,
        )
        with context.begin_transaction():
            context.run_migrations()

    def _run_online(self) -> None:
        """オンラインモード（sync/async 自動判定）."""
        url = self.get_url()

        # SQLite は同期モードで実行（aiosqlite は Alembic 非対応のため）
        if is_sqlite(url):
            sync_url = to_sync_url(url)
            connectable = create_engine(sync_url, poolclass=pool.NullPool)
            with connectable.connect() as connection:
                self._do_run_migrations(connection)
            connectable.dispose()
            return

        # PostgreSQL/MySQL は非同期エンジンから同期実行
        async_url = to_async_url(url)
        asyncio.run(self._run_async_migrations(async_url))

    async def _run_async_migrations(self, url: str) -> None:
        """非同期エンジンでマイグレーション実行."""
        connectable = create_async_engine(url, poolclass=pool.NullPool)
        async with connectable.connect() as connection:
            await connection.run_sync(self._do_run_migrations)
        await connectable.dispose()

    def run(self) -> None:
        """エントリーポイント: offline/online 自動判定して実行."""
        if context.is_offline_mode():
            self._run_offline()
        else:
            self._run_online()
