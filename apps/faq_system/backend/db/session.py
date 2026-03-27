"""FAQ システム DB セッション管理.

infrastructure.database.DatabaseManager を利用した統一セッション管理。
エンジン/セッションファクトリのライフサイクルはフレームワークに委譲し、
アプリ固有ロジック（ensure_ready, SyncSessionAdapter）のみ保持する。
"""

from __future__ import annotations

import asyncio
import os
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.db.models import Base
from infrastructure.database import DatabaseConfig, DatabaseManager


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator

    from sqlalchemy.ext.asyncio import AsyncSession
    from sqlalchemy.orm import Session


# ---------------------------------------------------------------------------
# 環境変数ヘルパー
# ---------------------------------------------------------------------------

_FAQ_BASE_DIR = Path(__file__).resolve().parent.parent.parent  # apps/faq_system/
_FAQ_DATA_DIR = _FAQ_BASE_DIR / "data"
_FAQ_DATA_DIR.mkdir(parents=True, exist_ok=True)
_FAQ_DB_PATH = _FAQ_DATA_DIR / "faq_system.db"
_FAQ_DEFAULT_URL = f"sqlite+aiosqlite:///{_FAQ_DB_PATH}"


def _sqlite_sync_mode_enabled() -> bool:
    """SQLite 同期モードが有効か判定."""
    return os.getenv("FAQ_SQLITE_SYNC_MODE", "true").lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


def _db_auto_create_enabled() -> bool:
    """起動時テーブル自動作成が有効か判定."""
    return os.getenv("FAQ_DB_AUTO_CREATE", "false").lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


# ---------------------------------------------------------------------------
# フレームワーク DatabaseManager インスタンス
# ---------------------------------------------------------------------------

_db = DatabaseManager(
    config=DatabaseConfig(
        url=_FAQ_DEFAULT_URL,
        url_env_key="FAQ_DATABASE_URL",
        echo_env_key="FAQ_DB_ECHO",
    ),
    metadata=Base.metadata,
    force_sync=_sqlite_sync_mode_enabled(),
)

# 初期化ガード
_is_ready = False
_ready_lock: asyncio.Lock | None = None
_ready_lock_loop_id: int | None = None


# ---------------------------------------------------------------------------
# 公開 API（後方互換）
# ---------------------------------------------------------------------------


def get_database_url() -> str:
    """DB URL を取得."""
    return str(_db.resolved_url)


async def init_db() -> None:
    """DB エンジンを初期化."""
    await _db.init()


async def create_all_tables() -> None:
    """モデル定義からテーブルを作成（ローカル/テスト用）."""
    await _db.create_all_tables()


async def _is_fresh_database() -> bool:
    """DB が完全新規（テーブルが1つも存在しない）か判定.

    既存DB に FAQ_DB_AUTO_CREATE=true を適用すると Alembic バージョン履歴と
    乖離して upgrade 時に "table already exists" エラーになるため、
    テーブルが0件の場合のみ create_all() を許可する。
    """
    from sqlalchemy import inspect as sa_inspect

    url = str(_db.resolved_url)
    if "sqlite" in url:
        # 同期エンジンで確認（SQLite は同期モードで動作）
        from sqlalchemy import create_engine, pool

        from infrastructure.database.url_utils import to_sync_url

        engine = create_engine(to_sync_url(url), poolclass=pool.NullPool)
        try:
            with engine.connect() as sync_conn:
                inspector = sa_inspect(sync_conn)
                tables = inspector.get_table_names()
                return len(tables) == 0
        finally:
            engine.dispose()
    else:
        # 非同期エンジン（PostgreSQL 等）
        from sqlalchemy import pool as sa_pool
        from sqlalchemy.ext.asyncio import create_async_engine

        async_engine = create_async_engine(url, poolclass=sa_pool.NullPool)
        try:
            async with async_engine.connect() as async_conn:

                def _get_tables(sync_connection: Any) -> list[str]:
                    return list(sa_inspect(sync_connection).get_table_names())

                tables = await async_conn.run_sync(_get_tables)
                return len(tables) == 0
        finally:
            await async_engine.dispose()


async def ensure_database_ready() -> None:
    """DB 利用可能状態を保証（二重初期化防止付き）.

    注意: FAQ_DB_AUTO_CREATE=true は完全新規DB（テーブル0件）の場合のみ有効。
    既存DBへの適用は Alembic バージョン履歴と乖離するため禁止。
    スキーマ変更は必ず `alembic upgrade head` で行うこと。
    """
    global _is_ready

    if _is_ready:
        return

    async with _get_ready_lock():
        if not _is_ready:
            await init_db()
            if _db_auto_create_enabled() and await _is_fresh_database():
                await create_all_tables()
            _is_ready = True


def _get_ready_lock() -> asyncio.Lock:
    """現在のイベントループに紐づく lock を取得."""
    global _ready_lock, _ready_lock_loop_id
    current_loop = asyncio.get_running_loop()
    current_loop_id = id(current_loop)
    if _ready_lock is None or _ready_lock_loop_id != current_loop_id:
        _ready_lock = asyncio.Lock()
        _ready_lock_loop_id = current_loop_id
    return _ready_lock


# ---------------------------------------------------------------------------
# SyncSessionAdapter（SQLite 同期モード互換層）
# ---------------------------------------------------------------------------


class SyncSessionAdapter:
    """同期 SQLAlchemy Session を async インターフェースで包む."""

    def __init__(self, session: Session) -> None:
        self._session = session

    def add(self, instance: Any) -> None:
        """インスタンスを追加."""
        self._session.add(instance)

    async def execute(self, statement: Any, *args: Any, **kwargs: Any) -> Any:
        """クエリ実行."""
        return self._session.execute(statement, *args, **kwargs)

    async def scalar(self, statement: Any, *args: Any, **kwargs: Any) -> Any:
        """スカラー取得."""
        return self._session.scalar(statement, *args, **kwargs)

    async def get(self, entity: Any, ident: Any, *args: Any, **kwargs: Any) -> Any:
        """主キーでエンティティ取得."""
        return self._session.get(entity, ident, *args, **kwargs)

    async def commit(self) -> None:
        """トランザクションをコミット."""
        self._session.commit()

    async def flush(self, objects: Any = None) -> None:
        """セッションをフラッシュ（コミットなし）."""
        self._session.flush(objects)

    async def rollback(self) -> None:
        """トランザクションをロールバック."""
        self._session.rollback()

    async def delete(self, instance: Any) -> None:
        """インスタンスを削除対象としてマーク."""
        self._session.delete(instance)

    async def refresh(self, instance: Any, *args: Any, **kwargs: Any) -> None:
        """インスタンスを DB から再読み込み."""
        self._session.refresh(instance, *args, **kwargs)


# ---------------------------------------------------------------------------
# セッション提供
# ---------------------------------------------------------------------------


@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession | SyncSessionAdapter]:
    """DB セッションを提供（async/sync 自動判定）."""
    await ensure_database_ready()

    if _db.is_sync_mode:
        sync_session = _db.session_sync()
        adapter = SyncSessionAdapter(sync_session)
        try:
            yield adapter
            sync_session.commit()
        except Exception:
            sync_session.rollback()
            raise
        finally:
            sync_session.close()
        return

    async with _db.session() as session:
        yield session


async def close_db() -> None:
    """DB 接続をクローズ."""
    global _is_ready, _ready_lock, _ready_lock_loop_id
    await _db.close()
    _is_ready = False
    _ready_lock = None
    _ready_lock_loop_id = None


# ---------------------------------------------------------------------------
# RAG 管理テーブル初期化
# ---------------------------------------------------------------------------


async def init_rag_tables() -> None:
    """フレームワーク RAG 管理テーブルをアプリ DB に作成.

    既存テーブルがある場合は何もしない（checkfirst=True）。
    """
    from shared.rag.models import Base as RAGBase

    await _db.init()

    if _db.is_sync_mode:
        from sqlalchemy import create_engine
        from sqlalchemy import pool as sa_pool

        from infrastructure.database.url_utils import to_sync_url

        engine = create_engine(to_sync_url(_db.resolved_url), poolclass=sa_pool.NullPool)
        try:
            RAGBase.metadata.create_all(engine, checkfirst=True)
        finally:
            engine.dispose()
    else:
        async_engine = _db.get_async_engine()
        async with async_engine.begin() as conn:
            await conn.run_sync(RAGBase.metadata.create_all, checkfirst=True)


def get_rag_session_factory() -> Any:
    """CollectionManager / DocumentManager 用セッションファクトリを返す.

    get_db_session と互換性のある callable を返し、
    ``async with factory() as session:`` パターンで利用可能。
    """
    return get_db_session
