"""FAQ システム DB セッション管理."""

from __future__ import annotations

import asyncio
import os
from collections.abc import Callable
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from typing import Any

from sqlalchemy import create_engine
from sqlalchemy.engine import Engine
from sqlalchemy.ext.asyncio import AsyncEngine, AsyncSession, async_sessionmaker, create_async_engine
from sqlalchemy.orm import Session, sessionmaker

from apps.faq_system.backend.db.models import Base

DEFAULT_DATABASE_URL = "sqlite+aiosqlite:///./faq_system.db"

_engine: AsyncEngine | None = None
_session_factory: async_sessionmaker[AsyncSession] | None = None
_sync_engine: Engine | None = None
_sync_session_factory: sessionmaker[Session] | None = None
_sync_mode = False
_is_ready = False
_ready_lock: asyncio.Lock | None = None
_ready_lock_loop_id: int | None = None


def get_database_url() -> str:
    """DB URL を取得."""
    url = os.getenv("FAQ_DATABASE_URL", DEFAULT_DATABASE_URL)
    if url.startswith("postgresql://") and "+asyncpg" not in url:
        url = url.replace("postgresql://", "postgresql+asyncpg://", 1)
    return url


def _is_sqlite(url: str) -> bool:
    return url.startswith("sqlite+")


def _sqlite_sync_mode_enabled() -> bool:
    return os.getenv("FAQ_SQLITE_SYNC_MODE", "true").lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


def _db_auto_create_enabled() -> bool:
    return os.getenv("FAQ_DB_AUTO_CREATE", "false").lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


async def init_db() -> None:
    """DB エンジンを初期化."""
    global _engine, _session_factory, _sync_engine, _sync_session_factory, _sync_mode

    if (_engine is not None and _session_factory is not None) or (
        _sync_engine is not None and _sync_session_factory is not None
    ):
        return

    url = get_database_url()
    engine_kwargs: dict[str, object] = {
        "echo": os.getenv("FAQ_DB_ECHO", "false").lower() in {"1", "true", "yes", "on"},
    }

    if _is_sqlite(url) and _sqlite_sync_mode_enabled():
        sync_url = url.replace("+aiosqlite", "")
        _sync_engine = create_engine(
            sync_url,
            connect_args={"check_same_thread": False},
            future=True,
            **engine_kwargs,
        )
        _sync_session_factory = sessionmaker(bind=_sync_engine, expire_on_commit=False)
        _sync_mode = True
        return

    if not _is_sqlite(url):
        engine_kwargs["pool_pre_ping"] = True
        engine_kwargs.update(
            {
                "pool_size": int(os.getenv("FAQ_DB_POOL_SIZE", "5")),
                "max_overflow": int(os.getenv("FAQ_DB_MAX_OVERFLOW", "10")),
            }
        )

    _engine = create_async_engine(url, **engine_kwargs)
    _session_factory = async_sessionmaker(bind=_engine, class_=AsyncSession, expire_on_commit=False)
    _sync_mode = False


async def create_all_tables() -> None:
    """モデル定義からテーブルを作成（ローカル/テスト用）."""
    if _sync_mode:
        if _sync_engine is None:
            await init_db()
        assert _sync_engine is not None
        Base.metadata.create_all(bind=_sync_engine)
        return

    if _engine is None:
        await init_db()
    assert _engine is not None
    async with _engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)


async def ensure_database_ready() -> None:
    """DB 利用可能状態を保証."""
    global _is_ready

    if _is_ready:
        return

    async with _get_ready_lock():
        if _is_ready:
            return
        await init_db()
        if _db_auto_create_enabled():
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


def get_session_factory() -> async_sessionmaker[AsyncSession]:
    """セッションファクトリを取得."""
    if _session_factory is None:
        msg = "Database is not initialized. Call ensure_database_ready() first."
        raise RuntimeError(msg)
    return _session_factory


class SyncSessionAdapter:
    """同期 SQLAlchemy Session を async インターフェースで包む."""

    def __init__(self, session: Session) -> None:
        self._session = session

    def add(self, instance: Any) -> None:
        self._session.add(instance)

    async def execute(self, statement: Any, *args: Any, **kwargs: Any) -> Any:
        return self._session.execute(statement, *args, **kwargs)

    async def scalar(self, statement: Any, *args: Any, **kwargs: Any) -> Any:
        return self._session.scalar(statement, *args, **kwargs)

    async def get(self, entity: Any, ident: Any, *args: Any, **kwargs: Any) -> Any:
        return self._session.get(entity, ident, *args, **kwargs)


@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession | SyncSessionAdapter]:
    """DB セッションを提供."""
    await ensure_database_ready()

    if _sync_mode:
        if _sync_session_factory is None:
            msg = "Database is not initialized. Call ensure_database_ready() first."
            raise RuntimeError(msg)
        sync_session = _sync_session_factory()
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

    session_factory = get_session_factory()
    async with session_factory() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise


async def close_db() -> None:
    """DB 接続をクローズ."""
    global _engine, _session_factory, _sync_engine, _sync_session_factory
    global _is_ready, _ready_lock, _ready_lock_loop_id, _sync_mode

    if _engine is not None:
        await _engine.dispose()
    if _sync_engine is not None:
        _sync_engine.dispose()
    _engine = None
    _session_factory = None
    _sync_engine = None
    _sync_session_factory = None
    _sync_mode = False
    _is_ready = False
    _ready_lock = None
    _ready_lock_loop_id = None
