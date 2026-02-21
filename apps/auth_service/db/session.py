"""auth_service DB セッション管理.

agentflow.database.DatabaseManager を利用した統一セッション管理。
"""

from __future__ import annotations

import asyncio
import logging
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING, Any

from apps.auth_service.models.user import Base

from agentflow.database import DatabaseConfig, DatabaseManager


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator

    from sqlalchemy.ext.asyncio import AsyncSession
    from sqlalchemy.orm import Session


logger = logging.getLogger(__name__)

_AUTH_DEFAULT_URL = "sqlite+aiosqlite:///./auth_service.db"

_db = DatabaseManager(
    config=DatabaseConfig(
        url=_AUTH_DEFAULT_URL,
        url_env_key="AUTH_DATABASE_URL",
        echo_env_key="AUTH_DB_ECHO",
    ),
    metadata=Base.metadata,
    force_sync=False,
)

_is_ready = False
_ready_lock: asyncio.Lock | None = None
_ready_lock_loop_id: int | None = None


def get_database_url() -> str:
    """DB URL を取得."""
    return _db.resolved_url


async def init_db() -> None:
    """DB エンジンを初期化."""
    await _db.init()


async def create_all_tables() -> None:
    """テーブルを作成（開発/テスト用）."""
    await _db.create_all_tables()


async def ensure_database_ready() -> None:
    """DB 利用可能状態を保証（二重初期化防止付き）."""
    global _is_ready

    if _is_ready:
        return

    async with _get_ready_lock():
        if _is_ready:
            return
        await init_db()
        from apps.auth_service.config import get_settings

        settings = get_settings()
        if settings.AUTH_DB_AUTO_CREATE:
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


class _SyncSessionAdapter:
    """同期 SQLAlchemy Session を async インターフェースで包む（SQLite 互換層）."""

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

    async def commit(self) -> None:
        self._session.commit()

    async def flush(self, objects: Any = None) -> None:
        self._session.flush(objects)

    async def rollback(self) -> None:
        self._session.rollback()

    async def delete(self, instance: Any) -> None:
        self._session.delete(instance)


@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession | _SyncSessionAdapter]:
    """DB セッションを提供."""
    await ensure_database_ready()

    if _db.is_sync_mode:
        sync_session = _db.session_sync()
        adapter = _SyncSessionAdapter(sync_session)
        try:
            yield adapter  # type: ignore[misc]
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
