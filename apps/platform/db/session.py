"""Platform DB セッション管理."""

from __future__ import annotations

import asyncio
import os
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING

from agentflow.database import DatabaseConfig, DatabaseManager
from apps.platform.db.models import Base


if TYPE_CHECKING:
    from collections.abc import AsyncGenerator

    from sqlalchemy.ext.asyncio import AsyncSession


_PLATFORM_DEFAULT_URL = "sqlite+aiosqlite:///./apps/platform/data/platform.db"


def _db_auto_create_enabled() -> bool:
    """起動時自動作成が有効か判定."""
    return os.getenv("PLATFORM_DB_AUTO_CREATE", "true").lower() in {
        "1",
        "true",
        "yes",
        "on",
    }


_db = DatabaseManager(
    config=DatabaseConfig(
        url=_PLATFORM_DEFAULT_URL,
        url_env_key="PLATFORM_DATABASE_URL",
        echo_env_key="PLATFORM_DB_ECHO",
    ),
    metadata=Base.metadata,
)

_is_ready = False
_ready_lock: asyncio.Lock | None = None
_ready_lock_loop_id: int | None = None


def get_platform_database_url() -> str:
    """Platform DB URL を返す."""
    return _db.resolved_url


def _get_ready_lock() -> asyncio.Lock:
    """現在ループに紐づく lock を返す."""
    global _ready_lock, _ready_lock_loop_id
    current_loop = asyncio.get_running_loop()
    current_loop_id = id(current_loop)
    if _ready_lock is None or _ready_lock_loop_id != current_loop_id:
        _ready_lock = asyncio.Lock()
        _ready_lock_loop_id = current_loop_id
    return _ready_lock


async def ensure_platform_db_ready() -> None:
    """Platform DB 初期化を保証する."""
    global _is_ready
    if _is_ready:
        return

    async with _get_ready_lock():
        if _is_ready:
            return
        await _db.init()
        if _db_auto_create_enabled():
            await _db.create_all_tables()
        _is_ready = True


@asynccontextmanager
async def get_platform_db_session() -> AsyncGenerator[AsyncSession]:
    """Platform DB セッションを返す."""
    await ensure_platform_db_ready()
    async with _db.session() as session:
        yield session


async def close_platform_db() -> None:
    """Platform DB をクローズする."""
    global _is_ready, _ready_lock, _ready_lock_loop_id
    await _db.close()
    _is_ready = False
    _ready_lock = None
    _ready_lock_loop_id = None
