# -*- coding: utf-8 -*-
"""DB セッション管理."""

from __future__ import annotations

from sqlalchemy.ext.asyncio import AsyncEngine, AsyncSession, async_sessionmaker, create_async_engine

from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.db.base import Base


def _build_async_url(url: str) -> str:
    """非同期ドライバのURLに変換."""
    if url.startswith("sqlite+aiosqlite://"):
        return url
    if url.startswith("sqlite:///"):
        return url.replace("sqlite:///", "sqlite+aiosqlite:///", 1)
    if url.startswith("sqlite://"):
        return url.replace("sqlite://", "sqlite+aiosqlite://", 1)
    if url.startswith("postgresql://"):
        return url.replace("postgresql://", "postgresql+asyncpg://", 1)
    return url


DATABASE_URL = _build_async_url(config.database.url)

engine: AsyncEngine = create_async_engine(
    DATABASE_URL,
    echo=False,
    pool_pre_ping=True,
)

async_session: async_sessionmaker[AsyncSession] = async_sessionmaker(
    engine,
    expire_on_commit=False,
)


_db_initialized = False


async def init_db() -> None:
    """テーブルを初期化."""
    global _db_initialized
    if _db_initialized:
        return
    from apps.market_trend_monitor.backend.db import models  # noqa: F401

    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    _db_initialized = True
