# -*- coding: utf-8 -*-
"""{{ app_name }} データベース接続管理.

非同期 SQLAlchemy + asyncpg を使用。
"""
from contextlib import asynccontextmanager
from typing import AsyncGenerator

from sqlalchemy.ext.asyncio import (
    AsyncEngine,
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)
{% if redis_enabled %}
import redis.asyncio as redis
{% endif %}

from config import get_settings


# グローバル接続オブジェクト
_engine: AsyncEngine | None = None
_session_factory: async_sessionmaker[AsyncSession] | None = None
{% if redis_enabled %}
_redis_client: redis.Redis | None = None
{% endif %}


async def init_db() -> None:
    """DB 接続を初期化."""
    global _engine, _session_factory{% if redis_enabled %}, _redis_client{% endif %}
    
    settings = get_settings()
    
    # PostgreSQL 接続
    _engine = create_async_engine(
        settings.database_url,
        echo=settings.debug,
        pool_size=5,
        max_overflow=10,
        pool_pre_ping=True,
    )
    
    _session_factory = async_sessionmaker(
        bind=_engine,
        class_=AsyncSession,
        expire_on_commit=False,
        autocommit=False,
        autoflush=False,
    )
    
{% if redis_enabled %}
    # Redis 接続
    _redis_client = redis.from_url(
        settings.redis_url,
        encoding="utf-8",
        decode_responses=True,
    )
{% endif %}


async def close_db() -> None:
    """DB 接続をクローズ."""
    global _engine{% if redis_enabled %}, _redis_client{% endif %}
    
    if _engine:
        await _engine.dispose()
        _engine = None
        
{% if redis_enabled %}
    if _redis_client:
        await _redis_client.close()
        _redis_client = None
{% endif %}


@asynccontextmanager
async def get_db() -> AsyncGenerator[AsyncSession, None]:
    """DB セッションを取得（コンテキストマネージャー）.
    
    使用例:
        async with get_db() as db:
            result = await db.execute(select(Model))
    """
    if _session_factory is None:
        raise RuntimeError("Database not initialized. Call init_db() first.")
    
    async with _session_factory() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise


{% if redis_enabled %}
def get_redis() -> redis.Redis:
    """Redis クライアントを取得."""
    if _redis_client is None:
        raise RuntimeError("Redis not initialized. Call init_db() first.")
    return _redis_client
{% endif %}

