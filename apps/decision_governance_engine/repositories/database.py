# -*- coding: utf-8 -*-
"""データベース接続管理.

目的:
    PostgreSQL（SQLAlchemy + asyncpg）と Redis の接続管理
    
環境変数:
    DATABASE_URL: PostgreSQL メイン接続 URL
    DATABASE_HISTORY_URL: PostgreSQL 履歴接続 URL
    REDIS_URL: Redis 接続 URL
"""

import logging
import os
from contextlib import asynccontextmanager
from typing import AsyncGenerator, Any

from sqlalchemy.ext.asyncio import (
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)

from apps.decision_governance_engine.repositories.models import Base

logger = logging.getLogger(__name__)

# デフォルト接続 URL（開発用、本番は環境変数で上書き）
DEFAULT_DATABASE_URL = "postgresql+asyncpg://dge:dge_password@localhost:5432/decision_governance"
DEFAULT_REDIS_URL = "redis://localhost:6379/0"

# グローバルエンジン/セッションファクトリ
_engine = None
_session_factory: async_sessionmaker[AsyncSession] | None = None
_redis_client: Any = None


def get_database_url() -> str:
    """データベース URL を取得.

    PostgreSQL + asyncpg ドライバーを使用。
    """
    url = os.getenv("DATABASE_URL", DEFAULT_DATABASE_URL)

    # postgresql:// → postgresql+asyncpg:// 自動変換
    if url.startswith("postgresql://") and "+asyncpg" not in url:
        url = url.replace("postgresql://", "postgresql+asyncpg://", 1)

    return url


def get_redis_url() -> str:
    """Redis URL を取得."""
    return os.getenv("REDIS_URL", DEFAULT_REDIS_URL)


async def init_db(create_tables: bool = False) -> None:
    """データベース初期化（PostgreSQL）.

    Args:
        create_tables: True の場合、テーブルを自動作成（開発用）
    """
    global _engine, _session_factory

    database_url = get_database_url()
    logger.info(f"Initializing database: {database_url[:50]}...")

    _engine = create_async_engine(
        database_url,
        echo=os.getenv("DB_ECHO", "false").lower() == "true",
        pool_size=5,
        max_overflow=10,
        pool_pre_ping=True,
    )

    _session_factory = async_sessionmaker(
        bind=_engine,
        class_=AsyncSession,
        expire_on_commit=False,
    )

    if create_tables:
        async with _engine.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)
            logger.info("Database tables created")


async def close_db() -> None:
    """データベース接続をクローズ."""
    global _engine, _session_factory, _redis_client
    
    if _engine:
        await _engine.dispose()
        _engine = None
        _session_factory = None
        logger.info("Database connection closed")
    
    if _redis_client:
        await _redis_client.close()
        _redis_client = None
        logger.info("Redis connection closed")


@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession, None]:
    """データベースセッションを取得（コンテキストマネージャ）.
    
    使用例:
        async with get_db_session() as session:
            result = await session.execute(query)
    """
    if _session_factory is None:
        await init_db()
    
    async with _session_factory() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise


async def get_redis() -> Any:
    """Redis クライアントを取得.
    
    Returns:
        redis.asyncio.Redis インスタンス
        
    Note:
        redis パッケージがインストールされていない場合は None を返す
    """
    global _redis_client
    
    if _redis_client is not None:
        return _redis_client
    
    try:
        import redis.asyncio as aioredis
        redis_url = get_redis_url()
        _redis_client = aioredis.from_url(
            redis_url,
            encoding="utf-8",
            decode_responses=True,
        )
        logger.info(f"Redis connected: {redis_url[:20]}...")
        return _redis_client
    except ImportError:
        logger.warning("redis package not installed, cache disabled")
        return None
    except Exception as e:
        logger.warning(f"Redis connection failed: {e}")
        return None

