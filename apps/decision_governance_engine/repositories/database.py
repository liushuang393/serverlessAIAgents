"""データベース接続管理.

目的:
    PostgreSQL（SQLAlchemy + asyncpg）と Redis の接続管理。
    agentflow.database.DatabaseManager を使用してエンジン/セッションを統一管理。

環境変数:
    DATABASE_URL: PostgreSQL 接続 URL
    REDIS_URL: Redis 接続 URL
"""

import logging
import os
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from typing import Any

from apps.decision_governance_engine.repositories.models import Base
from sqlalchemy.ext.asyncio import AsyncSession

from agentflow.database import DatabaseConfig, DatabaseManager


logger = logging.getLogger(__name__)

# デフォルト接続 URL（開発用、本番は環境変数で上書き）
_DEFAULT_DATABASE_URL = "postgresql+asyncpg://dge:dge_password@localhost:5432/decision_governance"
_DEFAULT_REDIS_URL = "redis://localhost:6379/0"

# フレームワーク統一 DatabaseManager インスタンス
_db = DatabaseManager(
    config=DatabaseConfig(
        url=_DEFAULT_DATABASE_URL,
        url_env_key="DATABASE_URL",
        echo_env_key="DB_ECHO",
    ),
    metadata=Base.metadata,
)

# Redis クライアント（DB フレームワーク外で管理）
_redis_client: Any = None


def get_database_url() -> str:
    """データベース URL を取得."""
    return _db.resolved_url


def get_redis_url() -> str:
    """Redis URL を取得."""
    return os.getenv("REDIS_URL", _DEFAULT_REDIS_URL)


async def init_db(create_tables: bool = False) -> None:
    """データベース初期化.

    Args:
        create_tables: True の場合、テーブルを自動作成（開発用）
    """
    await _db.init()
    if create_tables:
        await _db.create_all_tables()


async def close_db() -> None:
    """データベース/Redis 接続をクローズ."""
    global _redis_client

    await _db.close()

    if _redis_client is not None:
        await _redis_client.close()
        _redis_client = None
        logger.info("Redis connection closed")


@asynccontextmanager
async def get_db_session() -> AsyncGenerator[AsyncSession]:
    """データベースセッションを取得（コンテキストマネージャ）.

    使用例:
        async with get_db_session() as session:
            result = await session.execute(query)
    """
    async with _db.session() as session:
        yield session


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
        logger.info("Redis connected: %s...", redis_url[:20])
        return _redis_client
    except ImportError:
        logger.warning("redis package not installed, cache disabled")
        return None
    except Exception:
        logger.warning("Redis connection failed", exc_info=True)
        return None
