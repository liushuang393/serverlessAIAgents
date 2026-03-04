"""RAG ナレッジ用軽量 DB セッション管理.

SQLite (aiosqlite) を使用してフレームワーク RAG テーブルを管理する。
CMA は本体 DB を持たないため、専用の SQLite ファイルを使用する。
"""

from __future__ import annotations

import logging
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from pathlib import Path

from sqlalchemy import StaticPool
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker, create_async_engine

from agentflow.knowledge.models import Base as RAGBase


logger = logging.getLogger("cma.knowledge_db")

_DB_DIR = Path(__file__).resolve().parents[1] / "data"
_DB_PATH = _DB_DIR / "knowledge.db"

_engine = create_async_engine(
    f"sqlite+aiosqlite:///{_DB_PATH}",
    connect_args={"check_same_thread": False},
    poolclass=StaticPool,
)
_session_factory = async_sessionmaker(_engine, expire_on_commit=False)


async def init_knowledge_db() -> None:
    """RAG テーブルを作成（起動時に呼ぶ）."""
    _DB_DIR.mkdir(parents=True, exist_ok=True)
    async with _engine.begin() as conn:
        await conn.run_sync(RAGBase.metadata.create_all)
    logger.info("Knowledge DB initialized at %s", _DB_PATH)


@asynccontextmanager
async def get_knowledge_session() -> AsyncGenerator[AsyncSession, None]:
    """RAG 管理用セッションを取得."""
    async with _session_factory() as session:
        yield session


async def close_knowledge_db() -> None:
    """DB エンジンをクローズ."""
    await _engine.dispose()
    logger.info("Knowledge DB closed")
