"""FAQ システム DB セッション管理.

agentflow.database.DatabaseManager を利用した統一セッション管理。
エンジン/セッションファクトリのライフサイクルはフレームワークに委譲し、
アプリ固有ロジック（ensure_ready, SyncSessionAdapter）のみ保持する。
"""

from __future__ import annotations

import asyncio
import os
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from typing import Any

from apps.faq_system.backend.db.models import Base
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import Session

from agentflow.database import DatabaseConfig, DatabaseManager


# ---------------------------------------------------------------------------
# 環境変数ヘルパー
# ---------------------------------------------------------------------------

_FAQ_DEFAULT_URL = "sqlite+aiosqlite:///./faq_system.db"


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
    return _db.resolved_url


async def init_db() -> None:
    """DB エンジンを初期化."""
    await _db.init()


async def create_all_tables() -> None:
    """モデル定義からテーブルを作成（ローカル/テスト用）."""
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
