"""agentflow.database モジュールの単体テスト.

対象: url_utils, DatabaseConfig, DatabaseManager
SQLite を使用しインメモリ DB でテスト（外部依存なし）。
"""

from __future__ import annotations

import pytest
from sqlalchemy import Column, Integer, MetaData, String, Table, text
from sqlalchemy.ext.asyncio import AsyncSession

from agentflow.database import (
    DatabaseConfig,
    DatabaseManager,
    get_dialect,
    is_async_url,
    is_sqlite,
    to_async_url,
    to_sync_url,
)


# ============================================================================
# url_utils テスト
# ============================================================================


class TestGetDialect:
    """get_dialect() の検証."""

    def test_sqlite_plain(self) -> None:
        """SQLite URL のダイアレクト取得."""
        assert get_dialect("sqlite:///./app.db") == "sqlite"

    def test_sqlite_aiosqlite(self) -> None:
        """SQLite (aiosqlite) URL のダイアレクト取得."""
        assert get_dialect("sqlite+aiosqlite:///./app.db") == "sqlite"

    def test_postgresql_plain(self) -> None:
        """PostgreSQL URL のダイアレクト取得."""
        assert get_dialect("postgresql://user:pass@host/db") == "postgresql"

    def test_postgresql_asyncpg(self) -> None:
        """PostgreSQL (asyncpg) URL のダイアレクト取得."""
        assert get_dialect("postgresql+asyncpg://user:pass@host/db") == "postgresql"

    def test_mysql(self) -> None:
        """MySQL URL のダイアレクト取得."""
        assert get_dialect("mysql://user:pass@host/db") == "mysql"


class TestToAsyncUrl:
    """to_async_url() の検証."""

    def test_sqlite_sync_to_async(self) -> None:
        """sqlite → sqlite+aiosqlite."""
        assert to_async_url("sqlite:///./app.db") == "sqlite+aiosqlite:///./app.db"

    def test_postgresql_sync_to_async(self) -> None:
        """postgresql → postgresql+asyncpg."""
        url = "postgresql://user:pass@host/db"
        assert to_async_url(url) == "postgresql+asyncpg://user:pass@host/db"

    def test_already_async_unchanged(self) -> None:
        """既に非同期 URL の場合は変更なし."""
        url = "sqlite+aiosqlite:///./app.db"
        assert to_async_url(url) == url

    def test_psycopg2_to_asyncpg(self) -> None:
        """postgresql+psycopg2 → postgresql+asyncpg."""
        url = "postgresql+psycopg2://user:pass@host/db"
        assert to_async_url(url) == "postgresql+asyncpg://user:pass@host/db"


class TestToSyncUrl:
    """to_sync_url() の検証."""

    def test_aiosqlite_to_sqlite(self) -> None:
        """sqlite+aiosqlite → sqlite."""
        assert to_sync_url("sqlite+aiosqlite:///./app.db") == "sqlite:///./app.db"

    def test_asyncpg_to_psycopg2(self) -> None:
        """postgresql+asyncpg → postgresql+psycopg2."""
        url = "postgresql+asyncpg://user:pass@host/db"
        assert to_sync_url(url) == "postgresql+psycopg2://user:pass@host/db"

    def test_already_sync_unchanged(self) -> None:
        """既に同期 URL の場合は変更なし."""
        url = "sqlite:///./app.db"
        assert to_sync_url(url) == url


class TestIsSqlite:
    """is_sqlite() の検証."""

    def test_sqlite_true(self) -> None:
        assert is_sqlite("sqlite:///app.db") is True

    def test_aiosqlite_true(self) -> None:
        assert is_sqlite("sqlite+aiosqlite:///app.db") is True

    def test_postgresql_false(self) -> None:
        assert is_sqlite("postgresql://host/db") is False


class TestIsAsyncUrl:
    """is_async_url() の検証."""

    def test_aiosqlite_true(self) -> None:
        assert is_async_url("sqlite+aiosqlite:///app.db") is True

    def test_asyncpg_true(self) -> None:
        assert is_async_url("postgresql+asyncpg://host/db") is True

    def test_sync_sqlite_false(self) -> None:
        assert is_async_url("sqlite:///app.db") is False

    def test_sync_postgresql_false(self) -> None:
        assert is_async_url("postgresql://host/db") is False


# ============================================================================
# DatabaseConfig テスト
# ============================================================================


class TestDatabaseConfig:
    """DatabaseConfig の検証."""

    def test_default_values(self) -> None:
        """デフォルト設定の確認."""
        cfg = DatabaseConfig()
        assert "sqlite" in cfg.url
        assert cfg.pool_size == 5
        assert cfg.expire_on_commit is False

    def test_custom_url(self) -> None:
        """カスタム URL の設定."""
        cfg = DatabaseConfig(url="sqlite+aiosqlite:///custom.db")
        assert cfg.url == "sqlite+aiosqlite:///custom.db"

    def test_invalid_url_raises(self) -> None:
        """不正な URL はバリデーションエラー."""
        with pytest.raises(ValueError, match="無効な DB URL"):
            DatabaseConfig(url="invalid-no-scheme")


# ============================================================================
# DatabaseManager テスト（SQLite インメモリ）
# ============================================================================

# テスト用テーブル定義
_test_metadata = MetaData()
_test_table = Table(
    "test_items",
    _test_metadata,
    Column("id", Integer, primary_key=True),
    Column("name", String(100), nullable=False),
)


def _make_manager(
    *,
    url: str = "sqlite+aiosqlite://",
    force_sync: bool = False,
) -> DatabaseManager:
    """テスト用 DatabaseManager を生成."""
    return DatabaseManager(
        config=DatabaseConfig(url=url, url_env_key="__UNUSED__"),
        metadata=_test_metadata,
        force_sync=force_sync,
    )


class TestDatabaseManagerProperties:
    """DatabaseManager プロパティの検証."""

    def test_is_sync_mode_false_for_async_sqlite(self) -> None:
        """force_sync=False → is_sync_mode=False."""
        db = _make_manager(force_sync=False)
        assert db.is_sync_mode is False

    def test_is_sync_mode_true_for_forced_sync_sqlite(self) -> None:
        """force_sync=True + SQLite → is_sync_mode=True."""
        db = _make_manager(force_sync=True)
        assert db.is_sync_mode is True

    def test_is_sync_mode_false_for_postgresql(self) -> None:
        """PostgreSQL は force_sync=True でも is_sync_mode=False（SQLite のみ）."""
        db = _make_manager(url="postgresql+asyncpg://host/db", force_sync=True)
        assert db.is_sync_mode is False

    def test_resolved_url(self) -> None:
        """resolved_url は設定 URL を返す."""
        db = _make_manager(url="sqlite+aiosqlite:///custom.db")
        assert db.resolved_url == "sqlite+aiosqlite:///custom.db"

    def test_is_initialized_false_initially(self) -> None:
        """初期状態は未初期化."""
        db = _make_manager()
        assert db.is_initialized is False


class TestDatabaseManagerAsyncInit:
    """DatabaseManager 非同期初期化の検証."""

    @pytest.mark.asyncio
    async def test_init_creates_engine(self) -> None:
        """init() でエンジンが作成される."""
        db = _make_manager()
        await db.init()
        assert db.is_initialized is True
        await db.close()

    @pytest.mark.asyncio
    async def test_double_init_is_idempotent(self) -> None:
        """init() 2回呼んでもエラーなし."""
        db = _make_manager()
        await db.init()
        await db.init()
        assert db.is_initialized is True
        await db.close()

    @pytest.mark.asyncio
    async def test_close_resets_state(self) -> None:
        """close() で未初期化状態に戻る."""
        db = _make_manager()
        await db.init()
        await db.close()
        assert db.is_initialized is False


class TestDatabaseManagerSession:
    """DatabaseManager セッション管理の検証."""

    @pytest.mark.asyncio
    async def test_session_auto_initializes(self) -> None:
        """session() は自動初期化する."""
        db = _make_manager()
        async with db.session() as session:
            assert isinstance(session, AsyncSession)
        await db.close()

    @pytest.mark.asyncio
    async def test_create_all_tables_and_query(self) -> None:
        """create_all_tables() + セッション経由の INSERT/SELECT."""
        db = _make_manager()
        await db.init()
        await db.create_all_tables()

        # INSERT
        async with db.session() as session:
            await session.execute(
                text("INSERT INTO test_items (id, name) VALUES (1, 'alpha')"),
            )

        # SELECT
        async with db.session() as session:
            result = await session.execute(text("SELECT name FROM test_items"))
            row = result.fetchone()
            assert row is not None
            assert row[0] == "alpha"

        await db.close()


class TestDatabaseManagerSyncMode:
    """DatabaseManager 同期モード（force_sync + SQLite）の検証."""

    @pytest.mark.asyncio
    async def test_sync_init_and_session(self) -> None:
        """同期モードでもセッション取得可能."""
        db = _make_manager(url="sqlite+aiosqlite://", force_sync=True)
        await db.init()
        assert db.is_initialized is True

        session = db.session_sync()
        assert session is not None
        session.close()

        await db.close()

    @pytest.mark.asyncio
    async def test_sync_create_tables(self) -> None:
        """同期モードでテーブル作成."""
        db = _make_manager(url="sqlite+aiosqlite://", force_sync=True)
        await db.init()
        await db.create_all_tables()

        session = db.session_sync()
        result = session.execute(text("SELECT count(*) FROM test_items"))
        count = result.scalar()
        assert count == 0
        session.close()

        await db.close()

    @pytest.mark.asyncio
    async def test_async_session_raises_in_sync_mode(self) -> None:
        """同期モードで async session() は RuntimeError."""
        db = _make_manager(url="sqlite+aiosqlite://", force_sync=True)
        await db.init()
        with pytest.raises(RuntimeError, match="非同期セッションファクトリが未初期化"):
            async with db.session():
                pass
        await db.close()
