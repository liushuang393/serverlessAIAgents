"""DB provider cache isolation tests."""

from __future__ import annotations

import pytest

from infrastructure.storage.database.provider import get_db, reset_db


@pytest.fixture(autouse=True)
def _reset_db_cache() -> None:
    reset_db()
    yield
    reset_db()


def test_get_db_cache_isolated_by_database_url(monkeypatch: pytest.MonkeyPatch) -> None:
    """DB URL が変われば別インスタンスを返す."""
    monkeypatch.delenv("FAQ_SQL_SOURCE_DATABASE_URL", raising=False)
    monkeypatch.setenv("FAQ_DATABASE_URL", "sqlite+aiosqlite:///./first.db")
    first = get_db()

    monkeypatch.setenv("FAQ_DATABASE_URL", "sqlite+aiosqlite:///./second.db")
    second = get_db()

    assert first is not second


def test_get_db_cache_reuses_same_database_url(monkeypatch: pytest.MonkeyPatch) -> None:
    """同一 DB URL はキャッシュを再利用する."""
    monkeypatch.delenv("FAQ_SQL_SOURCE_DATABASE_URL", raising=False)
    monkeypatch.setenv("FAQ_DATABASE_URL", "sqlite+aiosqlite:///./shared.db")

    first = get_db()
    second = get_db()

    assert first is second
