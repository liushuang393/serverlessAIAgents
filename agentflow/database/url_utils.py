"""DB URL 正規化ユーティリティ.

目的:
    SQLAlchemy の同期/非同期ドライバ間の URL 変換を一元管理する。
    各アプリが個別に変換ロジックを持つ必要をなくす。

対応ドライバ:
    - SQLite:   sqlite:// ↔ sqlite+aiosqlite://
    - PostgreSQL: postgresql:// ↔ postgresql+asyncpg:// ↔ postgresql+psycopg2://
    - MySQL:    mysql:// ↔ mysql+aiomysql:// (将来対応)
"""

from __future__ import annotations


# 同期 → 非同期ドライバ変換マップ
_ASYNC_DRIVER_MAP: dict[str, str] = {
    "sqlite": "sqlite+aiosqlite",
    "postgresql": "postgresql+asyncpg",
    "postgresql+psycopg2": "postgresql+asyncpg",
    "mysql": "mysql+aiomysql",
    "mysql+pymysql": "mysql+aiomysql",
}

# 非同期 → 同期ドライバ変換マップ（Alembic 用）
_SYNC_DRIVER_MAP: dict[str, str] = {
    "sqlite+aiosqlite": "sqlite",
    "postgresql+asyncpg": "postgresql+psycopg2",
    "mysql+aiomysql": "mysql+pymysql",
}


def get_dialect(url: str) -> str:
    """URL からダイアレクト名を取得.

    Args:
        url: SQLAlchemy 接続 URL

    Returns:
        ダイアレクト名 ("sqlite", "postgresql", "mysql" 等)

    Examples:
        >>> get_dialect("sqlite:///./app.db")
        'sqlite'
        >>> get_dialect("postgresql+asyncpg://localhost/db")
        'postgresql'
    """
    scheme = url.split("://", maxsplit=1)[0] if "://" in url else url
    # "postgresql+asyncpg" → "postgresql"
    return scheme.split("+")[0]


def to_async_url(url: str) -> str:
    """同期 URL を非同期ドライバ URL に変換.

    Args:
        url: SQLAlchemy 接続 URL

    Returns:
        非同期ドライバ対応 URL

    Examples:
        >>> to_async_url("sqlite:///./app.db")
        'sqlite+aiosqlite:///./app.db'
        >>> to_async_url("postgresql://user:pass@host/db")
        'postgresql+asyncpg://user:pass@host/db'
    """
    scheme = url.split("://", maxsplit=1)[0] if "://" in url else ""
    if scheme in _ASYNC_DRIVER_MAP:
        new_scheme = _ASYNC_DRIVER_MAP[scheme]
        return url.replace(f"{scheme}://", f"{new_scheme}://", 1)
    return url


def to_sync_url(url: str) -> str:
    """非同期 URL を同期ドライバ URL に変換（Alembic 用）.

    Args:
        url: SQLAlchemy 接続 URL

    Returns:
        同期ドライバ対応 URL

    Examples:
        >>> to_sync_url("sqlite+aiosqlite:///./app.db")
        'sqlite:///./app.db'
        >>> to_sync_url("postgresql+asyncpg://user:pass@host/db")
        'postgresql+psycopg2://user:pass@host/db'
    """
    scheme = url.split("://", maxsplit=1)[0] if "://" in url else ""
    if scheme in _SYNC_DRIVER_MAP:
        new_scheme = _SYNC_DRIVER_MAP[scheme]
        return url.replace(f"{scheme}://", f"{new_scheme}://", 1)
    return url


def is_sqlite(url: str) -> bool:
    """SQLite URL かどうか判定.

    Args:
        url: SQLAlchemy 接続 URL

    Returns:
        SQLite の場合 True
    """
    return get_dialect(url) == "sqlite"


def is_async_url(url: str) -> bool:
    """非同期ドライバ URL かどうか判定.

    Args:
        url: SQLAlchemy 接続 URL

    Returns:
        非同期ドライバの場合 True
    """
    scheme = url.split("://", maxsplit=1)[0] if "://" in url else ""
    return scheme in _SYNC_DRIVER_MAP
