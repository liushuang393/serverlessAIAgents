"""後方互換 re-export スタブ.

実体は infrastructure.database に移動済み。
新規コードは ``from infrastructure.database import ...`` を使用すること。
"""

from infrastructure.database import (  # noqa: F401
    DatabaseConfig,
    DatabaseManager,
    MigrationEnv,
    get_dialect,
    is_async_url,
    is_sqlite,
    to_async_url,
    to_sync_url,
)

__all__ = [
    "DatabaseConfig",
    "DatabaseManager",
    "MigrationEnv",
    "get_dialect",
    "is_async_url",
    "is_sqlite",
    "to_async_url",
    "to_sync_url",
]
