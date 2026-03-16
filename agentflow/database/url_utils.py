"""後方互換 re-export スタブ.

実体は infrastructure.database.url_utils に移動済み。
"""

from infrastructure.database.url_utils import (  # noqa: F401
    get_dialect,
    is_async_url,
    is_sqlite,
    to_async_url,
    to_sync_url,
)

__all__ = [
    "get_dialect",
    "is_async_url",
    "is_sqlite",
    "to_async_url",
    "to_sync_url",
]
