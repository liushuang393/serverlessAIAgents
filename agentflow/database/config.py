"""後方互換 re-export スタブ.

実体は infrastructure.database.config に移動済み。
"""

from infrastructure.database.config import DatabaseConfig  # noqa: F401

__all__ = ["DatabaseConfig"]
