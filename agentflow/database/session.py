"""後方互換 re-export スタブ.

実体は infrastructure.database.session に移動済み。
"""

from infrastructure.database.session import DatabaseManager  # noqa: F401

__all__ = ["DatabaseManager"]
