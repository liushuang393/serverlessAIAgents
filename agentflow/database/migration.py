"""後方互換 re-export スタブ.

実体は infrastructure.database.migration に移動済み。
"""

from infrastructure.database.migration import MigrationEnv  # noqa: F401

__all__ = ["MigrationEnv"]
