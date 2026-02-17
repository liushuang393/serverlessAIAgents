"""AgentFlow 統一データベース管理モジュール.

目的:
    SQLAlchemy ベースの DB セッション管理と Alembic マイグレーションの
    共通機能を提供し、各アプリの重複コードを排除する。

提供機能:
    - DatabaseConfig: Pydantic ベースの DB 設定モデル
    - DatabaseManager: エンジン/セッション/テーブル作成の統一管理
    - MigrationEnv: Alembic env.py の共通ロジック（auto-stamp 対応）
    - URL ユーティリティ: sync/async ドライバ変換

使用例 (セッション管理):
    >>> from agentflow.database import DatabaseConfig, DatabaseManager
    >>> from my_app.models import Base
    >>>
    >>> db = DatabaseManager(
    ...     config=DatabaseConfig(
    ...         url="sqlite+aiosqlite:///./app.db",
    ...         url_env_key="MY_APP_DATABASE_URL",
    ...     ),
    ...     metadata=Base.metadata,
    ... )
    >>> await db.init()
    >>> async with db.session() as session:
    ...     result = await session.execute(query)
    >>> await db.close()

使用例 (マイグレーション env.py):
    >>> from agentflow.database import MigrationEnv
    >>> from my_app.models import Base
    >>>
    >>> MigrationEnv(
    ...     target_metadata=Base.metadata,
    ...     db_url_env="MY_APP_DATABASE_URL",
    ...     initial_revision="20260101_0001",
    ...     initial_tables=frozenset({"users", "sessions"}),
    ... ).run()
"""

from agentflow.database.config import DatabaseConfig
from agentflow.database.migration import MigrationEnv
from agentflow.database.session import DatabaseManager
from agentflow.database.url_utils import (
    get_dialect,
    is_async_url,
    is_sqlite,
    to_async_url,
    to_sync_url,
)


__all__ = [
    # 設定
    "DatabaseConfig",
    # セッション管理
    "DatabaseManager",
    # マイグレーション
    "MigrationEnv",
    # URL ユーティリティ
    "get_dialect",
    "is_async_url",
    "is_sqlite",
    "to_async_url",
    "to_sync_url",
]
