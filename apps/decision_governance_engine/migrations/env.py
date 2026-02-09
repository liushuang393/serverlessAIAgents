"""
Alembic マイグレーション環境設定.

目的:
    - SQLAlchemy モデルからの自動マイグレーション生成
    - 環境変数による DB URL 上書き
    - 同期/非同期 DB 対応

作成日: 2026-01-18
"""
import os
from logging.config import fileConfig

from alembic import context
from sqlalchemy import create_engine, pool


# ==============================================================================
# Alembic 設定
# ==============================================================================
config = context.config

# ロギング設定
if config.config_file_name is not None:
    fileConfig(config.config_file_name)

# ==============================================================================
# SQLAlchemy モデルのメタデータをインポート
# ==============================================================================
from apps.decision_governance_engine.repositories.models import Base


target_metadata = Base.metadata

# ==============================================================================
# 環境変数から DB URL を取得（上書き可能）
# ==============================================================================
def get_url() -> str:
    """
    DB URL を取得（環境変数優先）.

    asyncpg URL を psycopg2 URL に変換（Alembic は同期ドライバを使用）
    """
    url = os.getenv(
        "DATABASE_URL",
        config.get_main_option("sqlalchemy.url"),
    )
    # asyncpg -> psycopg2 変換（Alembic は同期ドライバを使用）
    if url and "+asyncpg" in url:
        url = url.replace("+asyncpg", "+psycopg2")
    return url


# ==============================================================================
# オフラインマイグレーション（SQL 出力のみ）
# ==============================================================================
def run_migrations_offline() -> None:
    """
    オフラインモードでマイグレーション実行.

    DB 接続なしで SQL スクリプトを生成。
    """
    url = get_url()
    context.configure(
        url=url,
        target_metadata=target_metadata,
        literal_binds=True,
        dialect_opts={"paramstyle": "named"},
    )

    with context.begin_transaction():
        context.run_migrations()


# ==============================================================================
# オンラインマイグレーション（同期モード）
# ==============================================================================
def run_migrations_online() -> None:
    """
    オンラインモードでマイグレーション実行.

    同期エンジンを使用（autogenerate 対応）
    """
    connectable = create_engine(
        get_url(),
        poolclass=pool.NullPool,
    )

    with connectable.connect() as connection:
        context.configure(
            connection=connection,
            target_metadata=target_metadata,
        )

        with context.begin_transaction():
            context.run_migrations()


# ==============================================================================
# エントリーポイント
# ==============================================================================
if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()
