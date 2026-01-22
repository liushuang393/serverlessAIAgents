# -*- coding: utf-8 -*-
"""{{ app_name }} Alembic マイグレーション環境設定.

注意:
    - Alembic は同期ドライバ（psycopg2）のみ対応
    - asyncpg URL は自動的に psycopg2 に変換
"""
from logging.config import fileConfig

from alembic import context
from sqlalchemy import engine_from_config, pool

from repositories.models import Base

# Alembic Config オブジェクト
config = context.config

# ログ設定
if config.config_file_name is not None:
    fileConfig(config.config_file_name)

# SQLAlchemy メタデータ（autogenerate 用）
target_metadata = Base.metadata


def get_url() -> str:
    """DB URL を取得.
    
    asyncpg を psycopg2 に変換。
    """
    url = config.get_main_option("sqlalchemy.url", "")
    # asyncpg → psycopg2 変換
    return url.replace("+asyncpg", "+psycopg2")


def run_migrations_offline() -> None:
    """オフラインモードでマイグレーションを実行.
    
    SQL スクリプトを生成（DB 接続なし）。
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


def run_migrations_online() -> None:
    """オンラインモードでマイグレーションを実行.
    
    DB に直接接続してマイグレーション適用。
    """
    configuration = config.get_section(config.config_ini_section) or {}
    configuration["sqlalchemy.url"] = get_url()
    
    connectable = engine_from_config(
        configuration,
        prefix="sqlalchemy.",
        poolclass=pool.NullPool,
    )

    with connectable.connect() as connection:
        context.configure(
            connection=connection,
            target_metadata=target_metadata,
        )

        with context.begin_transaction():
            context.run_migrations()


# マイグレーション実行
if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()

