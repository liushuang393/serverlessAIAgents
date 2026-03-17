"""Alembic migration environment for Platform."""

from logging.config import fileConfig

from alembic import context
from apps.platform.db.models import Base

from infrastructure.database import MigrationEnv


if context.config.config_file_name is not None:
    fileConfig(context.config.config_file_name)

MigrationEnv(
    target_metadata=Base.metadata,
    db_url_env="PLATFORM_DATABASE_URL",
    initial_revision="20260310_0001",
    initial_tables=frozenset({"llm_provider_secrets", "llm_engine_deployments"}),
).run()
