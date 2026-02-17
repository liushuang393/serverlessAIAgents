"""Alembic migration environment for Decision Governance Engine.

agentflow.database.MigrationEnv を使用した統一マイグレーション環境。
既存DBの自動検知・auto-stamp・offline/online モードに自動対応。
"""

from logging.config import fileConfig

from alembic import context
from apps.decision_governance_engine.repositories.models import Base

from agentflow.database import MigrationEnv


# Alembic ロギング設定
if context.config.config_file_name is not None:
    fileConfig(context.config.config_file_name)

# フレームワーク統一マイグレーション実行
MigrationEnv(
    target_metadata=Base.metadata,
    db_url_env="DATABASE_URL",
    initial_revision="0001",
    initial_tables=frozenset(
        {
            "decision_records",
            "evidence_items",
            "claims",
            "claim_evidence_refs",
        }
    ),
).run()
