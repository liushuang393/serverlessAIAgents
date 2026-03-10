"""initial llm management state

Revision ID: 20260310_0001
Revises:
Create Date: 2026-03-10 00:00:00
"""

from __future__ import annotations

import sqlalchemy as sa
from alembic import op


revision = "20260310_0001"
down_revision = None
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.create_table(
        "llm_provider_secrets",
        sa.Column("provider_name", sa.String(length=64), nullable=False),
        sa.Column("api_key_env", sa.String(length=128), nullable=True),
        sa.Column("encrypted_secret", sa.Text(), nullable=False),
        sa.Column("secret_mask", sa.String(length=64), nullable=False),
        sa.Column("source_label", sa.String(length=32), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("provider_name"),
    )
    op.create_table(
        "llm_engine_deployments",
        sa.Column("engine_name", sa.String(length=64), nullable=False),
        sa.Column("engine_type", sa.String(length=32), nullable=False),
        sa.Column("deployment_mode", sa.String(length=32), nullable=False),
        sa.Column("docker_image", sa.String(length=255), nullable=True),
        sa.Column("served_model_name", sa.String(length=255), nullable=True),
        sa.Column("container_name", sa.String(length=255), nullable=True),
        sa.Column("host_port", sa.Integer(), nullable=True),
        sa.Column("public_base_url", sa.String(length=512), nullable=True),
        sa.Column("compose_path", sa.String(length=512), nullable=True),
        sa.Column("compose_yaml", sa.Text(), nullable=True),
        sa.Column("gpu_enabled", sa.Boolean(), nullable=False),
        sa.Column("gpu_devices_json", sa.JSON(), nullable=False),
        sa.Column("gpu_count", sa.Integer(), nullable=True),
        sa.Column("extra_env_json", sa.JSON(), nullable=False),
        sa.Column("status", sa.String(length=32), nullable=False),
        sa.Column("last_error", sa.Text(), nullable=True),
        sa.Column("deployed_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("stopped_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("engine_name"),
    )


def downgrade() -> None:
    op.drop_table("llm_engine_deployments")
    op.drop_table("llm_provider_secrets")
