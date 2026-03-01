"""Add ingestion run/checkpoint tracking tables.

Revision ID: 20260301_0002
Revises: 20260213_0001
Create Date: 2026-03-01
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import sqlalchemy as sa
from alembic import op


if TYPE_CHECKING:
    from collections.abc import Sequence


revision: str = "20260301_0002"
down_revision: str | None = "20260213_0001"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    """Upgrade schema."""
    op.create_table(
        "ingestion_runs",
        sa.Column("id", sa.String(length=64), nullable=False),
        sa.Column("app_name", sa.String(length=100), nullable=False),
        sa.Column("status", sa.String(length=32), nullable=False),
        sa.Column("trigger_mode", sa.String(length=32), nullable=False, server_default="sync"),
        sa.Column("dry_run", sa.Boolean(), nullable=False, server_default=sa.false()),
        sa.Column("source_ids_json", sa.JSON(), nullable=False),
        sa.Column("summary_json", sa.JSON(), nullable=False),
        sa.Column("error_message", sa.Text(), nullable=True),
        sa.Column("started_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("finished_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("id"),
    )
    op.create_index("ix_ingestion_runs_app_name", "ingestion_runs", ["app_name"], unique=False)
    op.create_index("ix_ingestion_runs_status", "ingestion_runs", ["status"], unique=False)
    op.create_index(
        "ix_ingestion_runs_app_started",
        "ingestion_runs",
        ["app_name", "started_at"],
        unique=False,
    )

    op.create_table(
        "ingestion_run_items",
        sa.Column("id", sa.Integer(), autoincrement=True, nullable=False),
        sa.Column("run_id", sa.String(length=64), nullable=False),
        sa.Column("source_id", sa.String(length=128), nullable=False),
        sa.Column("source_type", sa.String(length=32), nullable=False),
        sa.Column("status", sa.String(length=32), nullable=False),
        sa.Column("message", sa.Text(), nullable=True),
        sa.Column("stats_json", sa.JSON(), nullable=False),
        sa.Column("payload_json", sa.JSON(), nullable=False),
        sa.Column("started_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("finished_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.ForeignKeyConstraint(["run_id"], ["ingestion_runs.id"], ondelete="CASCADE"),
        sa.PrimaryKeyConstraint("id"),
    )
    op.create_index("ix_ingestion_run_items_run_id", "ingestion_run_items", ["run_id"], unique=False)
    op.create_index(
        "ix_ingestion_run_items_source_id",
        "ingestion_run_items",
        ["source_id"],
        unique=False,
    )
    op.create_index("ix_ingestion_run_items_status", "ingestion_run_items", ["status"], unique=False)
    op.create_index(
        "ix_ingestion_run_items_run_source",
        "ingestion_run_items",
        ["run_id", "source_id"],
        unique=False,
    )

    op.create_table(
        "ingestion_checkpoints",
        sa.Column("id", sa.Integer(), autoincrement=True, nullable=False),
        sa.Column("source_id", sa.String(length=128), nullable=False),
        sa.Column("cursor_text", sa.String(length=512), nullable=True),
        sa.Column("cursor_time", sa.DateTime(timezone=True), nullable=True),
        sa.Column("metadata_json", sa.JSON(), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("source_id"),
    )
    op.create_index(
        "ix_ingestion_checkpoints_source_id",
        "ingestion_checkpoints",
        ["source_id"],
        unique=True,
    )


def downgrade() -> None:
    """Downgrade schema."""
    op.drop_index("ix_ingestion_checkpoints_source_id", table_name="ingestion_checkpoints")
    op.drop_table("ingestion_checkpoints")

    op.drop_index("ix_ingestion_run_items_run_source", table_name="ingestion_run_items")
    op.drop_index("ix_ingestion_run_items_status", table_name="ingestion_run_items")
    op.drop_index("ix_ingestion_run_items_source_id", table_name="ingestion_run_items")
    op.drop_index("ix_ingestion_run_items_run_id", table_name="ingestion_run_items")
    op.drop_table("ingestion_run_items")

    op.drop_index("ix_ingestion_runs_app_started", table_name="ingestion_runs")
    op.drop_index("ix_ingestion_runs_status", table_name="ingestion_runs")
    op.drop_index("ix_ingestion_runs_app_name", table_name="ingestion_runs")
    op.drop_table("ingestion_runs")
