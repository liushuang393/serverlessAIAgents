"""段階的保存用フィールド追加.

Revision ID: 8bf2ccdc3fc1
Revises: 0001
Create Date: 2026-02-02

目的:
    - analysis_status: 分析ステータス（PENDING/IN_PROGRESS/COMPLETED/FAILED/PARTIAL）
    - last_completed_stage: 最後に完了したステージ名
    - clarification_result: 明確化ステージの結果（JSONB）
"""

from collections.abc import Sequence

import sqlalchemy as sa
from alembic import op
from sqlalchemy.dialects import postgresql


# revision identifiers, used by Alembic.
revision: str = "8bf2ccdc3fc1"
down_revision: str | Sequence[str] | None = "0001"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    """段階的保存用フィールドを追加."""
    # analysis_status: 分析ステータス
    op.add_column(
        "decision_records",
        sa.Column("analysis_status", sa.String(length=20), nullable=True, server_default="PENDING"),
    )
    # last_completed_stage: 最後に完了したステージ
    op.add_column("decision_records", sa.Column("last_completed_stage", sa.String(length=30), nullable=True))
    # clarification_result: 明確化ステージの結果
    op.add_column(
        "decision_records",
        sa.Column("clarification_result", postgresql.JSONB(astext_type=sa.Text()), nullable=True),
    )

    # analysis_status にインデックスを追加（部分完了レコードの検索用）
    op.create_index(
        "idx_decision_records_analysis_status",
        "decision_records",
        ["analysis_status"],
        unique=False,
    )


def downgrade() -> None:
    """段階的保存用フィールドを削除."""
    op.drop_index("idx_decision_records_analysis_status", table_name="decision_records")
    op.drop_column("decision_records", "clarification_result")
    op.drop_column("decision_records", "last_completed_stage")
    op.drop_column("decision_records", "analysis_status")
