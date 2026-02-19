"""stage_io_logs を追加.

Revision ID: c2f4a9d1e9b1
Revises: a1c9f4e2d7b0
Create Date: 2026-02-15
"""

from collections.abc import Sequence

from alembic import op


# revision identifiers, used by Alembic.
revision: str = "c2f4a9d1e9b1"
down_revision: str | Sequence[str] | None = "a1c9f4e2d7b0"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    """カラム追加."""
    op.execute(
        """
        ALTER TABLE decision_records
        ADD COLUMN IF NOT EXISTS stage_io_logs JSONB
        """
    )


def downgrade() -> None:
    """カラム削除."""
    op.execute(
        """
        ALTER TABLE decision_records
        DROP COLUMN IF EXISTS stage_io_logs
        """
    )
