"""report_case_id と human_review_records を追加.

Revision ID: a1c9f4e2d7b0
Revises: 8bf2ccdc3fc1
Create Date: 2026-02-10
"""

from collections.abc import Sequence

from alembic import op


# revision identifiers, used by Alembic.
revision: str = "a1c9f4e2d7b0"
down_revision: str | Sequence[str] | None = "8bf2ccdc3fc1"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    """カラム追加."""
    op.execute(
        """
        ALTER TABLE decision_records
        ADD COLUMN IF NOT EXISTS report_case_id VARCHAR(64)
        """
    )
    op.execute(
        """
        ALTER TABLE decision_records
        ADD COLUMN IF NOT EXISTS human_review_records JSONB
        """
    )
    op.execute(
        """
        CREATE INDEX IF NOT EXISTS idx_decision_records_report_case_id
        ON decision_records(report_case_id)
        """
    )


def downgrade() -> None:
    """カラム削除."""
    op.execute(
        """
        DROP INDEX IF EXISTS idx_decision_records_report_case_id
        """
    )
    op.execute(
        """
        ALTER TABLE decision_records
        DROP COLUMN IF EXISTS human_review_records
        """
    )
    op.execute(
        """
        ALTER TABLE decision_records
        DROP COLUMN IF EXISTS report_case_id
        """
    )

