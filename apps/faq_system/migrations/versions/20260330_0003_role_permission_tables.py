"""KB 権限テーブル追加（auth_service 連携版）.

Revision ID: 20260330_0003
Revises: 20260301_0002
Create Date: 2026-03-30

auth_service のロール名をキーとして KB 権限マッピングを管理。
ロール定義自体は auth_service 側で管理される。
デフォルト KB 権限をシードする。
"""

from __future__ import annotations

from datetime import UTC, datetime

import sqlalchemy as sa
from alembic import op


# revision identifiers, used by Alembic.
revision = "20260330_0003"
down_revision = "20260301_0002"
branch_labels = None
depends_on = None

# デフォルト KB 権限マッピング（auth_service のロール名をキーに使用）
_DEFAULT_KB_PERMS: dict[str, list[tuple[str, str]]] = {
    "guest": [("external", "read")],
    "employee": [("internal", "read"), ("external", "read")],
    "manager": [
        ("internal", "read"), ("internal", "write"),
        ("external", "read"),
        ("confidential", "read"),
    ],
    "analyst": [
        ("internal", "read"),
        ("external", "read"),
    ],
    "hr_admin": [
        ("internal", "read"), ("internal", "write"),
        ("confidential", "read"), ("confidential", "write"),
    ],
    "admin": [
        ("internal", "read"), ("internal", "write"),
        ("external", "read"), ("external", "write"),
        ("confidential", "read"), ("confidential", "write"),
    ],
}


def upgrade() -> None:
    """Upgrade schema."""
    # 旧テーブルが存在する場合はドロップ（v1 → v2 移行対応）
    conn = op.get_bind()
    inspector = sa.inspect(conn)
    existing_tables = inspector.get_table_names()

    if "role_kb_permissions" in existing_tables:
        op.drop_table("role_kb_permissions")
    if "role_definitions" in existing_tables:
        op.drop_table("role_definitions")

    # KB 権限テーブル（role_name 文字列ベース）
    op.create_table(
        "role_kb_permissions",
        sa.Column("id", sa.Integer, primary_key=True, autoincrement=True),
        sa.Column("role_name", sa.String(100), nullable=False),
        sa.Column("kb_type", sa.String(32), nullable=False),
        sa.Column("action", sa.String(16), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
    )
    op.create_index("ix_role_kb_permissions_role_name", "role_kb_permissions", ["role_name"])
    op.create_index(
        "ix_role_kb_permissions_unique",
        "role_kb_permissions",
        ["role_name", "kb_type", "action"],
        unique=True,
    )

    # デフォルト KB 権限をシード
    now = datetime.now(tz=UTC).isoformat()
    conn = op.get_bind()
    for role_name, perms in _DEFAULT_KB_PERMS.items():
        for kb_type, action in perms:
            conn.execute(
                sa.text(
                    "INSERT INTO role_kb_permissions (role_name, kb_type, action, created_at) "
                    "VALUES (:role_name, :kb_type, :action, :created_at)"
                ),
                {"role_name": role_name, "kb_type": kb_type, "action": action, "created_at": now},
            )


def downgrade() -> None:
    """Downgrade schema."""
    op.drop_index("ix_role_kb_permissions_unique", table_name="role_kb_permissions")
    op.drop_index("ix_role_kb_permissions_role_name", table_name="role_kb_permissions")
    op.drop_table("role_kb_permissions")

