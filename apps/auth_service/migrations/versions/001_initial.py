"""初期スキーマ作成マイグレーション.

Revision ID: 001
Revises: -
Create Date: 2026-02-21

テーブル:
    - user_accounts
    - auth_sessions
    - password_reset_tokens
    - refresh_tokens
    - token_blacklist
    - proxy_auth_nonces
"""

from __future__ import annotations


# Alembic revision identifier
revision = "001"
down_revision = None
branch_labels = None
depends_on = None


def upgrade() -> None:
    """テーブルを作成."""
    import sqlalchemy as sa
    from alembic import op  # type: ignore[import-not-found]

    op.create_table(
        "user_accounts",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column("username", sa.String(100), nullable=False, unique=True, index=True),
        sa.Column("email", sa.String(256), nullable=True, index=True),
        sa.Column("password_hash", sa.String(256), nullable=False),
        sa.Column("password_salt", sa.String(64), nullable=False),
        sa.Column("display_name", sa.String(100), nullable=False),
        sa.Column("department", sa.String(100), nullable=False, default=""),
        sa.Column("position", sa.String(100), nullable=False, default=""),
        sa.Column("role", sa.String(50), nullable=False, default="employee"),
        sa.Column("auth_source", sa.String(32), nullable=False, default="local_db"),
        sa.Column("is_active", sa.Boolean, nullable=False, default=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("last_login_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("mfa_secret", sa.String(64), nullable=True),
        sa.Column("mfa_enabled", sa.Boolean, nullable=False, default=False),
        sa.Column("login_attempts", sa.Integer, nullable=False, default=0),
        sa.Column("locked_until", sa.DateTime(timezone=True), nullable=True),
    )

    op.create_table(
        "auth_sessions",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column(
            "user_id", sa.String(64), sa.ForeignKey("user_accounts.id", ondelete="CASCADE"), nullable=False, index=True
        ),
        sa.Column("token_hash", sa.String(128), nullable=False, unique=True, index=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("expires_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("revoked_at", sa.DateTime(timezone=True), nullable=True),
    )

    op.create_table(
        "password_reset_tokens",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column(
            "user_id", sa.String(64), sa.ForeignKey("user_accounts.id", ondelete="CASCADE"), nullable=False, index=True
        ),
        sa.Column("token_hash", sa.String(128), nullable=False, unique=True, index=True),
        sa.Column("expires_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("used_at", sa.DateTime(timezone=True), nullable=True),
    )

    op.create_table(
        "refresh_tokens",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column(
            "user_id", sa.String(64), sa.ForeignKey("user_accounts.id", ondelete="CASCADE"), nullable=False, index=True
        ),
        sa.Column("token_hash", sa.String(128), nullable=False, unique=True, index=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("expires_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("revoked_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("family", sa.String(64), nullable=False, index=True),
    )

    op.create_table(
        "token_blacklist",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column("jti", sa.String(128), nullable=False, unique=True, index=True),
        sa.Column("expires_at", sa.DateTime(timezone=True), nullable=False, index=True),
        sa.Column("revoked_at", sa.DateTime(timezone=True), nullable=False),
    )

    op.create_table(
        "proxy_auth_nonces",
        sa.Column("id", sa.String(64), primary_key=True),
        sa.Column("nonce_hash", sa.String(128), nullable=False, unique=True, index=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("expires_at", sa.DateTime(timezone=True), nullable=False, index=True),
    )


def downgrade() -> None:
    """テーブルを削除."""
    from alembic import op  # type: ignore[import-not-found]

    op.drop_table("proxy_auth_nonces")
    op.drop_table("token_blacklist")
    op.drop_table("refresh_tokens")
    op.drop_table("password_reset_tokens")
    op.drop_table("auth_sessions")
    op.drop_table("user_accounts")
