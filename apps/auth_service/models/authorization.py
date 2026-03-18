"""認可（Authorization）SQLAlchemy モデル.

RBAC（ロールベースアクセス制御）のためのテーブル定義。
roles, permissions, role_permissions, user_roles, resource_permissions,
resource_definitions の6テーブル。
"""

from __future__ import annotations

from datetime import datetime

from sqlalchemy import Boolean, DateTime, ForeignKey, Integer, String, Text, UniqueConstraint
from sqlalchemy.orm import Mapped, mapped_column, relationship

from apps.auth_service.models.user import Base, _utcnow


class Role(Base):
    """ロールモデル."""

    __tablename__ = "roles"

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    name: Mapped[str] = mapped_column(String(100), unique=True, nullable=False, index=True)
    display_name: Mapped[str] = mapped_column(String(200), nullable=False)
    description: Mapped[str] = mapped_column(Text, default="", nullable=False)
    is_system: Mapped[bool] = mapped_column(Boolean, default=False, nullable=False)
    priority: Mapped[int] = mapped_column(Integer, default=0, nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, onupdate=_utcnow, nullable=False
    )

    # リレーション
    role_permissions: Mapped[list[RolePermission]] = relationship(
        "RolePermission", back_populates="role", cascade="all, delete-orphan"
    )
    user_roles: Mapped[list[UserRole]] = relationship(
        "UserRole", back_populates="role", cascade="all, delete-orphan"
    )
    resource_permissions: Mapped[list[ResourcePermission]] = relationship(
        "ResourcePermission", back_populates="role", cascade="all, delete-orphan"
    )


class Permission(Base):
    """パーミッションモデル."""

    __tablename__ = "permissions"

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    name: Mapped[str] = mapped_column(String(200), unique=True, nullable=False, index=True)
    display_name: Mapped[str] = mapped_column(String(200), nullable=False)
    description: Mapped[str] = mapped_column(Text, default="", nullable=False)
    resource_type: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    action: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    is_system: Mapped[bool] = mapped_column(Boolean, default=False, nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, onupdate=_utcnow, nullable=False
    )

    # リレーション
    role_permissions: Mapped[list[RolePermission]] = relationship(
        "RolePermission", back_populates="permission", cascade="all, delete-orphan"
    )


class RolePermission(Base):
    """ロール-パーミッション中間テーブル."""

    __tablename__ = "role_permissions"
    __table_args__ = (
        UniqueConstraint("role_id", "permission_id", name="uq_role_permission"),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    role_id: Mapped[str] = mapped_column(
        String(64), ForeignKey("roles.id", ondelete="CASCADE"), nullable=False, index=True
    )
    permission_id: Mapped[str] = mapped_column(
        String(64), ForeignKey("permissions.id", ondelete="CASCADE"), nullable=False, index=True
    )

    # リレーション
    role: Mapped[Role] = relationship("Role", back_populates="role_permissions")
    permission: Mapped[Permission] = relationship("Permission", back_populates="role_permissions")


class UserRole(Base):
    """ユーザー-ロール中間テーブル."""

    __tablename__ = "user_roles"
    __table_args__ = (
        UniqueConstraint("user_id", "role_id", name="uq_user_role"),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    user_id: Mapped[str] = mapped_column(
        String(64), ForeignKey("user_accounts.id", ondelete="CASCADE"), nullable=False, index=True
    )
    role_id: Mapped[str] = mapped_column(
        String(64), ForeignKey("roles.id", ondelete="CASCADE"), nullable=False, index=True
    )
    assigned_by: Mapped[str] = mapped_column(String(64), default="system", nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )

    # リレーション
    role: Mapped[Role] = relationship("Role", back_populates="user_roles")


class ResourcePermission(Base):
    """リソースパーミッションモデル.

    特定ロールに対するリソース単位のアクセス制御を定義。
    レコードが存在しない場合はデフォルトでフルアクセス許可（Default-open）。
    """

    __tablename__ = "resource_permissions"
    __table_args__ = (
        UniqueConstraint("role_id", "resource_type", "resource_id", name="uq_resource_perm"),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    role_id: Mapped[str] = mapped_column(
        String(64), ForeignKey("roles.id", ondelete="CASCADE"), nullable=False, index=True
    )
    resource_type: Mapped[str] = mapped_column(String(100), nullable=False, index=True)
    resource_id: Mapped[str] = mapped_column(String(200), nullable=False)
    permission_level: Mapped[str] = mapped_column(
        String(20), default="read", nullable=False
    )  # none / read / write / admin
    conditions: Mapped[str | None] = mapped_column(Text, nullable=True)  # JSON 条件
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, onupdate=_utcnow, nullable=False
    )

    # リレーション
    role: Mapped[Role] = relationship("Role", back_populates="resource_permissions")


class ResourceDefinition(Base):
    """リソース定義マスタ.

    auth_service が管理するリソースの標準定義。
    各 App はこのテーブルに自分のリソース（vector_db, business_db, kb 等）を登録し、
    resource_permissions と組み合わせて role→scope→backend の解決を実現する。
    """

    __tablename__ = "resource_definitions"
    __table_args__ = (
        UniqueConstraint("resource_type", "resource_id", name="uq_resource_def"),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    resource_type: Mapped[str] = mapped_column(
        String(100), nullable=False, index=True
    )
    resource_id: Mapped[str] = mapped_column(String(200), nullable=False)
    display_name: Mapped[str] = mapped_column(String(200), nullable=False)
    app_name: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    scope: Mapped[str] = mapped_column(String(100), default="", nullable=False)
    backend_key: Mapped[str] = mapped_column(
        String(100), default="shared", nullable=False
    )
    metadata_json: Mapped[str | None] = mapped_column(
        "metadata", Text, nullable=True
    )  # JSON 文字列（SQLite 互換）
    is_active: Mapped[bool] = mapped_column(
        Boolean, default=True, nullable=False
    )
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, nullable=False
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=_utcnow, onupdate=_utcnow, nullable=False
    )
