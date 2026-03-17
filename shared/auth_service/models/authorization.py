import sqlalchemy as sa
from sqlalchemy.orm import relationship
from shared.auth_service.db.session import Base

class Permission(Base):
    """認可パーミッション."""
    __tablename__ = "permissions"

    id = sa.Column(sa.String(64), primary_key=True)
    name = sa.Column(sa.String(100), unique=True, nullable=False, index=True)
    display_name = sa.Column(sa.String(100), nullable=False)
    description = sa.Column(sa.Text)
    resource_type = sa.Column(sa.String(100))
    action = sa.Column(sa.String(100))
    is_system = sa.Column(sa.Boolean, default=False)
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))

class Role(Base):
    """ロール."""
    __tablename__ = "roles"

    id = sa.Column(sa.String(64), primary_key=True)
    name = sa.Column(sa.String(100), unique=True, nullable=False, index=True)
    display_name = sa.Column(sa.String(100), nullable=False)
    description = sa.Column(sa.Text)
    is_system = sa.Column(sa.Boolean, default=False)
    priority = sa.Column(sa.Integer, default=0)
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))

class RolePermission(Base):
    """ロールとパーミッションの関連付け."""
    __tablename__ = "role_permissions"

    id = sa.Column(sa.String(64), primary_key=True)
    role_id = sa.Column(sa.String(64), sa.ForeignKey("roles.id", ondelete="CASCADE"), nullable=False)
    permission_id = sa.Column(sa.String(64), sa.ForeignKey("permissions.id", ondelete="CASCADE"), nullable=False)
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))

class UserRole(Base):
    """ユーザーとロールの関連付け."""
    __tablename__ = "user_roles"

    id = sa.Column(sa.String(64), primary_key=True)
    user_id = sa.Column(sa.String(64), sa.ForeignKey("user_accounts.id", ondelete="CASCADE"), nullable=False)
    role_id = sa.Column(sa.String(64), sa.ForeignKey("roles.id", ondelete="CASCADE"), nullable=False)
    assigned_by = sa.Column(sa.String(100))
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))

class ResourceDefinition(Base):
    """認可リソース定義 (例: "kb_collection")."""
    __tablename__ = "resource_definitions"

    id = sa.Column(sa.String(64), primary_key=True)
    resource_type = sa.Column(sa.String(100), nullable=False, index=True)
    resource_id = sa.Column(sa.String(255), nullable=False, index=True)
    display_name = sa.Column(sa.String(100), nullable=False)
    app_name = sa.Column(sa.String(100))
    scope = sa.Column(sa.String(100))
    backend_key = sa.Column(sa.String(100))
    metadata_json = sa.Column(sa.Text)
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))

class ResourcePermission(Base):
    """リソースに対するユーザー/ロールのパーミッション."""
    __tablename__ = "resource_permissions"

    id = sa.Column(sa.String(64), primary_key=True)
    role_id = sa.Column(sa.String(64), sa.ForeignKey("roles.id", ondelete="CASCADE"))
    user_id = sa.Column(sa.String(64), sa.ForeignKey("user_accounts.id", ondelete="CASCADE"))
    resource_type = sa.Column(sa.String(100), nullable=False, index=True)
    resource_id = sa.Column(sa.String(255), nullable=False, index=True)
    permission_level = sa.Column(sa.String(100), nullable=False)  # "read", "write", "admin" 等
    created_at = sa.Column(sa.DateTime, server_default=sa.text("now()"))
