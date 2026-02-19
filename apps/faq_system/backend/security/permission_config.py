"""権限設定（Permission Config）.

FAQ システムの権限管理設定。

機能:
- ロール別権限定義
- KB別アクセス制御
- フィールド制限（MyNumber等）

使用例:
    >>> from apps.faq_system.backend.security import PermissionConfig
    >>>
    >>> config = PermissionConfig()
    >>> allowed = config.check_access(
    ...     user_role="employee",
    ...     kb_type="internal",
    ...     action="read",
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class PermissionLevel(str, Enum):
    """権限レベル."""

    NONE = "none"  # アクセス不可
    READ = "read"  # 読み取りのみ
    WRITE = "write"  # 読み書き
    ADMIN = "admin"  # 管理者


class KBPermission(str, Enum):
    """KB 権限."""

    INTERNAL_READ = "internal:read"
    INTERNAL_WRITE = "internal:write"
    EXTERNAL_READ = "external:read"
    EXTERNAL_WRITE = "external:write"
    CONFIDENTIAL_READ = "confidential:read"
    CONFIDENTIAL_WRITE = "confidential:write"
    ANALYTICS_READ = "analytics:read"
    ANALYTICS_WRITE = "analytics:write"


@dataclass
class FieldRestriction:
    """フィールド制限.

    Attributes:
        field_pattern: フィールドパターン（table.column または *.column）
        reason: 制限理由
        allowed_roles: アクセス許可ロール
        requires_audit: 監査必須か
    """

    field_pattern: str
    reason: str = ""
    allowed_roles: list[str] = field(default_factory=list)
    requires_audit: bool = True


@dataclass
class RolePermissions:
    """ロール権限.

    Attributes:
        role: ロール名
        description: 説明
        kb_permissions: KB 権限リスト
        can_export: エクスポート可否
        can_bulk_query: 一括照会可否
        max_results_per_query: 1クエリあたりの最大結果数
        allowed_topics: 許可トピック（空の場合は全て）
        denied_topics: 禁止トピック
    """

    role: str
    description: str = ""
    kb_permissions: list[KBPermission] = field(default_factory=list)
    can_export: bool = False
    can_bulk_query: bool = False
    max_results_per_query: int = 100
    allowed_topics: list[str] = field(default_factory=list)
    denied_topics: list[str] = field(default_factory=list)


@dataclass
class PermissionConfigData:
    """権限設定データ."""

    # ロール定義
    roles: dict[str, RolePermissions] = field(default_factory=dict)

    # フィールド制限
    restricted_fields: list[FieldRestriction] = field(default_factory=list)

    # デフォルト設定
    default_role: str = "guest"
    require_auth_for_internal: bool = True
    require_auth_for_external: bool = False


class PermissionConfig:
    """権限設定マネージャー.

    FAQ システムの権限管理。

    Example:
        >>> config = PermissionConfig()
        >>>
        >>> # アクセスチェック
        >>> allowed = config.check_access(
        ...     user_role="employee",
        ...     kb_type="internal",
        ...     action="read",
        ... )
        >>>
        >>> # フィールド制限チェック
        >>> restricted = config.is_field_restricted(
        ...     field_name="employees.mynumber",
        ...     user_role="employee",
        ... )
    """

    # デフォルトロール定義
    DEFAULT_ROLES = {
        "guest": RolePermissions(
            role="guest",
            description="未認証ユーザー",
            kb_permissions=[KBPermission.EXTERNAL_READ],
            can_export=False,
            max_results_per_query=10,
        ),
        "employee": RolePermissions(
            role="employee",
            description="一般社員",
            kb_permissions=[
                KBPermission.INTERNAL_READ,
                KBPermission.EXTERNAL_READ,
            ],
            can_export=False,
            max_results_per_query=100,
        ),
        "manager": RolePermissions(
            role="manager",
            description="管理職",
            kb_permissions=[
                KBPermission.INTERNAL_READ,
                KBPermission.INTERNAL_WRITE,
                KBPermission.EXTERNAL_READ,
                KBPermission.CONFIDENTIAL_READ,
                KBPermission.ANALYTICS_READ,
            ],
            can_export=True,
            can_bulk_query=True,
            max_results_per_query=500,
        ),
        "analyst": RolePermissions(
            role="analyst",
            description="データ分析者",
            kb_permissions=[
                KBPermission.INTERNAL_READ,
                KBPermission.EXTERNAL_READ,
                KBPermission.ANALYTICS_READ,
                KBPermission.ANALYTICS_WRITE,
            ],
            can_export=True,
            can_bulk_query=True,
            max_results_per_query=1000,
            denied_topics=["人事", "給与"],
        ),
        "hr_admin": RolePermissions(
            role="hr_admin",
            description="人事管理者",
            kb_permissions=[
                KBPermission.INTERNAL_READ,
                KBPermission.INTERNAL_WRITE,
                KBPermission.CONFIDENTIAL_READ,
                KBPermission.CONFIDENTIAL_WRITE,
            ],
            can_export=True,
            max_results_per_query=500,
            allowed_topics=["人事", "給与", "福利厚生"],
        ),
        "admin": RolePermissions(
            role="admin",
            description="システム管理者",
            kb_permissions=list(KBPermission),
            can_export=True,
            can_bulk_query=True,
            max_results_per_query=10000,
        ),
    }

    # デフォルトフィールド制限
    DEFAULT_RESTRICTED_FIELDS = [
        FieldRestriction(
            field_pattern="*.mynumber",
            reason="マイナンバー（個人番号）は法律により厳格に管理",
            allowed_roles=["hr_admin", "admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="*.my_number",
            reason="マイナンバー（個人番号）は法律により厳格に管理",
            allowed_roles=["hr_admin", "admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="employees.salary",
            reason="給与情報は機密",
            allowed_roles=["hr_admin", "admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="employees.bonus",
            reason="賞与情報は機密",
            allowed_roles=["hr_admin", "admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="*.password",
            reason="パスワード情報",
            allowed_roles=[],  # 誰もアクセス不可
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="*.credit_card",
            reason="クレジットカード情報",
            allowed_roles=["admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="customers.phone",
            reason="個人電話番号",
            allowed_roles=["manager", "admin"],
            requires_audit=True,
        ),
        FieldRestriction(
            field_pattern="customers.address",
            reason="個人住所",
            allowed_roles=["manager", "admin"],
            requires_audit=True,
        ),
    ]

    def __init__(
        self,
        config: PermissionConfigData | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定データ
        """
        self._config = config or PermissionConfigData()
        self._logger = logging.getLogger(__name__)

        # デフォルトロールを設定
        if not self._config.roles:
            self._config.roles = self.DEFAULT_ROLES.copy()

        # デフォルトフィールド制限を設定
        if not self._config.restricted_fields:
            self._config.restricted_fields = self.DEFAULT_RESTRICTED_FIELDS.copy()

    def check_access(
        self,
        user_role: str,
        kb_type: str,
        action: str,
        topic: str | None = None,
    ) -> dict[str, Any]:
        """アクセス権限をチェック.

        Args:
            user_role: ユーザーロール
            kb_type: KBタイプ
            action: アクション（read, write）
            topic: トピック

        Returns:
            {"allowed": bool, "reason": str, "max_results": int}
        """
        role_perms = self._config.roles.get(
            user_role, self._config.roles.get(self._config.default_role)
        )

        if not role_perms:
            return {
                "allowed": False,
                "reason": f"Unknown role: {user_role}",
                "max_results": 0,
            }

        # KB 権限チェック
        required_perm = KBPermission(f"{kb_type}:{action}")
        if required_perm not in role_perms.kb_permissions:
            return {
                "allowed": False,
                "reason": f"Permission denied: {required_perm.value}",
                "max_results": 0,
            }

        # トピックチェック
        if topic:
            if role_perms.denied_topics and topic in role_perms.denied_topics:
                return {
                    "allowed": False,
                    "reason": f"Topic denied: {topic}",
                    "max_results": 0,
                }

            if role_perms.allowed_topics and topic not in role_perms.allowed_topics:
                return {
                    "allowed": False,
                    "reason": f"Topic not allowed: {topic}",
                    "max_results": 0,
                }

        return {
            "allowed": True,
            "reason": "OK",
            "max_results": role_perms.max_results_per_query,
            "can_export": role_perms.can_export,
            "can_bulk_query": role_perms.can_bulk_query,
        }

    def is_field_restricted(
        self,
        field_name: str,
        user_role: str,
    ) -> dict[str, Any]:
        """フィールドが制限されているかチェック.

        Args:
            field_name: フィールド名（table.column）
            user_role: ユーザーロール

        Returns:
            {"restricted": bool, "reason": str, "requires_audit": bool}
        """
        field_lower = field_name.lower()

        for restriction in self._config.restricted_fields:
            if self._match_field_pattern(field_lower, restriction.field_pattern):
                if user_role in restriction.allowed_roles:
                    return {
                        "restricted": False,
                        "reason": "Allowed by role",
                        "requires_audit": restriction.requires_audit,
                    }
                return {
                    "restricted": True,
                    "reason": restriction.reason,
                    "requires_audit": restriction.requires_audit,
                }

        return {
            "restricted": False,
            "reason": "Not restricted",
            "requires_audit": False,
        }

    def _match_field_pattern(self, field_name: str, pattern: str) -> bool:
        """フィールドパターンがマッチするか.

        Args:
            field_name: フィールド名
            pattern: パターン

        Returns:
            マッチする場合True
        """
        pattern_lower = pattern.lower()

        if pattern_lower.startswith("*."):
            # *.column 形式
            col_name = pattern_lower[2:]
            return field_name.endswith(f".{col_name}") or field_name == col_name

        return field_name == pattern_lower

    def get_role_permissions(self, role: str) -> RolePermissions | None:
        """ロール権限を取得.

        Args:
            role: ロール名

        Returns:
            ロール権限、または None
        """
        return self._config.roles.get(role)

    def list_roles(self) -> list[str]:
        """ロール一覧を取得.

        Returns:
            ロール名リスト
        """
        return list(self._config.roles.keys())

    def add_role(self, role_perms: RolePermissions) -> None:
        """ロールを追加.

        Args:
            role_perms: ロール権限
        """
        self._config.roles[role_perms.role] = role_perms
        self._logger.info("Added role: %s", role_perms.role)

    def add_field_restriction(self, restriction: FieldRestriction) -> None:
        """フィールド制限を追加.

        Args:
            restriction: フィールド制限
        """
        self._config.restricted_fields.append(restriction)
        self._logger.info("Added field restriction: %s", restriction.field_pattern)


__all__ = [
    "FieldRestriction",
    "KBPermission",
    "PermissionConfig",
    "PermissionConfigData",
    "PermissionLevel",
    "RolePermissions",
]
