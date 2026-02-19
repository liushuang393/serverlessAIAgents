"""RBAC（ロールベースアクセス制御）モジュール.

ロールとパーミッションベースのアクセス制御を提供します。

特徴:
- ロール定義
- パーミッション継承
- ワイルドカードサポート
- 階層的ロール
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class Permission:
    """パーミッション.

    Attributes:
        name: パーミッション名（例: "users:read"）
        description: 説明
    """

    name: str
    description: str = ""

    def matches(self, required: str) -> bool:
        """パーミッションがマッチするかを確認.

        Args:
            required: 必要なパーミッション

        Returns:
            マッチする場合 True
        """
        # 完全一致
        if self.name == required:
            return True

        # ワイルドカード
        if self.name == "*":
            return True

        # プレフィックスワイルドカード（例: "users:*" は "users:read" にマッチ）
        if self.name.endswith(":*"):
            prefix = self.name[:-2]
            return required.startswith(prefix + ":")

        return False


@dataclass
class Role:
    """ロール.

    Attributes:
        name: ロール名
        permissions: パーミッションリスト
        inherits: 継承するロール
        description: 説明
    """

    name: str
    permissions: list[Permission] = field(default_factory=list)
    inherits: list[str] = field(default_factory=list)
    description: str = ""

    def add_permission(
        self,
        name: str,
        description: str = "",
    ) -> None:
        """パーミッションを追加.

        Args:
            name: パーミッション名
            description: 説明
        """
        self.permissions.append(Permission(name=name, description=description))

    def has_permission(self, permission: str) -> bool:
        """パーミッションを持っているかを確認.

        Note:
            継承は考慮しません。RBACManager.has_permission() を使用してください。

        Args:
            permission: パーミッション名

        Returns:
            持っている場合 True
        """
        return any(p.matches(permission) for p in self.permissions)


class RBACManager:
    """RBAC マネージャー.

    ロールとパーミッションを管理します。

    Example:
        >>> rbac = RBACManager()
        >>> rbac.define_role("admin", permissions=["*"])
        >>> rbac.define_role("user", permissions=["read"])
        >>> rbac.assign_role("user-123", "user")
        >>> rbac.has_permission("user-123", "read")  # True
    """

    def __init__(self) -> None:
        """初期化."""
        self._roles: dict[str, Role] = {}
        self._user_roles: dict[str, list[str]] = {}
        self._logger = logging.getLogger(__name__)

        # デフォルトロールを定義
        self._define_default_roles()

    def _define_default_roles(self) -> None:
        """デフォルトロールを定義."""
        # 管理者
        admin = Role(name="admin", description="Full access")
        admin.add_permission("*")
        self._roles["admin"] = admin

        # 一般ユーザー
        user = Role(name="user", description="Basic user access")
        user.add_permission("read")
        user.add_permission("write")
        self._roles["user"] = user

        # 読み取り専用
        readonly = Role(name="readonly", description="Read-only access")
        readonly.add_permission("read")
        self._roles["readonly"] = readonly

    def define_role(
        self,
        name: str,
        permissions: list[str] | None = None,
        inherits: list[str] | None = None,
        description: str = "",
    ) -> Role:
        """ロールを定義.

        Args:
            name: ロール名
            permissions: パーミッション名リスト
            inherits: 継承するロール名リスト
            description: 説明

        Returns:
            Role インスタンス
        """
        role = Role(
            name=name,
            inherits=inherits or [],
            description=description,
        )

        if permissions:
            for perm in permissions:
                role.add_permission(perm)

        self._roles[name] = role
        self._logger.info(f"Defined role: {name}")
        return role

    def get_role(self, name: str) -> Role | None:
        """ロールを取得.

        Args:
            name: ロール名

        Returns:
            Role、または None
        """
        return self._roles.get(name)

    def assign_role(self, user_id: str, role_name: str) -> bool:
        """ユーザーにロールを割り当て.

        Args:
            user_id: ユーザー ID
            role_name: ロール名

        Returns:
            成功した場合 True
        """
        if role_name not in self._roles:
            self._logger.warning(f"Role not found: {role_name}")
            return False

        if user_id not in self._user_roles:
            self._user_roles[user_id] = []

        if role_name not in self._user_roles[user_id]:
            self._user_roles[user_id].append(role_name)
            self._logger.info(f"Assigned role {role_name} to user {user_id}")

        return True

    def revoke_role(self, user_id: str, role_name: str) -> bool:
        """ユーザーからロールを削除.

        Args:
            user_id: ユーザー ID
            role_name: ロール名

        Returns:
            成功した場合 True
        """
        if user_id not in self._user_roles:
            return False

        if role_name in self._user_roles[user_id]:
            self._user_roles[user_id].remove(role_name)
            self._logger.info(f"Revoked role {role_name} from user {user_id}")
            return True

        return False

    def get_user_roles(self, user_id: str) -> list[str]:
        """ユーザーのロールを取得.

        Args:
            user_id: ユーザー ID

        Returns:
            ロール名リスト
        """
        return self._user_roles.get(user_id, [])

    def get_user_permissions(self, user_id: str) -> list[str]:
        """ユーザーの全パーミッションを取得（継承を含む）.

        Args:
            user_id: ユーザー ID

        Returns:
            パーミッション名リスト
        """
        permissions = set()
        visited_roles = set()

        def collect_permissions(role_name: str) -> None:
            if role_name in visited_roles:
                return
            visited_roles.add(role_name)

            role = self._roles.get(role_name)
            if not role:
                return

            for perm in role.permissions:
                permissions.add(perm.name)

            for inherited in role.inherits:
                collect_permissions(inherited)

        for role_name in self._user_roles.get(user_id, []):
            collect_permissions(role_name)

        return list(permissions)

    def has_permission(
        self,
        user_id: str,
        permission: str,
    ) -> bool:
        """ユーザーがパーミッションを持っているか確認.

        Args:
            user_id: ユーザー ID
            permission: パーミッション名

        Returns:
            持っている場合 True
        """
        visited_roles = set()

        def check_role(role_name: str) -> bool:
            if role_name in visited_roles:
                return False
            visited_roles.add(role_name)

            role = self._roles.get(role_name)
            if not role:
                return False

            # このロールのパーミッションをチェック
            for perm in role.permissions:
                if perm.matches(permission):
                    return True

            # 継承ロールをチェック
            return any(check_role(inherited) for inherited in role.inherits)

        return any(check_role(role_name) for role_name in self._user_roles.get(user_id, []))

    def has_role(self, user_id: str, role_name: str) -> bool:
        """ユーザーがロールを持っているか確認.

        Args:
            user_id: ユーザー ID
            role_name: ロール名

        Returns:
            持っている場合 True
        """
        return role_name in self._user_roles.get(user_id, [])

    def list_roles(self) -> list[Role]:
        """全ロールを一覧.

        Returns:
            Role のリスト
        """
        return list(self._roles.values())

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換.

        Returns:
            辞書
        """
        return {
            "roles": {
                name: {
                    "permissions": [p.name for p in role.permissions],
                    "inherits": role.inherits,
                    "description": role.description,
                }
                for name, role in self._roles.items()
            },
            "user_roles": dict(self._user_roles),
        }
