"""認可コアサービス.

ユーザーパーミッション解決、ワイルドカードマッチ、リソースアクセスチェック、
TTL 付きインメモリキャッシュを提供する。
"""

from __future__ import annotations

import logging
import time
from typing import Any

from sqlalchemy import select

from apps.auth_service.config import get_settings
from apps.auth_service.db.session import get_db_session
from apps.auth_service.models.authorization import (
    Permission,
    ResourcePermission,
    Role,
    RolePermission,
    UserRole,
)
from apps.auth_service.models.user import UserAccount


logger = logging.getLogger(__name__)

# パーミッションレベルの序列（数値が大きいほど高い権限）
PERMISSION_LEVEL_ORDER: dict[str, int] = {
    "none": 0,
    "read": 1,
    "write": 2,
    "admin": 3,
}


class _CacheEntry:
    """TTL 付きキャッシュエントリ."""

    __slots__ = ("value", "expires_at")

    def __init__(self, value: Any, ttl: float) -> None:
        self.value = value
        self.expires_at = time.monotonic() + ttl

    @property
    def is_expired(self) -> bool:
        return time.monotonic() > self.expires_at


class AuthorizationService:
    """認可サービス.

    ユーザーのパーミッション解決、権限チェック、リソースアクセス制御を提供。
    TTL 付きインメモリキャッシュでパフォーマンスを最適化。
    """

    def __init__(self) -> None:
        """初期化."""
        self._cache: dict[str, _CacheEntry] = {}

    @property
    def _ttl(self) -> int:
        return get_settings().AUTHZ_CACHE_TTL_SECONDS

    async def get_user_permissions(self, user_id: str) -> list[str]:
        """ユーザーの有効パーミッション一覧を取得.

        user_roles → role_permissions → permissions.name を解決。
        user_roles が空の場合は UserAccount.role にフォールバック。

        Args:
            user_id: ユーザーID

        Returns:
            パーミッション名の一覧
        """
        cache_key = f"perms:{user_id}"
        cached = self._get_cached(cache_key)
        if cached is not None:
            return cached  # type: ignore[return-value]

        permissions = await self._resolve_permissions(user_id)
        self._set_cache(cache_key, permissions)
        return permissions

    async def get_user_roles(self, user_id: str) -> list[str]:
        """ユーザーのロール名一覧を取得.

        Args:
            user_id: ユーザーID

        Returns:
            ロール名の一覧
        """
        cache_key = f"roles:{user_id}"
        cached = self._get_cached(cache_key)
        if cached is not None:
            return cached  # type: ignore[return-value]

        roles = await self._resolve_roles(user_id)
        self._set_cache(cache_key, roles)
        return roles

    async def has_permission(self, user_id: str, permission: str) -> bool:
        """ユーザーが指定パーミッションを保持しているか確認.

        ワイルドカード "*" およびプレフィックスマッチ "users:*" をサポート。

        Args:
            user_id: ユーザーID
            permission: 要求パーミッション（例: "faq:write"）

        Returns:
            権限がある場合 True
        """
        user_permissions = await self.get_user_permissions(user_id)
        return any(
            self.match_permission(held, permission) for held in user_permissions
        )

    async def check_resource_access(
        self,
        user_id: str,
        resource_type: str,
        resource_id: str,
        required_level: str = "read",
    ) -> dict[str, Any]:
        """リソースアクセスチェック.

        resource_permissions テーブルを検索し、アクセス可否を判定。
        レコードなし → Default-open（全ロールにフルアクセス許可）。
        レコードあり → permission_level と required_level を比較。

        Args:
            user_id: ユーザーID
            resource_type: リソース種別（例: "vector_db", "business_db"）
            resource_id: リソースID（例: "faq_knowledge"）
            required_level: 要求レベル（none/read/write/admin）

        Returns:
            {"allowed": bool, "reason": str, "level": str}
        """
        settings = get_settings()

        # required_level の検証
        if required_level not in PERMISSION_LEVEL_ORDER:
            raise ValueError(f"不明な required_level: {required_level!r}")

        # admin は常に許可
        user_permissions = await self.get_user_permissions(user_id)
        if "*" in user_permissions:
            return {"allowed": True, "reason": "admin_wildcard", "level": "admin"}

        user_roles = await self.get_user_roles(user_id)
        if not user_roles:
            if settings.AUTHZ_DEFAULT_OPEN:
                return {"allowed": True, "reason": "no_roles_default_open", "level": "none"}
            return {"allowed": False, "reason": "no_roles", "level": "none"}

        # ロール ID を取得
        async with get_db_session() as session:
            result = await session.execute(
                select(Role.id, Role.name).where(Role.name.in_(user_roles))
            )
            role_rows = result.all()
            role_ids = [row[0] for row in role_rows]

            if not role_ids:
                if settings.AUTHZ_DEFAULT_OPEN:
                    return {"allowed": True, "reason": "no_roles_default_open", "level": "none"}
                return {"allowed": False, "reason": "no_roles", "level": "none"}

            # resource_permissions を検索
            result = await session.execute(
                select(ResourcePermission).where(
                    ResourcePermission.role_id.in_(role_ids),
                    ResourcePermission.resource_type == resource_type,
                    ResourcePermission.resource_id == resource_id,
                )
            )
            resource_perms = result.scalars().all()

        # マッピングなし → Default-open
        if not resource_perms:
            if settings.AUTHZ_DEFAULT_OPEN:
                return {"allowed": True, "reason": "no_mapping_default_open", "level": "none"}
            return {"allowed": False, "reason": "no_mapping", "level": "none"}

        # 最高レベルのパーミッションを採用
        max_level = "none"
        for rp in resource_perms:
            level_val = PERMISSION_LEVEL_ORDER.get(rp.permission_level, 0)
            if level_val > PERMISSION_LEVEL_ORDER.get(max_level, 0):
                max_level = rp.permission_level

        required_val = PERMISSION_LEVEL_ORDER[required_level]
        granted_val = PERMISSION_LEVEL_ORDER.get(max_level, 0)

        if granted_val >= required_val:
            return {"allowed": True, "reason": "resource_permission_granted", "level": max_level}
        return {
            "allowed": False,
            "reason": "insufficient_resource_permission",
            "level": max_level,
        }

    async def resolve_scopes(
        self,
        role_name: str,
        app_name: str,
        resource_type: str = "vector_db",
    ) -> list[dict[str, Any]]:
        """ロールが許可されたスコープ一覧を解決.

        resource_permissions と resource_definitions を結合し、
        ロールがアクセス可能なスコープ + backend 情報を返す。

        Args:
            role_name: ロール名
            app_name: App 名（フィルタ）
            resource_type: リソース種別（既定: vector_db）

        Returns:
            [{"scope": str, "resource_id": str, "backend_key": str,
              "collection_tpl": str, "permission_level": str}, ...]
        """
        import json as _json

        from apps.auth_service.models.authorization import ResourceDefinition

        async with get_db_session() as session:
            # ロール ID を取得
            role_id = await session.scalar(
                select(Role.id).where(Role.name == role_name)
            )
            if role_id is None:
                return []

            # resource_permissions から許可されたリソースを取得
            rp_result = await session.execute(
                select(ResourcePermission).where(
                    ResourcePermission.role_id == role_id,
                    ResourcePermission.resource_type == resource_type,
                    ResourcePermission.permission_level != "none",
                )
            )
            resource_perms = rp_result.scalars().all()

            if not resource_perms:
                return []

            # 許可された resource_id 一覧
            permitted_ids = {rp.resource_id for rp in resource_perms}
            perm_map = {rp.resource_id: rp.permission_level for rp in resource_perms}

            # resource_definitions から詳細情報を取得
            rd_result = await session.execute(
                select(ResourceDefinition).where(
                    ResourceDefinition.resource_type == resource_type,
                    ResourceDefinition.app_name == app_name,
                    ResourceDefinition.resource_id.in_(permitted_ids),
                    ResourceDefinition.is_active.is_(True),
                )
            )
            definitions = rd_result.scalars().all()

            scopes: list[dict[str, Any]] = []
            for rd in definitions:
                metadata: dict[str, Any] = {}
                if rd.metadata_json:
                    try:
                        metadata = _json.loads(rd.metadata_json)
                    except (ValueError, TypeError):
                        pass
                scopes.append({
                    "scope": rd.scope,
                    "resource_id": rd.resource_id,
                    "backend_key": rd.backend_key,
                    "collection_tpl": metadata.get("collection_tpl", ""),
                    "permission_level": perm_map.get(rd.resource_id, "read"),
                    "metadata": metadata,
                })

            return scopes

    def invalidate_cache(self, user_id: str | None = None) -> None:
        """キャッシュを無効化.

        Args:
            user_id: 特定ユーザーのキャッシュを削除。None の場合は全削除。
        """
        if user_id is None:
            self._cache.clear()
            return
        keys_to_delete = [
            k for k in self._cache
            if k == f"perms:{user_id}" or k == f"roles:{user_id}"
        ]
        for key in keys_to_delete:
            del self._cache[key]

    @staticmethod
    def match_permission(held: str, required: str) -> bool:
        """保持パーミッションが要求パーミッションにマッチするか判定.

        マッチルール:
        - "*" は全てにマッチ
        - "users:*" は "users:" プレフィックスの全てにマッチ
        - 完全一致

        Args:
            held: 保持しているパーミッション
            required: 要求されているパーミッション

        Returns:
            マッチする場合 True
        """
        if held == "*":
            return True
        if held == required:
            return True
        # プレフィックスマッチ: "faq:*" → "faq:read", "faq:write" にマッチ
        if held.endswith(":*"):
            prefix = held[:-1]  # "faq:" 部分
            if required.startswith(prefix):
                return True
        return False

    # ------------------------------------------------------------------
    # プライベート
    # ------------------------------------------------------------------

    async def _resolve_permissions(self, user_id: str) -> list[str]:
        """ユーザーの全パーミッションを DB から解決."""
        async with get_db_session() as session:
            # user_roles からロール ID を取得
            result = await session.execute(
                select(UserRole.role_id).where(UserRole.user_id == user_id)
            )
            role_ids = [row[0] for row in result.all()]

            # user_roles が空 → UserAccount.role にフォールバック
            if not role_ids:
                account = await session.get(UserAccount, user_id)
                if account is None:
                    return []
                fallback_result = await session.execute(
                    select(Role.id).where(Role.name == account.role)
                )
                role_ids = [row[0] for row in fallback_result.all()]
                if not role_ids:
                    return []

            # role_permissions → permissions.name
            result = await session.execute(
                select(Permission.name)
                .join(RolePermission, RolePermission.permission_id == Permission.id)
                .where(RolePermission.role_id.in_(role_ids))
            )
            permissions = list({row[0] for row in result.all()})
            return sorted(permissions)

    async def _resolve_roles(self, user_id: str) -> list[str]:
        """ユーザーのロール名を DB から解決."""
        async with get_db_session() as session:
            result = await session.execute(
                select(Role.name)
                .join(UserRole, UserRole.role_id == Role.id)
                .where(UserRole.user_id == user_id)
            )
            role_names = [row[0] for row in result.all()]

            # user_roles が空 → UserAccount.role にフォールバック
            if not role_names:
                account = await session.get(UserAccount, user_id)
                if account is not None and account.role:
                    # ロールが roles テーブルに存在するか検証
                    exists = await session.scalar(
                        select(Role.name).where(Role.name == account.role)
                    )
                    if exists:
                        role_names = [account.role]

            return sorted(role_names)

    def _get_cached(self, key: str) -> Any | None:
        """キャッシュから取得."""
        entry = self._cache.get(key)
        if entry is None:
            return None
        if entry.is_expired:
            del self._cache[key]
            return None
        return entry.value

    def _set_cache(self, key: str, value: Any) -> None:
        """キャッシュに設定."""
        self._cache[key] = _CacheEntry(value, self._ttl)


# ---------------------------------------------------------------------------
# シングルトン
# ---------------------------------------------------------------------------

_authz_singleton: AuthorizationService | None = None


def get_authorization_service() -> AuthorizationService:
    """AuthorizationService シングルトンを取得."""
    global _authz_singleton
    if _authz_singleton is None:
        _authz_singleton = AuthorizationService()
    return _authz_singleton


def reset_authorization_service() -> None:
    """テスト用にシングルトンをリセット."""
    global _authz_singleton
    _authz_singleton = None
