"""LDAP / Active Directory 認証プロバイダー.

ldap3 ライブラリを使用した LDAP バインド認証を実装する。
"""

from __future__ import annotations

import asyncio
import contextlib
import json
import logging
from typing import TYPE_CHECKING, Any

from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)


class LDAPProvider(AuthProvider):
    """LDAP 認証プロバイダー.

    LDAP_SERVER, LDAP_BASE_DN, LDAP_BIND_DN_TEMPLATE を使って
    ディレクトリサーバーに対してバインド認証を行う。
    """

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)

    @property
    def provider_name(self) -> str:
        return "ldap"

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """LDAP でユーザーを認証.

        Args:
            username: LDAP ユーザー名
            password: パスワード

        Returns:
            AuthResult
        """
        if not self._settings.LDAP_SERVER:
            logger.error("LDAP_SERVER が未設定です")
            return AuthResult(success=False, message="LDAP サーバーが設定されていません")

        identity = await asyncio.to_thread(
            self._authenticate_sync,
            self._settings.LDAP_SERVER,
            self._settings.LDAP_BIND_DN_TEMPLATE,
            self._settings.LDAP_BASE_DN,
            self._settings.LDAP_USER_FILTER,
            username,
            password,
        )
        if identity is None:
            return AuthResult(success=False, message="LDAP 認証に失敗しました")
        return AuthResult(success=True, message="ログイン成功", identity=identity)

    def _authenticate_sync(
        self,
        server_uri: str,
        bind_dn_template: str,
        base_dn: str,
        user_filter_template: str,
        username: str,
        password: str,
    ) -> ExternalIdentity | None:
        """LDAP 同期認証（スレッド実行用）."""
        try:
            import ldap3  # type: ignore[import-not-found]
        except ImportError:
            logger.exception("ldap3 がインストールされていません: pip install ldap3")
            return None

        user_dn = bind_dn_template.format(
            username=username,
            base_dn=base_dn,
        )
        server = ldap3.Server(server_uri, get_info=ldap3.ALL)
        conn = ldap3.Connection(server, user=user_dn, password=password, auto_bind=False)

        if not conn.bind():
            logger.info("LDAP バインド失敗: %s", user_dn)
            return None

        if not base_dn:
            return ExternalIdentity(
                username=username,
                display_name=username,
                role=self._settings.LDAP_DEFAULT_ROLE,
                email=f"{username}@example.com",
            )

        search_filter = user_filter_template.format(username=username)
        conn.search(
            search_base=base_dn,
            search_filter=search_filter,
            attributes=["displayName", "cn", "mail", "department", "title", "memberOf"],
        )

        if not conn.entries:
            return ExternalIdentity(
                username=username,
                display_name=username,
                role=self._settings.LDAP_DEFAULT_ROLE,
                email=f"{username}@example.com",
            )

        entry = conn.entries[0]
        display_name = str(getattr(entry, "displayName", None) or getattr(entry, "cn", None) or username)
        email = str(getattr(entry, "mail", None) or f"{username}@example.com")
        department = str(getattr(entry, "department", None) or "")
        position = str(getattr(entry, "title", None) or "")

        # グループからロールを解決
        groups: list[str] = []
        member_of = getattr(entry, "memberOf", None)
        if member_of:
            if isinstance(member_of, list):
                groups = [str(g) for g in member_of]
            else:
                groups = [str(member_of)]

        role = self._resolve_role(groups)

        return ExternalIdentity(
            username=username,
            display_name=display_name,
            email=email,
            role=role,
            department=department,
            position=position,
        )

    def _resolve_role(self, groups: list[str]) -> str:
        """グループリストからロールを解決.

        Args:
            groups: LDAP グループ DN リスト

        Returns:
            解決されたロール名
        """
        role_mapping: dict[str, Any] = {}
        with contextlib.suppress(json.JSONDecodeError, AttributeError):
            role_mapping = json.loads(self._settings.LDAP_ROLE_MAPPING)

        resolved = self._settings.LDAP_DEFAULT_ROLE
        for group_dn, mapped_role in role_mapping.items():
            if group_dn in groups:
                resolved = str(mapped_role)
                if resolved == "admin":
                    break
        return resolved
