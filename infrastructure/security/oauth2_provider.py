"""infrastructure.security.oauth2_provider — OAuth2 外部 ID プロバイダー.

外部 OAuth2 認証で解決されたユーザー情報（ExternalIdentity）を提供する。

使用例:
    identity = ExternalIdentity(
        provider="google",
        username="tanaka",
        email="tanaka@example.com",
        display_name="田中 一郎",
    )
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass
class ExternalIdentity:
    """外部 OAuth2 / SAML / LDAP 認証で解決されたユーザー情報.

    Attributes:
        provider: 認証プロバイダー識別子 (例: "google", "azure_ad", "ldap", "saml")。
        username: ユーザー名（一意識別子）。
        email: メールアドレス。
        display_name: 表示名。
        department: 部署名。
        position: 役職。
        role: ロール（例: "admin", "employee"）。
        raw: プロバイダーから返された生の属性情報（任意）。
    """

    provider: str = ""
    username: str = ""
    email: str = ""
    display_name: str = ""
    department: str = ""
    position: str = ""
    role: str = "employee"
    raw: dict[str, Any] = field(default_factory=dict)

    @classmethod
    def from_google(cls, token_info: dict[str, Any]) -> "ExternalIdentity":
        """Google OAuth2 トークン情報から ExternalIdentity を生成する.

        Args:
            token_info: Google の userinfo エンドポイントから返された辞書。

        Returns:
            ExternalIdentity インスタンス。
        """
        email: str = token_info.get("email", "")
        username = email.split("@")[0] if email else token_info.get("sub", "")
        return cls(
            provider="google",
            username=username,
            email=email,
            display_name=token_info.get("name", username),
            raw=token_info,
        )

    @classmethod
    def from_azure_ad(cls, token_info: dict[str, Any]) -> "ExternalIdentity":
        """Azure AD トークン情報から ExternalIdentity を生成する.

        Args:
            token_info: Azure AD の userinfo / token payload から返された辞書。

        Returns:
            ExternalIdentity インスタンス。
        """
        email: str = token_info.get("email", token_info.get("upn", ""))
        username = email.split("@")[0] if email else token_info.get("oid", "")
        return cls(
            provider="azure_ad",
            username=username,
            email=email,
            display_name=token_info.get("name", token_info.get("displayName", username)),
            department=token_info.get("department", ""),
            position=token_info.get("jobTitle", ""),
            raw=token_info,
        )

    @classmethod
    def from_saml(cls, attributes: dict[str, Any]) -> "ExternalIdentity":
        """SAML アサーションから ExternalIdentity を生成する.

        Args:
            attributes: SAML アサーションの属性辞書。

        Returns:
            ExternalIdentity インスタンス。
        """
        email_list = attributes.get("email", attributes.get("EmailAddress", []))
        email = email_list[0] if isinstance(email_list, list) and email_list else str(email_list)
        username = email.split("@")[0] if email else ""
        display_name_list = attributes.get("displayName", attributes.get("name", [username]))
        display_name = (
            display_name_list[0]
            if isinstance(display_name_list, list) and display_name_list
            else str(display_name_list)
        )
        return cls(
            provider="saml",
            username=username,
            email=email,
            display_name=display_name,
            raw=attributes,
        )


__all__ = ["ExternalIdentity"]

