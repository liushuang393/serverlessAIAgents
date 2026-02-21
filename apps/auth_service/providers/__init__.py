"""認証プロバイダーパッケージ.

Strategy パターンで認証プロバイダーを切り替え可能にする。
各プロバイダーは AuthProvider 抽象基底クラスを実装する。
"""

from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity
from apps.auth_service.providers.local import LocalDBProvider


def get_provider(provider_name: str) -> AuthProvider:
    """プロバイダー名からプロバイダーインスタンスを取得.

    Args:
        provider_name: プロバイダー名（local_db / google / azure_ad / ldap / saml / proxy）

    Returns:
        AuthProvider インスタンス

    Raises:
        ValueError: 未知のプロバイダー名の場合
    """
    from apps.auth_service.config import get_settings

    settings = get_settings()

    if provider_name == "local_db":
        return LocalDBProvider(settings=settings)

    if provider_name in {"google", "azure_ad"}:
        from apps.auth_service.providers.azure_ad import AzureADProvider
        from apps.auth_service.providers.google import GoogleOAuth2Provider

        if provider_name == "google":
            return GoogleOAuth2Provider(settings=settings)
        return AzureADProvider(settings=settings)

    if provider_name == "ldap":
        from apps.auth_service.providers.ldap import LDAPProvider

        return LDAPProvider(settings=settings)

    if provider_name == "saml":
        from apps.auth_service.providers.saml import SAMLProvider

        return SAMLProvider(settings=settings)

    if provider_name == "proxy":
        from apps.auth_service.providers.proxy import ProxyAuthProvider

        return ProxyAuthProvider(settings=settings)

    msg = f"未知の認証プロバイダー: {provider_name}"
    raise ValueError(msg)


__all__ = [
    "AuthProvider",
    "AuthResult",
    "ExternalIdentity",
    "LocalDBProvider",
    "get_provider",
]
