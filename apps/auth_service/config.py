"""auth_service 設定モジュール.

pydantic-settings を利用した設定管理。
全設定は環境変数または .env ファイルから読み込む。
"""

from __future__ import annotations

from pydantic import field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """auth_service 全設定."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",
    )

    # --- コア設定 ---
    AUTH_SERVICE_PORT: int = 8010
    AUTH_SERVICE_HOST: str = "0.0.0.0"
    AUTH_DATABASE_URL: str = "sqlite+aiosqlite:///./auth_service.db"
    AUTH_DB_AUTO_CREATE: bool = True
    AUTH_DB_ECHO: bool = False

    # --- JWT 設定 ---
    JWT_SECRET_KEY: str = ""
    JWT_ALGORITHM: str = "HS256"
    JWT_ACCESS_EXPIRE_MINUTES: int = 30
    JWT_REFRESH_EXPIRE_DAYS: int = 7
    JWT_ISSUER: str = "auth-service"
    JWT_AUDIENCE: str = "auth-service"

    # --- 認証プロバイダー選択 ---
    # 選択肢: local_db | google | azure_ad | ldap | saml | proxy
    AUTH_PROVIDER: str = "local_db"

    # --- Google OAuth2 (AUTH_PROVIDER=google 時に使用) ---
    GOOGLE_CLIENT_ID: str = ""
    GOOGLE_CLIENT_SECRET: str = ""
    GOOGLE_REDIRECT_URI: str = "http://localhost:8010/auth/oauth2/google/callback"

    # --- Azure AD (AUTH_PROVIDER=azure_ad 時に使用) ---
    AZURE_CLIENT_ID: str = ""
    AZURE_CLIENT_SECRET: str = ""
    AZURE_TENANT_ID: str = ""  # または "common"（マルチテナント）

    # --- LDAP (AUTH_PROVIDER=ldap 時に使用) ---
    LDAP_SERVER: str = ""
    LDAP_BASE_DN: str = ""
    LDAP_BIND_DN_TEMPLATE: str = "uid={username},ou=users,{base_dn}"
    LDAP_USER_FILTER: str = "(uid={username})"
    LDAP_DEFAULT_ROLE: str = "employee"
    LDAP_ROLE_MAPPING: str = "{}"  # JSON: {"CN=Admins,DC=...": "admin"}

    # --- SAML 2.0 (AUTH_PROVIDER=saml 時に使用) ---
    SAML_IDP_METADATA_URL: str = ""
    SAML_SP_ENTITY_ID: str = "auth-service"
    SAML_ACS_URL: str = "http://localhost:8010/auth/saml/acs"

    # --- Proxy Auth (AUTH_PROVIDER=proxy 時に使用) ---
    PROXY_AUTH_HEADER: str = "X-Remote-User"
    PROXY_AUTH_SECRET: str = ""
    PROXY_AUTH_MAX_SKEW_SECONDS: int = 300
    PROXY_AUTH_REQUIRE_SIGNATURE: bool = True

    # --- セキュリティ設定 ---
    MAX_LOGIN_ATTEMPTS: int = 5
    LOCKOUT_DURATION_MINUTES: int = 15
    PASSWORD_HASH_ITERATIONS: int = 200000
    PASSWORD_RESET_TTL_MINUTES: int = 15
    SESSION_TTL_SECONDS: int = 604800  # 7日

    # --- CORS 設定 ---
    ALLOWED_ORIGINS: list[str] = ["*"]

    # --- 開発モード ---
    DEV_MODE: bool = False  # True の場合、パスワードリセットトークンをレスポンスに含める

    @field_validator("JWT_SECRET_KEY")
    @classmethod
    def validate_jwt_secret(cls, v: str) -> str:
        """JWT シークレットキーのバリデーション."""
        if not v:
            import secrets

            return secrets.token_hex(32)
        return v


def get_settings() -> Settings:
    """設定シングルトンを取得."""
    return _settings


_settings = Settings()
