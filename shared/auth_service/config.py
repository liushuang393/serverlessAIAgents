"""auth_service 設定モジュール.

pydantic-settings を利用した設定管理。
全設定は環境変数または .env ファイルから読み込む。
"""

from __future__ import annotations

import json
import os
from pathlib import Path

from pydantic import field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


def _resolve_app_runtime_lazy(*args: object, **kwargs: object) -> object:
    """遅延インポート: kernel.runtime.resolve_app_runtime（L2→L3 違反回避）."""
    from kernel.runtime import resolve_app_runtime as _resolve

    return _resolve(*args, **kwargs)


_AUTH_ENV_FILE = Path(__file__).resolve().parent / ".env"
_AUTH_APP_CONFIG = Path(__file__).resolve().parent / "app_config.json"
_AUTH_MANIFEST = json.loads(_AUTH_APP_CONFIG.read_text(encoding="utf-8"))
_AUTH_RUNTIME = _resolve_app_runtime_lazy(
    _AUTH_APP_CONFIG,
    env=os.environ,
    backend_host_env="AUTH_SERVICE_HOST",
    backend_port_env="AUTH_SERVICE_PORT",
)
_AUTH_BACKEND_URL = _AUTH_RUNTIME.urls.backend or f"http://localhost:{_AUTH_RUNTIME.ports.api or 8010}"
_AUTH_DATABASE_URL = (
    _AUTH_MANIFEST.get("runtime", {}).get("database", {}).get("url")
    or "postgresql+asyncpg://postgres:postgres@localhost:5438/auth_service"
)


class Settings(BaseSettings):
    """auth_service 全設定."""

    model_config = SettingsConfigDict(
        env_file=str(_AUTH_ENV_FILE),
        env_file_encoding="utf-8",
        extra="ignore",
    )

    # --- コア設定 ---
    AUTH_SERVICE_PORT: int = _AUTH_RUNTIME.ports.api or 8010
    AUTH_SERVICE_HOST: str = _AUTH_RUNTIME.hosts.backend or "0.0.0.0"
    AUTH_DATABASE_URL: str = _AUTH_DATABASE_URL
    AUTH_DB_AUTO_CREATE: bool = True
    AUTH_DB_ECHO: bool = False
    AUTH_DB_SEED_DEFAULTS: bool = True  # デフォルトユーザーを自動作成

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
    GOOGLE_REDIRECT_URI: str = f"{_AUTH_BACKEND_URL}/auth/oauth2/google/callback"

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
    SAML_ACS_URL: str = f"{_AUTH_BACKEND_URL}/auth/saml/acs"

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

    # --- Tenant SSO ポリシー ---
    DEFAULT_TENANT_ID: str = "default"
    DEFAULT_CLIENT_APP: str = "unknown_app"
    DEFAULT_REQUESTED_SCOPES: str = "api,faq.access"
    ALLOW_SAME_TENANT_SSO: bool = True

    # --- 認可（Authorization）設定 ---
    AUTHZ_CACHE_TTL_SECONDS: int = 300  # パーミッションキャッシュ TTL（秒）
    AUTHZ_EMBED_PERMISSIONS_IN_JWT: bool = True  # JWT にパーミッションを埋め込む
    AUTHZ_MAX_JWT_PERMISSIONS: int = 50  # JWT に埋め込むパーミッション最大数
    AUTHZ_DEFAULT_OPEN: bool = True  # リソースマッピング未設定時フルアクセス許可

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
