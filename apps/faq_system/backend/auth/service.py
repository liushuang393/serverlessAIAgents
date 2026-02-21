"""FAQ システム認証サービス.

ローカル DB / LDAP / 企業 IdP を切り替え可能な認証基盤。
セッション・パスワード再設定トークン・プロフィールは DB で管理する。
"""

from __future__ import annotations

import asyncio
import hashlib
import json
import logging
import os
import secrets
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from enum import Enum
from typing import TYPE_CHECKING, Any

import httpx
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.db.models import AuthSession, PasswordResetToken, TokenBlacklist, UserAccount
from apps.faq_system.backend.db.session import ensure_database_ready, get_db_session
from sqlalchemy import delete, select, update
from sqlalchemy.exc import IntegrityError

from agentflow.security.auth_middleware import AuthMiddleware, AuthUser, JWTConfig
from agentflow.security.mfa import TimeBasedMFA
from agentflow.security.oauth2_provider import ExternalIdentity as OAuth2Identity


if TYPE_CHECKING:
    from agentflow.security.oauth2_provider import ExternalIdentity as OAuth2Identity


logger = logging.getLogger(__name__)


def _env_bool(name: str, default: str = "false") -> bool:
    value = os.getenv(name, default).lower()
    return value in {"1", "true", "yes", "on"}


def _build_jwt_config() -> JWTConfig:
    return JWTConfig(
        secret_key=os.getenv("JWT_SECRET_KEY", secrets.token_hex(32)),
        algorithm=os.getenv("FAQ_JWT_ALGORITHM", "HS256"),
        expire_minutes=int(os.getenv("JWT_EXPIRE_MINUTES", "60")),
        issuer=os.getenv("FAQ_JWT_ISSUER", "faq-system"),
        audience=os.getenv("FAQ_JWT_AUDIENCE", "faq-system"),
    )


class AuthProvider(str, Enum):
    """認証プロバイダー."""

    LOCAL_DB = "local_db"
    LDAP = "ldap"
    IDP = "idp"
    GOOGLE = "google"
    AZURE_AD = "azure_ad"


@dataclass(slots=True)
class ExternalIdentity:
    """外部認証で解決されたユーザー情報."""

    username: str
    display_name: str
    department: str
    position: str
    role: str
    email: str | None = None


# デモユーザー定義（ローカル DB 初期シード）
DEMO_USERS: dict[str, dict[str, Any]] = {
    "admin": {
        "initial_password": "admin123",
        "display_name": "管理者 太郎",
        "department": "情報システム部",
        "position": "部長",
        "role": "admin",
    },
    "tanaka": {
        "initial_password": "tanaka123",
        "display_name": "田中 一郎",
        "department": "人事部",
        "position": "課長",
        "role": "manager",
    },
    "suzuki": {
        "initial_password": "suzuki123",
        "display_name": "鈴木 花子",
        "department": "営業部",
        "position": "主任",
        "role": "employee",
    },
    "yamamoto": {
        "initial_password": "yamamoto123",
        "display_name": "山本 健太",
        "department": "DX推進部",
        "position": "データアナリスト",
        "role": "analyst",
    },
    "sato": {
        "initial_password": "sato123",
        "display_name": "佐藤 美咲",
        "department": "人事部",
        "position": "マネージャー",
        "role": "hr_admin",
    },
}


class AuthService:
    """FAQ システム認証サービス."""

    def __init__(self) -> None:
        """初期化."""
        self._jwt_config = _build_jwt_config()
        self._middleware = AuthMiddleware(jwt_config=self._jwt_config)
        self._password_hash_iterations = int(os.getenv("FAQ_PASSWORD_HASH_ITERATIONS", "200000"))
        self._password_reset_ttl_minutes = int(os.getenv("FAQ_PASSWORD_RESET_TTL_MINUTES", "15"))
        self._session_ttl_seconds = int(os.getenv("FAQ_SESSION_TTL_SECONDS", str(86400 * 7)))
        self._auth_dev_mode = _env_bool("FAQ_AUTH_DEV_MODE", "true")
        self._bootstrap_lock = asyncio.Lock()
        self._bootstrap_completed = False

    async def ensure_bootstrap_data(self) -> None:
        """DB と初期データを利用可能状態にする."""
        if self._bootstrap_completed:
            return
        async with self._bootstrap_lock:
            if self._bootstrap_completed:
                return

            await ensure_database_ready()
            if self._active_provider() == AuthProvider.LOCAL_DB:
                await self._seed_demo_users_if_needed()
            self._bootstrap_completed = True

    async def authenticate(
        self, username: str, password: str, totp_code: str | None = None
    ) -> tuple[bool, str, UserInfo | None]:
        """ユーザー認証."""
        if not username or not password:
            return False, "ユーザー名またはパスワードが不足しています", None

        await self.ensure_bootstrap_data()
        provider = self._active_provider()

        if provider == AuthProvider.LOCAL_DB:
            return await self._authenticate_local_db_with_mfa(username, password, totp_code)

        identity = await self._authenticate_external(provider, username, password)
        if identity is None:
            return False, "外部認証に失敗しました", None
        user = await self._upsert_external_user(identity, provider)
        return True, "ログイン成功", user

    async def _authenticate_local_db_with_mfa(
        self, username: str, password: str, totp_code: str | None
    ) -> tuple[bool, str, UserInfo | None]:
        async with get_db_session() as session:
            account = await session.scalar(
                select(UserAccount).where(
                    UserAccount.username == username,
                    UserAccount.is_active.is_(True),
                )
            )
            if account is None:
                return False, "ユーザー名またはパスワードが間違っています", None

            # ロックアウトチェック
            now = datetime.now(tz=UTC)
            if account.locked_until is not None:
                locked_until_aware = (
                    account.locked_until
                    if account.locked_until.tzinfo is not None
                    else account.locked_until.replace(tzinfo=UTC)
                )
                if locked_until_aware > now:
                    return (
                        False,
                        "アカウントがロックされています。15分後に再試行してください。",
                        None,
                    )

            if account.auth_source != AuthProvider.LOCAL_DB.value:
                return False, "このアカウントはローカル認証ではありません", None

            max_attempts = int(os.getenv("FAQ_AUTH_MAX_LOGIN_ATTEMPTS", "5"))
            lockout_minutes = int(os.getenv("FAQ_AUTH_LOCKOUT_MINUTES", "15"))

            if not self._verify_password(password, account.password_salt, account.password_hash):
                account.login_attempts += 1
                if account.login_attempts >= max_attempts:
                    account.locked_until = now + timedelta(minutes=lockout_minutes)
                    await session.commit()
                    return (
                        False,
                        f"ログイン試行回数が制限を超えました。アカウントを {lockout_minutes} 分間ロックしました。",
                        None,
                    )

                await session.commit()
                return False, "ユーザー名またはパスワードが間違っています", None

            if account.mfa_enabled:
                if not totp_code:
                    return False, "MFA_REQUIRED", None
                if not account.mfa_secret or not TimeBasedMFA.verify_totp(account.mfa_secret, totp_code):
                    return False, "MFAコードが無効です", None

            account.login_attempts = 0
            account.locked_until = None
            account.last_login_at = now
            await session.commit()
            return True, "ログイン成功", self._to_user_info(account)

    async def register_user(
        self,
        username: str,
        password: str,
        display_name: str,
        department: str | None = None,
        position: str | None = None,
        email: str | None = None,
    ) -> tuple[bool, str, UserInfo | None]:
        """新規ユーザー登録."""
        await self.ensure_bootstrap_data()

        if self._active_provider() != AuthProvider.LOCAL_DB:
            return False, "ユーザー登録はローカル認証モードでのみ利用可能です", None

        is_valid, reason = self._validate_new_password(password)
        if not is_valid:
            return False, reason, None

        async with get_db_session() as session:
            existing = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if existing:
                return False, "このユーザー名は既に使用されています", None

            now = datetime.now(tz=UTC)
            salt = self._generate_salt()
            new_user = UserAccount(
                id=self._build_user_id(username),
                username=username,
                password_hash=self._hash_password(password, salt),
                password_salt=salt,
                display_name=display_name,
                department=department or "",
                position=position or "",
                role="employee",  # デフォルトは一般ユーザー
                auth_source=AuthProvider.LOCAL_DB.value,
                is_active=True,
                created_at=now,
                updated_at=now,
                last_login_at=None,
            )
            session.add(new_user)
            user_info = self._to_user_info(new_user)

        return True, "ユーザー登録が完了しました", user_info

    async def login_external(self, oauth_identity: OAuth2Identity) -> UserInfo | None:
        """OAuth2 ID情報を使ってログイン/登録."""
        await self.ensure_bootstrap_data()

        role = "employee"
        department = ""
        position = ""

        if oauth_identity.provider == "azure_ad" and oauth_identity.raw_info:
            department = oauth_identity.raw_info.get("officeLocation") or ""
            position = oauth_identity.raw_info.get("jobTitle") or ""

        identity = ExternalIdentity(
            username=oauth_identity.username or oauth_identity.sub,
            display_name=oauth_identity.display_name or oauth_identity.username or "Unknown",
            email=oauth_identity.email,
            department=department,
            position=position,
            role=role,
        )

        return await self._upsert_external_user(identity, oauth_identity.provider)  # type: ignore

    def create_access_token(self, user: UserInfo) -> str:
        """アクセストークン生成."""
        jti = secrets.token_hex(16)
        return self._middleware.create_jwt_token(
            user_id=user.user_id,
            email=f"{user.username}@example.com",
            roles=[user.role],
            metadata={
                "username": user.username,
                "display_name": user.display_name,
                "department": user.department,
                "position": user.position,
                "jti": jti,
            },
        )

    def _decode_token_payload(self, authorization: str) -> dict[str, Any] | None:
        """Bearer トークンをデコードしてペイロードを返す (署名検証あり)."""
        if not self._jwt_config.secret_key:
            return None
        if not authorization.startswith("Bearer "):
            return None
        token = authorization[7:]
        try:
            import jwt as pyjwt

            return pyjwt.decode(
                token,
                self._jwt_config.secret_key,
                algorithms=[self._jwt_config.algorithm],
                audience=self._jwt_config.audience,
                issuer=self._jwt_config.issuer,
            )
        except Exception:
            return None

    async def blacklist_token(self, authorization: str | None, user_id: str) -> None:
        """JWTトークンのJTIをブラックリストに登録する."""
        if not authorization:
            return
        payload = self._decode_token_payload(authorization)
        if not payload:
            return
        jti: str | None = payload.get("metadata", {}).get("jti") or payload.get("jti")
        if not jti:
            return
        exp_raw = payload.get("exp")
        if exp_raw is None:
            return
        try:
            expires_at = datetime.fromtimestamp(int(exp_raw), tz=UTC)
        except (TypeError, ValueError):
            return

        await self.ensure_bootstrap_data()
        async with get_db_session() as session:
            try:
                session.add(
                    TokenBlacklist(
                        jti=jti,
                        user_id=user_id,
                        expires_at=expires_at,
                    )
                )
                await session.flush()
            except IntegrityError:
                # すでに blacklist 済み → 無視
                await session.rollback()

    async def is_token_blacklisted(self, jti: str) -> bool:
        """JTIがブラックリストに存在するか確認する."""
        await self.ensure_bootstrap_data()
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            result = await session.scalar(
                select(TokenBlacklist.id).where(
                    TokenBlacklist.jti == jti,
                    TokenBlacklist.expires_at > now,
                )
            )
            return result is not None

    async def cleanup_expired_blacklist_entries(self) -> None:
        """期限切れのブラックリストエントリを削除する."""
        await self.ensure_bootstrap_data()
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            await session.execute(delete(TokenBlacklist).where(TokenBlacklist.expires_at <= now))

    async def verify_token(self, authorization: str | None) -> AuthUser | None:
        """トークン検証 (ブラックリストチェック込み)."""
        if not authorization:
            return None
        payload = self._decode_token_payload(authorization)
        if payload is not None:
            jti: str | None = payload.get("metadata", {}).get("jti") or payload.get("jti")
            if jti and await self.is_token_blacklisted(jti):
                logger.warning("ブラックリスト済みトークンを拒否: jti=%s", jti)
                return None
        return await self._middleware.authenticate(authorization=authorization)

    def get_expire_seconds(self) -> int:
        """トークン有効期限 (秒)."""
        return self._jwt_config.expire_minutes * 60

    async def get_user(self, username: str) -> UserInfo | None:
        """ユーザー情報を取得."""
        await self.ensure_bootstrap_data()
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None or not account.is_active:
                return None
            return self._to_user_info(account)

    async def create_session(self, user: UserInfo) -> str:
        """セッショントークンを作成して DB に保存."""
        await self.ensure_bootstrap_data()

        session_token = secrets.token_urlsafe(32)
        token_hash = self._hash_token(session_token)
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            session.add(
                AuthSession(
                    id=f"sess-{secrets.token_hex(12)}",
                    user_id=user.user_id,
                    token_hash=token_hash,
                    created_at=now,
                    expires_at=now + timedelta(seconds=self._session_ttl_seconds),
                    revoked_at=None,
                )
            )
        return session_token

    async def get_user_by_session_token(self, session_token: str | None) -> UserInfo | None:
        """セッショントークンからユーザーを取得."""
        if not session_token:
            return None

        await self.ensure_bootstrap_data()
        token_hash = self._hash_token(session_token)
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            session_record = await session.scalar(
                select(AuthSession).where(
                    AuthSession.token_hash == token_hash,
                    AuthSession.revoked_at.is_(None),
                    AuthSession.expires_at > now,
                )
            )
            if session_record is None:
                return None

            account = await session.get(UserAccount, session_record.user_id)
            if account is None or not account.is_active:
                return None

            return self._to_user_info(account)

    async def revoke_sessions_for_user(self, user_id: str) -> None:
        """ユーザーの有効セッションを失効させる."""
        await self.ensure_bootstrap_data()
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            await session.execute(
                update(AuthSession)
                .where(AuthSession.user_id == user_id, AuthSession.revoked_at.is_(None))
                .values(revoked_at=now)
            )

    async def revoke_session_token(self, session_token: str | None) -> None:
        """単一セッションを失効."""
        if not session_token:
            return
        await self.ensure_bootstrap_data()
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            await session.execute(
                update(AuthSession)
                .where(
                    AuthSession.token_hash == self._hash_token(session_token),
                    AuthSession.revoked_at.is_(None),
                )
                .values(revoked_at=now)
            )

    async def change_password(
        self,
        username: str,
        current_password: str,
        new_password: str,
    ) -> tuple[bool, str]:
        """パスワードを変更."""
        await self.ensure_bootstrap_data()
        is_valid, reason = self._validate_new_password(new_password)
        if not is_valid:
            return False, reason

        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None:
                return False, "ユーザーが存在しません"
            if account.auth_source != AuthProvider.LOCAL_DB.value:
                return False, "このアカウントは企業認証管理のため変更できません"

            if not self._verify_password(
                current_password,
                account.password_salt,
                account.password_hash,
            ):
                return False, "現在のパスワードが正しくありません"

            salt = self._generate_salt()
            account.password_salt = salt
            account.password_hash = self._hash_password(new_password, salt)
            account.updated_at = datetime.now(tz=UTC)
            logger.info("パスワード変更: username=%s", username)

        return True, "パスワードを変更しました"

    async def create_password_reset_token(self, username: str) -> str | None:
        """パスワード再設定トークンを発行."""
        await self.ensure_bootstrap_data()
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            await session.execute(
                delete(PasswordResetToken).where(
                    PasswordResetToken.expires_at < now,
                )
            )
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None or account.auth_source != AuthProvider.LOCAL_DB.value:
                return None

            raw_token = secrets.token_urlsafe(32)
            session.add(
                PasswordResetToken(
                    id=f"pr-{secrets.token_hex(12)}",
                    user_id=account.id,
                    token_hash=self._hash_token(raw_token),
                    expires_at=now + timedelta(minutes=self._password_reset_ttl_minutes),
                    created_at=now,
                    used_at=None,
                )
            )
            logger.info("パスワード再設定トークン発行: username=%s", username)
            return raw_token

    async def reset_password(self, reset_token: str, new_password: str) -> tuple[bool, str]:
        """トークンを使ってパスワード再設定."""
        await self.ensure_bootstrap_data()
        is_valid, reason = self._validate_new_password(new_password)
        if not is_valid:
            return False, reason

        now = datetime.now(tz=UTC)
        token_hash = self._hash_token(reset_token)
        async with get_db_session() as session:
            token_record = await session.scalar(
                select(PasswordResetToken).where(
                    PasswordResetToken.token_hash == token_hash,
                    PasswordResetToken.used_at.is_(None),
                    PasswordResetToken.expires_at > now,
                )
            )
            if token_record is None:
                return False, "トークンが無効または期限切れです"

            account = await session.get(UserAccount, token_record.user_id)
            if account is None:
                return False, "ユーザーが存在しません"
            if account.auth_source != AuthProvider.LOCAL_DB.value:
                return False, "このアカウントは企業認証管理のため再設定できません"

            salt = self._generate_salt()
            account.password_salt = salt
            account.password_hash = self._hash_password(new_password, salt)
            account.updated_at = now
            token_record.used_at = now
            logger.info("パスワード再設定: username=%s", account.username)

        return True, "パスワードを再設定しました"

    async def update_profile(
        self,
        username: str,
        *,
        display_name: str | None = None,
        department: str | None = None,
        position: str | None = None,
    ) -> UserInfo | None:
        """プロフィール情報を更新."""
        await self.ensure_bootstrap_data()
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None:
                return None

            if display_name is not None:
                account.display_name = display_name
            if department is not None:
                account.department = department
            if position is not None:
                account.position = position
            account.updated_at = datetime.now(tz=UTC)
            logger.info("プロフィール更新: username=%s", username)
            return self._to_user_info(account)

    def expose_reset_token_in_response(self) -> bool:
        """再設定トークンをレスポンスに含めるか."""
        return self._auth_dev_mode

    async def reset_demo_data(self) -> None:
        """テスト向けにローカル DB 認証データを初期化."""
        await ensure_database_ready()
        async with get_db_session() as session:
            await session.execute(delete(AuthSession))
            await session.execute(delete(PasswordResetToken))
            await session.execute(delete(UserAccount))

        await self._seed_demo_users_if_needed(force=True)
        self._bootstrap_completed = True

    async def _seed_demo_users_if_needed(self, *, force: bool = False) -> None:
        async with get_db_session() as session:
            if not force:
                existing = await session.scalar(select(UserAccount.id).limit(1))
                if existing:
                    return

            if force:
                await session.execute(delete(UserAccount))

            now = datetime.now(tz=UTC)
            for username, meta in DEMO_USERS.items():
                salt = self._generate_salt()
                session.add(
                    UserAccount(
                        id=self._build_user_id(username),
                        username=username,
                        password_hash=self._hash_password(str(meta["initial_password"]), salt),
                        password_salt=salt,
                        display_name=str(meta["display_name"]),
                        department=str(meta["department"]),
                        position=str(meta["position"]),
                        role=str(meta["role"]),
                        auth_source=AuthProvider.LOCAL_DB.value,
                        is_active=True,
                        created_at=now,
                        updated_at=now,
                    )
                )

    def _active_provider(self) -> AuthProvider:
        raw = os.getenv("FAQ_AUTH_PROVIDER", AuthProvider.LOCAL_DB.value).strip().lower()
        try:
            return AuthProvider(raw)
        except ValueError:
            logger.warning("Unknown FAQ_AUTH_PROVIDER=%s. Fallback to local_db.", raw)
            return AuthProvider.LOCAL_DB

    async def _authenticate_local_db(self, username: str, password: str) -> UserInfo | None:
        async with get_db_session() as session:
            account = await session.scalar(
                select(UserAccount).where(
                    UserAccount.username == username,
                    UserAccount.is_active.is_(True),
                )
            )
            if account is None:
                return None
            if account.auth_source != AuthProvider.LOCAL_DB.value:
                return None
            if not self._verify_password(password, account.password_salt, account.password_hash):
                return None

            account.last_login_at = datetime.now(tz=UTC)
            return self._to_user_info(account)

    async def _authenticate_external(
        self,
        provider: AuthProvider,
        username: str,
        password: str,
    ) -> ExternalIdentity | None:
        if provider == AuthProvider.LDAP:
            return await self._authenticate_ldap(username, password)
        if provider == AuthProvider.IDP:
            return await self._authenticate_idp(username, password)
        return None

    async def _authenticate_ldap(self, username: str, password: str) -> ExternalIdentity | None:
        mock_users = self._load_mock_users("FAQ_LDAP_USERS_JSON")
        if mock_users:
            return self._authenticate_from_mock(mock_users, username, password)

        server_uri = os.getenv("FAQ_LDAP_SERVER_URI", "").strip()
        bind_dn_template = os.getenv("FAQ_LDAP_BIND_DN_TEMPLATE", "").strip()
        if not server_uri or not bind_dn_template:
            logger.warning("LDAP config missing. Set FAQ_LDAP_SERVER_URI and FAQ_LDAP_BIND_DN_TEMPLATE.")
            return None

        return await asyncio.to_thread(
            self._authenticate_ldap_sync,
            server_uri,
            bind_dn_template,
            username,
            password,
        )

    @staticmethod
    def _authenticate_ldap_sync(
        server_uri: str,
        bind_dn_template: str,
        username: str,
        password: str,
    ) -> ExternalIdentity | None:
        try:
            import ldap3  # type: ignore[import-not-found]
        except ImportError:
            logger.warning("ldap3 not installed. LDAP authentication is unavailable.")
            return None

        user_dn = bind_dn_template.format(username=username)
        server = ldap3.Server(server_uri, get_info=ldap3.ALL)
        conn = ldap3.Connection(server, user=user_dn, password=password, auto_bind=False)

        if not conn.bind():
            logger.info("LDAP Bind failed for %s", user_dn)
            return None

        base_dn = os.getenv("FAQ_LDAP_BASE_DN", "")
        if not base_dn:
            return ExternalIdentity(
                username=username,
                display_name=username,
                department="",
                position="",
                role=os.getenv("FAQ_LDAP_DEFAULT_ROLE", "employee"),
                email=f"{username}@example.com",
            )

        search_filter = os.getenv("FAQ_LDAP_USER_FILTER", "(uid={username})").format(username=username)
        conn.search(
            search_base=base_dn,
            search_filter=search_filter,
            attributes=["displayName", "cn", "mail", "department", "title", "memberOf"],
        )

        if not conn.entries:
            return ExternalIdentity(
                username=username,
                display_name=username,
                department="",
                position="",
                role=os.getenv("FAQ_LDAP_DEFAULT_ROLE", "employee"),
                email=f"{username}@example.com",
            )

        entry = conn.entries[0]
        display_name = str(entry.displayName or entry.cn or username)
        email = str(entry.mail or f"{username}@example.com")
        department = str(entry.department or "")
        position = str(entry.title or "")

        groups: list[str] = []
        if entry.memberOf:
            if isinstance(entry.memberOf, list):
                groups = [str(g) for g in entry.memberOf]
            else:
                groups = [str(entry.memberOf)]

        role = os.getenv("FAQ_LDAP_DEFAULT_ROLE", "employee")
        role_mapping_json = os.getenv("FAQ_LDAP_ROLE_MAPPING", "{}")
        try:
            import json

            role_mapping = json.loads(role_mapping_json)
            for group_dn, mapped_role in role_mapping.items():
                if group_dn in groups:
                    role = mapped_role
                    if role == "admin":  # Highest priority
                        break
        except Exception:
            logger.warning("Failed to parse FAQ_LDAP_ROLE_MAPPING or map roles.")

        return ExternalIdentity(
            username=username,
            display_name=display_name,
            department=department,
            position=position,
            role=role,
            email=email,
        )

    async def _authenticate_idp(self, username: str, password: str) -> ExternalIdentity | None:
        mock_users = self._load_mock_users("FAQ_IDP_USERS_JSON")
        if mock_users:
            return self._authenticate_from_mock(mock_users, username, password)

        token_url = os.getenv("FAQ_IDP_TOKEN_URL", "").strip()
        client_id = os.getenv("FAQ_IDP_CLIENT_ID", "").strip()
        client_secret = os.getenv("FAQ_IDP_CLIENT_SECRET", "").strip()
        if not token_url or not client_id:
            logger.warning("IdP config missing. Set FAQ_IDP_TOKEN_URL and FAQ_IDP_CLIENT_ID.")
            return None

        data: dict[str, str] = {
            "grant_type": "password",
            "username": username,
            "password": password,
            "client_id": client_id,
        }
        if client_secret:
            data["client_secret"] = client_secret
        scope = os.getenv("FAQ_IDP_SCOPE", "").strip()
        if scope:
            data["scope"] = scope

        async with httpx.AsyncClient(timeout=10.0) as client:
            token_resp = await client.post(token_url, data=data)
            if token_resp.status_code >= 400:
                return None

            token_json = token_resp.json()
            access_token = token_json.get("access_token")
            if not access_token:
                return None

            userinfo_url = os.getenv("FAQ_IDP_USERINFO_URL", "").strip()
            userinfo: dict[str, Any] = {}
            if userinfo_url:
                user_resp = await client.get(
                    userinfo_url,
                    headers={"Authorization": f"Bearer {access_token}"},
                )
                if user_resp.status_code < 400:
                    userinfo = user_resp.json()

        resolved_username = str(userinfo.get("preferred_username") or userinfo.get("sub") or username)
        groups = userinfo.get("groups")
        role = self._resolve_role_from_groups(groups)

        return ExternalIdentity(
            username=resolved_username,
            display_name=str(userinfo.get("name") or resolved_username),
            department=str(userinfo.get("department") or ""),
            position=str(userinfo.get("title") or ""),
            role=role,
            email=str(userinfo.get("email") or "") or None,
        )

    async def enable_mfa(self, username: str) -> tuple[str, str]:
        """MFA設定開始: シークレット生成."""
        secret = TimeBasedMFA.generate_secret()
        uri = TimeBasedMFA.get_provisioning_uri(secret, username, "AgentFlowFAQ")

        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account:
                account.mfa_secret = secret
                account.mfa_enabled = False
                await session.commit()

        return secret, uri

    async def verify_mfa_setup(self, username: str, code: str) -> bool:
        """MFA設定完了: コード検証して有効化."""
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if not account or not account.mfa_secret:
                return False

            if TimeBasedMFA.verify_totp(account.mfa_secret, code):
                account.mfa_enabled = True
                await session.commit()
                return True
            return False

    async def disable_mfa(self, username: str) -> bool:
        """MFA無効化."""
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account:
                account.mfa_enabled = False
                account.mfa_secret = None
                await session.commit()
                return True
            return False

    async def _upsert_external_user(
        self,
        identity: ExternalIdentity,
        provider: str | AuthProvider,
    ) -> UserInfo:
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == identity.username))

            if account is None:
                random_seed = secrets.token_hex(16)
                salt = self._generate_salt()
                account = UserAccount(
                    id=self._build_user_id(identity.username),
                    username=identity.username,
                    password_hash=self._hash_password(random_seed, salt),
                    password_salt=salt,
                    display_name=identity.display_name,
                    department=identity.department,
                    position=identity.position,
                    role=identity.role,
                    auth_source=provider.value,
                    is_active=True,
                    created_at=now,
                    updated_at=now,
                    last_login_at=now,
                )
                session.add(account)
            else:
                account.display_name = identity.display_name
                account.department = identity.department
                account.position = identity.position
                account.role = identity.role
                account.auth_source = provider.value
                account.is_active = True
                account.last_login_at = now
                account.updated_at = now

            return self._to_user_info(account)

    @staticmethod
    def _load_mock_users(env_name: str) -> dict[str, Any]:
        raw = os.getenv(env_name, "").strip()
        if not raw:
            return {}
        try:
            parsed = json.loads(raw)
            if isinstance(parsed, dict):
                return parsed
        except json.JSONDecodeError:
            logger.warning("Failed to parse %s as JSON.", env_name)
        return {}

    def _authenticate_from_mock(
        self,
        mock_users: dict[str, Any],
        username: str,
        password: str,
    ) -> ExternalIdentity | None:
        meta = mock_users.get(username)
        if not isinstance(meta, dict):
            return None
        expected_password = str(meta.get("password", ""))
        if not expected_password or expected_password != password:
            return None
        return ExternalIdentity(
            username=username,
            display_name=str(meta.get("display_name", username)),
            department=str(meta.get("department", "")),
            position=str(meta.get("position", "")),
            role=str(meta.get("role", "employee")),
            email=str(meta.get("email", "")) or None,
        )

    @staticmethod
    def _resolve_role_from_groups(groups: Any) -> str:
        if isinstance(groups, list):
            normalized = [str(item).lower() for item in groups]
            if "admin" in normalized:
                return "admin"
            if "manager" in normalized:
                return "manager"
            if normalized:
                return normalized[0]
        return os.getenv("FAQ_IDP_DEFAULT_ROLE", "employee")

    @staticmethod
    def _generate_salt() -> str:
        return secrets.token_hex(16)

    def _hash_password(self, password: str, salt: str) -> str:
        return hashlib.pbkdf2_hmac(
            "sha256",
            password.encode("utf-8"),
            salt.encode("utf-8"),
            self._password_hash_iterations,
        ).hex()

    def _verify_password(self, password: str, salt: str, expected_hash: str) -> bool:
        input_hash = self._hash_password(password, salt)
        return secrets.compare_digest(input_hash, expected_hash)

    @staticmethod
    def _hash_token(raw_token: str) -> str:
        return hashlib.sha256(raw_token.encode("utf-8")).hexdigest()

    @staticmethod
    def _validate_new_password(password: str) -> tuple[bool, str]:
        if len(password) < 8:
            return False, "新しいパスワードは8文字以上にしてください"
        if password.isalpha() or password.isdigit():
            return False, "新しいパスワードは英字と数字を含めてください"
        return True, "OK"

    @staticmethod
    def _build_user_id(username: str) -> str:
        candidate = f"user-{username}"
        if len(candidate) <= 64:
            return candidate
        digest = hashlib.sha256(username.encode("utf-8")).hexdigest()[:59]
        return f"user-{digest}"

    @staticmethod
    def _to_user_info(account: UserAccount) -> UserInfo:
        return UserInfo(
            user_id=account.id,
            username=account.username,
            display_name=account.display_name,
            department=account.department,
            position=account.position,
            role=account.role,
        )


_auth_service_singleton: AuthService | None = None


def get_auth_service() -> AuthService:
    """AuthService シングルトンを取得."""
    global _auth_service_singleton
    if _auth_service_singleton is None:
        _auth_service_singleton = AuthService()
    return _auth_service_singleton


def reset_auth_service_singleton() -> None:
    """テスト向けに AuthService シングルトンをリセット."""
    global _auth_service_singleton
    _auth_service_singleton = None
