"""auth_service コアサービスクラス.

認証ロジックの中心。プロバイダー・JWT・セッション管理を統合する。
"""

from __future__ import annotations

import asyncio
import logging
import secrets
from datetime import UTC, datetime, timedelta
from typing import Any

from apps.auth_service.api.schemas import UserInfo
from apps.auth_service.config import Settings, get_settings
from apps.auth_service.core.jwt import JWTManager, TokenPair
from apps.auth_service.core.password import PasswordManager
from apps.auth_service.db.session import ensure_database_ready, get_db_session
from apps.auth_service.models.token import PasswordResetToken, RefreshToken, TokenBlacklist
from apps.auth_service.models.user import AuthSession, UserAccount
from sqlalchemy import delete, select, update


logger = logging.getLogger(__name__)


class AuthService:
    """統合認証サービス.

    プロバイダー経由の認証、JWT 発行、セッション管理、
    パスワードリセット、MFA 管理を担う。
    """

    def __init__(self, settings: Settings | None = None) -> None:
        """初期化."""
        self._settings = settings or get_settings()
        self._jwt = JWTManager(
            secret_key=self._settings.JWT_SECRET_KEY,
            algorithm=self._settings.JWT_ALGORITHM,
            access_expire_minutes=self._settings.JWT_ACCESS_EXPIRE_MINUTES,
            refresh_expire_days=self._settings.JWT_REFRESH_EXPIRE_DAYS,
            issuer=self._settings.JWT_ISSUER,
            audience=self._settings.JWT_AUDIENCE,
        )
        self._pwd = PasswordManager(iterations=self._settings.PASSWORD_HASH_ITERATIONS)
        self._bootstrap_lock = asyncio.Lock()
        self._bootstrap_completed = False

    async def ensure_ready(self) -> None:
        """DB 初期化を保証する."""
        if self._bootstrap_completed:
            return
        async with self._bootstrap_lock:
            if self._bootstrap_completed:
                return
            await ensure_database_ready()
            self._bootstrap_completed = True

    # ------------------------------------------------------------------
    # 認証メイン
    # ------------------------------------------------------------------

    async def login(
        self,
        username: str,
        password: str,
        totp_code: str | None = None,
    ) -> tuple[bool, str, UserInfo | None, TokenPair | None]:
        """ユーザーログイン.

        Args:
            username: ユーザー名
            password: パスワード
            totp_code: TOTP コード（MFA 有効時）

        Returns:
            (success, message, user_info, token_pair) タプル
        """
        await self.ensure_ready()

        provider_name = self._settings.AUTH_PROVIDER
        if provider_name == "local_db":
            from apps.auth_service.providers.local import LocalDBProvider

            provider = LocalDBProvider(settings=self._settings)
            result = await provider.authenticate_with_mfa(username, password, totp_code)
        else:
            from apps.auth_service.providers import get_provider

            provider = get_provider(provider_name)
            result = await provider.authenticate(username, password)

        if not result.success or result.identity is None:
            return False, result.message, None, None

        # 外部認証の場合は DB にユーザーを upsert
        if provider_name != "local_db":
            user_info = await self._upsert_external_user(result.identity, provider_name)
        else:
            user_info = await self._get_user_info_by_username(result.identity.username)
            if user_info is None:
                return False, "ユーザー情報の取得に失敗しました", None, None

        token_pair = await self._issue_token_pair(user_info)
        return True, "ログイン成功", user_info, token_pair

    async def login_with_external_identity(
        self,
        identity: Any,
        provider_name: str,
    ) -> tuple[UserInfo, TokenPair]:
        """外部認証アイデンティティでログイン（OAuth2 コールバック用）.

        Args:
            identity: ExternalIdentity インスタンス
            provider_name: プロバイダー名（google / azure_ad 等）

        Returns:
            (user_info, token_pair) タプル
        """
        await self.ensure_ready()
        user_info = await self._upsert_external_user(identity, provider_name)
        token_pair = await self._issue_token_pair(user_info)
        return user_info, token_pair

    async def register(
        self,
        username: str,
        password: str,
        display_name: str,
        department: str = "",
        position: str = "",
        email: str | None = None,
    ) -> tuple[bool, str, UserInfo | None, TokenPair | None]:
        """ユーザー登録（local_db のみ）.

        Returns:
            (success, message, user_info, token_pair) タプル
        """
        await self.ensure_ready()

        if self._settings.AUTH_PROVIDER != "local_db":
            return False, "ユーザー登録はローカル認証モードでのみ利用できます", None, None

        from apps.auth_service.providers.local import LocalDBProvider

        provider = LocalDBProvider(settings=self._settings)
        result = await provider.register_user(
            username=username,
            password=password,
            display_name=display_name,
            department=department,
            position=position,
            email=email,
        )

        if not result.success or result.identity is None:
            return False, result.message, None, None

        user_info = await self._get_user_info_by_username(username)
        if user_info is None:
            return False, "ユーザー情報の取得に失敗しました", None, None

        token_pair = await self._issue_token_pair(user_info)
        return True, result.message, user_info, token_pair

    # ------------------------------------------------------------------
    # トークン操作
    # ------------------------------------------------------------------

    async def refresh_access_token(self, refresh_token: str) -> tuple[bool, str, TokenPair | None]:
        """リフレッシュトークンを使って新しいトークンペアを発行.

        Args:
            refresh_token: リフレッシュトークン

        Returns:
            (success, message, token_pair) タプル
        """
        await self.ensure_ready()

        claims = self._jwt.decode_refresh_token(refresh_token)
        if claims is None:
            return False, "リフレッシュトークンが無効または期限切れです", None

        user_id = str(claims.get("sub", ""))
        family = str(claims.get("family", ""))
        str(claims.get("jti", ""))

        token_hash = self._pwd.hash_token(refresh_token)
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            stored = await session.scalar(
                select(RefreshToken).where(
                    RefreshToken.token_hash == token_hash,
                    RefreshToken.revoked_at.is_(None),
                    RefreshToken.expires_at > now,
                )
            )
            if stored is None:
                # トークン再利用攻撃の可能性：同一ファミリー全失効
                await session.execute(
                    update(RefreshToken)
                    .where(RefreshToken.family == family, RefreshToken.revoked_at.is_(None))
                    .values(revoked_at=now)
                )
                await session.commit()
                return False, "リフレッシュトークンが無効です（セキュリティアラート）", None

            # 使用済みトークンを失効
            stored.revoked_at = now
            await session.commit()

        # ユーザー情報取得
        user_info = await self._get_user_info_by_id(user_id)
        if user_info is None:
            return False, "ユーザーが存在しません", None

        # 新しいトークンペアを同一ファミリーで発行
        token_pair = await self._issue_token_pair(user_info, family=family)
        return True, "トークンを更新しました", token_pair

    async def verify_access_token(self, authorization: str | None) -> UserInfo | None:
        """アクセストークンを検証してユーザー情報を返す.

        Args:
            authorization: "Bearer <token>" 形式の Authorization ヘッダー値

        Returns:
            UserInfo または None
        """
        if not authorization or not authorization.startswith("Bearer "):
            return None

        token = authorization[7:]
        claims = self._jwt.decode_access_token(token)
        if claims is None:
            return None

        # ブラックリストチェック
        if claims.jti:
            now = datetime.now(tz=UTC)
            async with get_db_session() as session:
                blacklisted = await session.scalar(
                    select(TokenBlacklist).where(
                        TokenBlacklist.jti == claims.jti,
                        TokenBlacklist.expires_at > now,
                    )
                )
                if blacklisted is not None:
                    return None

        return UserInfo(
            user_id=claims.sub,
            username=claims.username,
            display_name=claims.display_name,
            department=claims.department,
            position=claims.position,
            role=claims.role,
            email=claims.email,
        )

    async def logout(
        self,
        user_id: str,
        access_token: str | None = None,
        session_token: str | None = None,
    ) -> None:
        """ログアウト処理.

        アクセストークンをブラックリストに追加し、セッションを失効させる。

        Args:
            user_id: ユーザーID
            access_token: アクセストークン（ブラックリスト追加用）
            session_token: セッショントークン（Cookie 用）
        """
        await self.ensure_ready()
        now = datetime.now(tz=UTC)

        if access_token:
            claims = self._jwt.decode_access_token(access_token)
            if claims and claims.jti:
                async with get_db_session() as session:
                    session.add(
                        TokenBlacklist(
                            id=f"bl-{secrets.token_hex(12)}",
                            jti=claims.jti,
                            expires_at=claims.exp,
                            revoked_at=now,
                        )
                    )
                    await session.commit()

        if session_token:
            await self._revoke_session_token(session_token)

        # 全リフレッシュトークンを失効
        async with get_db_session() as session:
            await session.execute(
                update(RefreshToken)
                .where(RefreshToken.user_id == user_id, RefreshToken.revoked_at.is_(None))
                .values(revoked_at=now)
            )
            await session.execute(
                update(AuthSession)
                .where(AuthSession.user_id == user_id, AuthSession.revoked_at.is_(None))
                .values(revoked_at=now)
            )
            await session.commit()

    # ------------------------------------------------------------------
    # セッション管理（Cookie 用）
    # ------------------------------------------------------------------

    async def create_session(self, user: UserInfo) -> str:
        """セッショントークンを DB に保存して返す.

        Args:
            user: ユーザー情報

        Returns:
            セッショントークン（生値）
        """
        await self.ensure_ready()
        raw_token = secrets.token_urlsafe(32)
        token_hash = self._pwd.hash_token(raw_token)
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            session.add(
                AuthSession(
                    id=f"sess-{secrets.token_hex(12)}",
                    user_id=user.user_id,
                    token_hash=token_hash,
                    created_at=now,
                    expires_at=now + timedelta(seconds=self._settings.SESSION_TTL_SECONDS),
                )
            )
            await session.commit()
        return raw_token

    async def get_user_by_session(self, session_token: str | None) -> UserInfo | None:
        """セッショントークンからユーザーを取得.

        Args:
            session_token: セッショントークン（生値）

        Returns:
            UserInfo または None
        """
        if not session_token:
            return None
        await self.ensure_ready()
        token_hash = self._pwd.hash_token(session_token)
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            record = await session.scalar(
                select(AuthSession).where(
                    AuthSession.token_hash == token_hash,
                    AuthSession.revoked_at.is_(None),
                    AuthSession.expires_at > now,
                )
            )
            if record is None:
                return None
            account = await session.get(UserAccount, record.user_id)
            if account is None or not account.is_active:
                return None
            return self._account_to_user_info(account)

    # ------------------------------------------------------------------
    # パスワード操作
    # ------------------------------------------------------------------

    async def change_password(self, username: str, current_password: str, new_password: str) -> tuple[bool, str]:
        """パスワードを変更（local_db のみ）."""
        await self.ensure_ready()
        if self._settings.AUTH_PROVIDER != "local_db":
            return False, "パスワード変更はローカル認証モードでのみ利用できます"

        from apps.auth_service.providers.local import LocalDBProvider

        provider = LocalDBProvider(settings=self._settings)
        return await provider.change_password(username, current_password, new_password)

    async def create_password_reset_token(self, username: str) -> str | None:
        """パスワード再設定トークンを発行.

        Args:
            username: ユーザー名

        Returns:
            再設定トークン（生値）または None
        """
        await self.ensure_ready()
        now = datetime.now(tz=UTC)

        async with get_db_session() as session:
            # 期限切れトークンを削除
            await session.execute(delete(PasswordResetToken).where(PasswordResetToken.expires_at < now))
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None or account.auth_source != "local_db":
                return None

            raw_token = secrets.token_urlsafe(32)
            session.add(
                PasswordResetToken(
                    id=f"pr-{secrets.token_hex(12)}",
                    user_id=account.id,
                    token_hash=self._pwd.hash_token(raw_token),
                    expires_at=now + timedelta(minutes=self._settings.PASSWORD_RESET_TTL_MINUTES),
                    created_at=now,
                )
            )
            await session.commit()
        return raw_token

    async def reset_password(self, reset_token: str, new_password: str) -> tuple[bool, str]:
        """トークンを使ってパスワードを再設定.

        Args:
            reset_token: 再設定トークン（生値）
            new_password: 新しいパスワード

        Returns:
            (success, message) タプル
        """
        await self.ensure_ready()
        is_valid, reason = self._pwd.validate_password_strength(new_password)
        if not is_valid:
            return False, reason

        now = datetime.now(tz=UTC)
        token_hash = self._pwd.hash_token(reset_token)

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
            if account.auth_source != "local_db":
                return False, "このアカウントはパスワードリセットに対応していません"

            salt = self._pwd.generate_salt()
            account.password_salt = salt
            account.password_hash = self._pwd.hash_password(new_password, salt)
            account.updated_at = now
            token_record.used_at = now
            await session.commit()

        return True, "パスワードを再設定しました"

    # ------------------------------------------------------------------
    # プロフィール
    # ------------------------------------------------------------------

    async def update_profile(
        self,
        username: str,
        *,
        display_name: str | None = None,
        department: str | None = None,
        position: str | None = None,
    ) -> UserInfo | None:
        """プロフィールを更新.

        Args:
            username: ユーザー名
            display_name: 新しい表示名
            department: 新しい部署
            position: 新しい役職

        Returns:
            更新後の UserInfo または None
        """
        await self.ensure_ready()
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
            await session.commit()
            return self._account_to_user_info(account)

    # ------------------------------------------------------------------
    # MFA
    # ------------------------------------------------------------------

    async def setup_mfa(self, username: str) -> tuple[str, str]:
        """MFA 設定を開始.

        Args:
            username: ユーザー名

        Returns:
            (secret, provisioning_uri) タプル
        """
        await self.ensure_ready()
        from apps.auth_service.providers.local import LocalDBProvider

        provider = LocalDBProvider(settings=self._settings)
        return await provider.enable_mfa(username)

    async def verify_mfa_setup(self, username: str, code: str) -> bool:
        """MFA コードを検証して有効化."""
        await self.ensure_ready()
        from apps.auth_service.providers.local import LocalDBProvider

        provider = LocalDBProvider(settings=self._settings)
        return await provider.verify_mfa_setup(username, code)

    async def disable_mfa(self, username: str) -> bool:
        """MFA を無効化."""
        await self.ensure_ready()
        from apps.auth_service.providers.local import LocalDBProvider

        provider = LocalDBProvider(settings=self._settings)
        return await provider.disable_mfa(username)

    # ------------------------------------------------------------------
    # ユーティリティ
    # ------------------------------------------------------------------

    def get_jwks(self) -> dict[str, Any]:
        """JWKS 情報を返す."""
        return self._jwt.get_jwks()

    @property
    def access_expire_seconds(self) -> int:
        """アクセストークン有効期限（秒）."""
        return self._jwt.access_expire_seconds

    # ------------------------------------------------------------------
    # プライベートヘルパー
    # ------------------------------------------------------------------

    async def _issue_token_pair(self, user: UserInfo, family: str | None = None) -> TokenPair:
        """トークンペアを発行して DB に保存."""
        access_token = self._jwt.create_access_token(
            user_id=user.user_id,
            username=user.username,
            display_name=user.display_name,
            department=user.department,
            position=user.position,
            role=user.role,
            email=user.email,
        )
        refresh_token_str, family_id = self._jwt.create_refresh_token(user_id=user.user_id, family=family)
        now = datetime.now(tz=UTC)
        token_hash = self._pwd.hash_token(refresh_token_str)

        async with get_db_session() as session:
            session.add(
                RefreshToken(
                    id=f"rt-{secrets.token_hex(12)}",
                    user_id=user.user_id,
                    token_hash=token_hash,
                    created_at=now,
                    expires_at=now + timedelta(days=self._settings.JWT_REFRESH_EXPIRE_DAYS),
                    family=family_id,
                )
            )
            await session.commit()

        return TokenPair(
            access_token=access_token,
            refresh_token=refresh_token_str,
            expires_in=self.access_expire_seconds,
        )

    async def _get_user_info_by_username(self, username: str) -> UserInfo | None:
        """ユーザー名で UserInfo を取得."""
        async with get_db_session() as session:
            account = await session.scalar(
                select(UserAccount).where(
                    UserAccount.username == username,
                    UserAccount.is_active.is_(True),
                )
            )
            if account is None:
                return None
            return self._account_to_user_info(account)

    async def _get_user_info_by_id(self, user_id: str) -> UserInfo | None:
        """ユーザーIDで UserInfo を取得."""
        async with get_db_session() as session:
            account = await session.get(UserAccount, user_id)
            if account is None or not account.is_active:
                return None
            return self._account_to_user_info(account)

    async def _upsert_external_user(self, identity: Any, provider_name: str) -> UserInfo:
        """外部認証ユーザーを DB に upsert."""
        now = datetime.now(tz=UTC)
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == identity.username))
            if account is None:
                rand_seed = secrets.token_hex(16)
                salt = self._pwd.generate_salt()
                account = UserAccount(
                    id=self._pwd.build_user_id(identity.username),
                    username=identity.username,
                    email=identity.email,
                    password_hash=self._pwd.hash_password(rand_seed, salt),
                    password_salt=salt,
                    display_name=identity.display_name,
                    department=identity.department,
                    position=identity.position,
                    role=identity.role,
                    auth_source=provider_name,
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
                account.email = identity.email
                account.auth_source = provider_name
                account.is_active = True
                account.last_login_at = now
                account.updated_at = now
            await session.commit()
            return self._account_to_user_info(account)

    async def _revoke_session_token(self, session_token: str) -> None:
        """セッショントークンを失効."""
        now = datetime.now(tz=UTC)
        token_hash = self._pwd.hash_token(session_token)
        async with get_db_session() as session:
            await session.execute(
                update(AuthSession)
                .where(
                    AuthSession.token_hash == token_hash,
                    AuthSession.revoked_at.is_(None),
                )
                .values(revoked_at=now)
            )
            await session.commit()

    @staticmethod
    def _account_to_user_info(account: UserAccount) -> UserInfo:
        """UserAccount を UserInfo に変換."""
        return UserInfo(
            user_id=account.id,
            username=account.username,
            display_name=account.display_name,
            department=account.department,
            position=account.position,
            role=account.role,
            email=account.email,
            mfa_enabled=account.mfa_enabled,
        )


# ---------------------------------------------------------------------------
# シングルトン
# ---------------------------------------------------------------------------

_service_singleton: AuthService | None = None


def get_auth_service() -> AuthService:
    """AuthService シングルトンを取得."""
    global _service_singleton
    if _service_singleton is None:
        _service_singleton = AuthService()
    return _service_singleton


def reset_auth_service() -> None:
    """テスト用にシングルトンをリセット."""
    global _service_singleton
    _service_singleton = None
