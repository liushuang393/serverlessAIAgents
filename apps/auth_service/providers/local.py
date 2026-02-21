"""ローカル DB 認証プロバイダー.

PBKDF2 ハッシュによるパスワード認証と MFA（TOTP）をサポートする。
完全機能実装。
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING

from apps.auth_service.core.mfa import MFAManager
from apps.auth_service.core.password import PasswordManager
from apps.auth_service.db.session import get_db_session
from apps.auth_service.models.user import UserAccount
from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity
from sqlalchemy import select


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)


class LocalDBProvider(AuthProvider):
    """ローカル DB 認証プロバイダー.

    UserAccount テーブルに対してパスワード認証を行う。
    ロックアウト・MFA をサポートする。
    """

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)
        self._pwd = PasswordManager(iterations=settings.PASSWORD_HASH_ITERATIONS)
        self._mfa = MFAManager(issuer_name="AuthService")

    @property
    def provider_name(self) -> str:
        return "local_db"

    def supports_registration(self) -> bool:
        return True

    def supports_password_change(self) -> bool:
        return True

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """ローカル DB でパスワード認証.

        MFA を使いたい場合は authenticate_with_mfa を使用すること。

        Args:
            username: ユーザー名
            password: パスワード

        Returns:
            AuthResult
        """
        return await self.authenticate_with_mfa(username, password, totp_code=None)

    async def authenticate_with_mfa(
        self,
        username: str,
        password: str,
        totp_code: str | None,
    ) -> AuthResult:
        """MFA 対応のローカル DB 認証.

        Args:
            username: ユーザー名
            password: パスワード
            totp_code: TOTP コード（MFA 有効時は必須）

        Returns:
            AuthResult
        """
        async with get_db_session() as session:
            account = await session.scalar(
                select(UserAccount).where(
                    UserAccount.username == username,
                    UserAccount.is_active.is_(True),
                )
            )
            if account is None:
                return AuthResult(success=False, message="ユーザー名またはパスワードが間違っています")

            # ロックアウトチェック
            now = datetime.now(tz=UTC)
            if account.locked_until and account.locked_until > now:
                remaining = int((account.locked_until - now).total_seconds() / 60)
                return AuthResult(
                    success=False,
                    message=f"アカウントがロックされています。あと {max(1, remaining)} 分後に再試行してください。",
                )

            if account.auth_source != "local_db":
                return AuthResult(
                    success=False,
                    message="このアカウントはローカル認証を使用していません",
                )

            max_attempts = self._settings.MAX_LOGIN_ATTEMPTS
            lockout_minutes = self._settings.LOCKOUT_DURATION_MINUTES

            # パスワード検証
            if not self._pwd.verify_password(password, account.password_salt, account.password_hash):
                account.login_attempts += 1
                if account.login_attempts >= max_attempts:
                    account.locked_until = now + timedelta(minutes=lockout_minutes)
                    await session.commit()
                    return AuthResult(
                        success=False,
                        message=f"ログイン試行回数が上限に達しました。{lockout_minutes} 分間ロックします。",
                    )
                await session.commit()
                return AuthResult(success=False, message="ユーザー名またはパスワードが間違っています")

            # MFA チェック
            if account.mfa_enabled:
                if not totp_code:
                    return AuthResult(success=False, message="MFA_REQUIRED")
                if not account.mfa_secret or not self._mfa.verify_totp(account.mfa_secret, totp_code):
                    return AuthResult(success=False, message="MFA コードが無効です")

            # ログイン成功
            account.login_attempts = 0
            account.locked_until = None
            account.last_login_at = now
            await session.commit()

            identity = ExternalIdentity(
                username=account.username,
                display_name=account.display_name,
                role=account.role,
                department=account.department,
                position=account.position,
                email=account.email,
            )
            return AuthResult(success=True, message="ログイン成功", identity=identity)

    async def register_user(
        self,
        username: str,
        password: str,
        display_name: str,
        department: str = "",
        position: str = "",
        email: str | None = None,
    ) -> AuthResult:
        """新規ユーザーを登録.

        Args:
            username: ユーザー名（英数字、_、- のみ）
            password: パスワード（8文字以上、英字+数字）
            display_name: 表示名
            department: 部署
            position: 役職
            email: メールアドレス

        Returns:
            AuthResult
        """
        is_valid, reason = self._pwd.validate_password_strength(password)
        if not is_valid:
            return AuthResult(success=False, message=reason)

        async with get_db_session() as session:
            existing = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if existing is not None:
                return AuthResult(success=False, message="このユーザー名は既に使用されています")

            now = datetime.now(tz=UTC)
            salt = self._pwd.generate_salt()
            new_user = UserAccount(
                id=self._pwd.build_user_id(username),
                username=username,
                email=email,
                password_hash=self._pwd.hash_password(password, salt),
                password_salt=salt,
                display_name=display_name,
                department=department,
                position=position,
                role="employee",
                auth_source="local_db",
                is_active=True,
                created_at=now,
                updated_at=now,
            )
            session.add(new_user)
            await session.commit()

            identity = ExternalIdentity(
                username=username,
                display_name=display_name,
                role="employee",
                department=department,
                position=position,
                email=email,
            )
            return AuthResult(success=True, message="ユーザー登録完了", identity=identity)

    async def change_password(self, username: str, current_password: str, new_password: str) -> tuple[bool, str]:
        """パスワードを変更.

        Args:
            username: ユーザー名
            current_password: 現在のパスワード
            new_password: 新しいパスワード

        Returns:
            (success, message) タプル
        """
        is_valid, reason = self._pwd.validate_password_strength(new_password)
        if not is_valid:
            return False, reason

        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None:
                return False, "ユーザーが存在しません"
            if account.auth_source != "local_db":
                return False, "このアカウントはローカル認証ではないためパスワード変更できません"
            if not self._pwd.verify_password(current_password, account.password_salt, account.password_hash):
                return False, "現在のパスワードが正しくありません"

            salt = self._pwd.generate_salt()
            account.password_salt = salt
            account.password_hash = self._pwd.hash_password(new_password, salt)
            account.updated_at = datetime.now(tz=UTC)
            await session.commit()

        return True, "パスワードを変更しました"

    async def enable_mfa(self, username: str) -> tuple[str, str]:
        """MFA 設定を開始.

        シークレットを生成して DB に一時保存する。
        verify_mfa_setup() でコードを確認するまで mfa_enabled は False のまま。

        Args:
            username: ユーザー名

        Returns:
            (secret, provisioning_uri) タプル
        """
        secret = self._mfa.generate_secret()
        uri = self._mfa.get_provisioning_uri(secret, username)

        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is not None:
                account.mfa_secret = secret
                account.mfa_enabled = False
                await session.commit()

        return secret, uri

    async def verify_mfa_setup(self, username: str, code: str) -> bool:
        """MFA 設定を完了（コード検証して有効化）.

        Args:
            username: ユーザー名
            code: TOTP コード

        Returns:
            成功した場合 True
        """
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None or not account.mfa_secret:
                return False
            if self._mfa.verify_totp(account.mfa_secret, code):
                account.mfa_enabled = True
                await session.commit()
                return True
            return False

    async def disable_mfa(self, username: str) -> bool:
        """MFA を無効化.

        Args:
            username: ユーザー名

        Returns:
            成功した場合 True
        """
        async with get_db_session() as session:
            account = await session.scalar(select(UserAccount).where(UserAccount.username == username))
            if account is None:
                return False
            account.mfa_enabled = False
            account.mfa_secret = None
            await session.commit()
            return True
