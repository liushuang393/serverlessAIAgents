"""リバースプロキシ認証プロバイダー.

HMAC-SHA256 署名付きヘッダーでプロキシからユーザー情報を受け取る。
Identity-Aware Proxy (IAP) や Nginx auth_request との連携に使用する。
"""

from __future__ import annotations

import hashlib
import hmac
import logging
import secrets
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING

from apps.auth_service.db.session import get_db_session
from apps.auth_service.models.user import ProxyAuthNonce
from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity
from sqlalchemy import delete, select


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)


class ProxyAuthProvider(AuthProvider):
    """リバースプロキシ認証プロバイダー.

    信頼されたプロキシからの HMAC 署名付きリクエストを検証する。
    """

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)

    @property
    def provider_name(self) -> str:
        return "proxy"

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """パスワード認証は非対応（プロキシヘッダー専用）."""
        return AuthResult(
            success=False,
            message="プロキシ認証はパスワード方式に対応していません。",
        )

    async def verify_proxy_headers(
        self,
        *,
        user: str | None,
        display_name: str | None,
        role: str | None,
        department: str | None,
        position: str | None,
        timestamp: str | None,
        nonce: str | None,
        signature: str | None,
        request_method: str,
        request_path: str,
    ) -> ExternalIdentity | None:
        """プロキシヘッダーを検証してアイデンティティを返す.

        Args:
            user: ユーザー名ヘッダー値
            display_name: 表示名ヘッダー値
            role: ロールヘッダー値
            department: 部署ヘッダー値
            position: 役職ヘッダー値
            timestamp: Unix タイムスタンプ文字列
            nonce: ランダム nonce
            signature: HMAC-SHA256 署名
            request_method: HTTP メソッド
            request_path: リクエストパス

        Returns:
            ExternalIdentity または None（検証失敗時）
        """
        username = (user or "").strip()
        if not username:
            return None

        require_sig = self._settings.PROXY_AUTH_REQUIRE_SIGNATURE
        shared_secret = self._settings.PROXY_AUTH_SECRET

        if require_sig:
            if not shared_secret:
                logger.error("PROXY_AUTH_SECRET が未設定です")
                return None
            if not timestamp or not nonce or not signature:
                logger.warning("プロキシ認証: timestamp/nonce/signature が不足しています")
                return None
            if not self._verify_timestamp(timestamp):
                logger.warning("プロキシ認証: タイムスタンプが許容範囲外です")
                return None
            if not self._verify_signature(
                username=username,
                display_name=display_name or username,
                role=role or "employee",
                department=department or "",
                position=position or "",
                timestamp=timestamp,
                nonce=nonce,
                signature=signature,
                request_method=request_method,
                request_path=request_path,
                shared_secret=shared_secret,
            ):
                logger.warning("プロキシ認証: HMAC 署名が無効です")
                return None
            if not await self._record_nonce(nonce):
                logger.warning("プロキシ認証: nonce が再利用されました")
                return None

        return ExternalIdentity(
            username=username,
            display_name=(display_name or username).strip(),
            role=(role or "employee").strip(),
            department=(department or "").strip(),
            position=(position or "").strip(),
        )

    def _verify_timestamp(self, timestamp: str) -> bool:
        """タイムスタンプが許容時刻差以内か検証."""
        try:
            ts = int(timestamp)
        except ValueError:
            return False
        now = int(datetime.now(tz=UTC).timestamp())
        return abs(now - ts) <= self._settings.PROXY_AUTH_MAX_SKEW_SECONDS

    def _verify_signature(
        self,
        *,
        username: str,
        display_name: str,
        role: str,
        department: str,
        position: str,
        timestamp: str,
        nonce: str,
        signature: str,
        request_method: str,
        request_path: str,
        shared_secret: str,
    ) -> bool:
        """HMAC-SHA256 署名を検証.

        署名対象文字列（canonical string）は改行区切りの順序付きフィールド。
        """
        canonical = "\n".join(
            [
                request_method.upper(),
                request_path,
                username,
                display_name,
                role,
                department,
                position,
                timestamp,
                nonce,
            ]
        )
        expected = hmac.new(
            shared_secret.encode("utf-8"),
            canonical.encode("utf-8"),
            hashlib.sha256,
        ).hexdigest()
        normalized_sig = signature.strip().lower()
        if normalized_sig.startswith("sha256="):
            normalized_sig = normalized_sig.split("=", 1)[1].strip()
        return secrets.compare_digest(expected, normalized_sig)

    async def _record_nonce(self, nonce: str) -> bool:
        """nonce を記録し、再利用を防止.

        Returns:
            新規 nonce の場合 True、再利用の場合 False
        """
        now = datetime.now(tz=UTC)
        nonce_hash = hashlib.sha256(nonce.encode("utf-8")).hexdigest()

        async with get_db_session() as session:
            # 期限切れ nonce を削除
            await session.execute(delete(ProxyAuthNonce).where(ProxyAuthNonce.expires_at < now))
            existing = await session.scalar(select(ProxyAuthNonce).where(ProxyAuthNonce.nonce_hash == nonce_hash))
            if existing is not None:
                return False
            session.add(
                ProxyAuthNonce(
                    id=f"nonce-{secrets.token_hex(12)}",
                    nonce_hash=nonce_hash,
                    expires_at=now + timedelta(seconds=self._settings.PROXY_AUTH_MAX_SKEW_SECONDS),
                )
            )
            await session.commit()
        return True
