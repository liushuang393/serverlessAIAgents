"""認証プロキシヘッダー検証."""

from __future__ import annotations

import hashlib
import hmac
import os
import secrets
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta

from sqlalchemy import delete, select

from apps.faq_system.backend.db.models import ProxyAuthNonce
from apps.faq_system.backend.db.session import get_db_session


@dataclass(slots=True)
class VerifiedProxyIdentity:
    """検証済みプロキシ認証アイデンティティ."""

    username: str
    display_name: str
    role: str
    department: str
    position: str


class ProxyAuthVerifier:
    """認証プロキシ署名を検証する."""

    def __init__(self) -> None:
        self._max_skew_seconds = 300
        self._require_signature = True
        self._shared_secret = ""
        self._reload_config()

    async def verify(
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
    ) -> VerifiedProxyIdentity | None:
        """ヘッダーを検証してアイデンティティを返す."""
        self._reload_config()
        username = (user or "").strip()
        if not username:
            return None

        if self._require_signature:
            if not self._shared_secret:
                return None
            if not timestamp or not nonce or not signature:
                return None
            if not self._verify_timestamp(timestamp):
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
            ):
                return None
            if not await self._record_nonce(nonce):
                return None

        return VerifiedProxyIdentity(
            username=username,
            display_name=(display_name or username).strip(),
            role=(role or "employee").strip(),
            department=(department or "").strip(),
            position=(position or "").strip(),
        )

    def _verify_timestamp(self, timestamp: str) -> bool:
        """許容時刻差を検証."""
        try:
            ts = int(timestamp)
        except ValueError:
            return False

        now = int(datetime.now(tz=UTC).timestamp())
        return abs(now - ts) <= self._max_skew_seconds

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
    ) -> bool:
        """HMAC-SHA256 署名を検証."""
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
            self._shared_secret.encode("utf-8"),
            canonical.encode("utf-8"),
            hashlib.sha256,
        ).hexdigest()
        return secrets.compare_digest(expected, self._normalize_signature(signature))

    @staticmethod
    def _normalize_signature(signature: str) -> str:
        value = signature.strip().lower()
        if value.startswith("sha256="):
            return value.split("=", 1)[1].strip()
        return value

    async def _record_nonce(self, nonce: str) -> bool:
        """nonce を記録し、再利用を拒否."""
        now = datetime.now(tz=UTC)
        nonce_hash = hashlib.sha256(nonce.encode("utf-8")).hexdigest()

        async with get_db_session() as session:
            await session.execute(delete(ProxyAuthNonce).where(ProxyAuthNonce.expires_at < now))
            existing = await session.scalar(
                select(ProxyAuthNonce).where(ProxyAuthNonce.nonce_hash == nonce_hash)
            )
            if existing is not None:
                return False

            session.add(
                ProxyAuthNonce(
                    id=f"nonce-{secrets.token_hex(12)}",
                    nonce_hash=nonce_hash,
                    expires_at=now + timedelta(seconds=self._max_skew_seconds),
                )
            )

        return True

    def _reload_config(self) -> None:
        self._max_skew_seconds = int(os.getenv("FAQ_PROXY_AUTH_MAX_SKEW_SECONDS", "300"))
        self._require_signature = os.getenv("FAQ_PROXY_AUTH_REQUIRE_SIGNATURE", "true").lower() in {
            "1",
            "true",
            "yes",
            "on",
        }
        self._shared_secret = os.getenv("FAQ_PROXY_AUTH_SHARED_SECRET", "")


proxy_auth_verifier = ProxyAuthVerifier()
