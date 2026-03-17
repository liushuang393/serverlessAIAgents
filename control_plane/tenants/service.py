"""Control-plane tenant 管理サービス.

テナント台帳と、招待メールを扱う TenantInvitationService の正規実装を提供する。
"""

from __future__ import annotations

import hashlib
import hmac
import logging
import os
import secrets
import smtplib
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from email.message import EmailMessage
from typing import TYPE_CHECKING, Literal
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit
from uuid import uuid4


if TYPE_CHECKING:
    from collections.abc import Callable


ChallengeMethod = Literal["otp", "link"]
_MASK_LOCAL_SHORT_THRESHOLD = 2


@dataclass(frozen=True, slots=True)
class TenantRecord:
    """tenant の最小情報."""

    tenant_id: str
    display_name: str
    scopes: tuple[str, ...] = ()


class TenantService:
    """tenant 情報を管理する."""

    def __init__(self) -> None:
        self._tenants: dict[str, TenantRecord] = {}

    def upsert(self, tenant: TenantRecord) -> None:
        """tenant を登録または更新する。"""
        self._tenants[tenant.tenant_id] = tenant

    def get(self, tenant_id: str) -> TenantRecord | None:
        """tenant を取得する。"""
        return self._tenants.get(tenant_id)

    def list_tenants(self) -> list[TenantRecord]:
        """tenant 一覧を返す。"""
        return sorted(self._tenants.values(), key=lambda item: item.tenant_id)


@dataclass(slots=True)
class OutboundEmail:
    """送信メール."""

    recipient: str
    subject: str
    body: str
    category: str
    sent_at: datetime


class InviteServiceError(Exception):
    """招待処理エラー."""

    def __init__(self, message: str, *, code: str, status_code: int = 400) -> None:
        super().__init__(message)
        self.code = code
        self.status_code = status_code


class EmailTransport:
    """メール送信抽象."""

    def send(self, mail: OutboundEmail) -> None:
        """メールを送信する。"""
        raise NotImplementedError


class InMemoryEmailTransport(EmailTransport):
    """テスト・ローカル開発用のインメモリ送信."""

    def __init__(self) -> None:
        self.messages: list[OutboundEmail] = []
        self._logger = logging.getLogger(__name__)

    def send(self, mail: OutboundEmail) -> None:
        """メールを保持する。"""
        self.messages.append(mail)
        self._logger.info(
            "Tenant invite mail queued: to=%s category=%s subject=%s",
            mail.recipient,
            mail.category,
            mail.subject,
        )


class SMTPEmailTransport(EmailTransport):
    """SMTP 送信."""

    def __init__(
        self,
        *,
        host: str,
        port: int,
        username: str | None,
        password: str | None,
        from_email: str,
        use_starttls: bool,
        timeout_seconds: int = 10,
    ) -> None:
        self._host = host
        self._port = port
        self._username = username
        self._password = password
        self._from_email = from_email
        self._use_starttls = use_starttls
        self._timeout_seconds = timeout_seconds

    def send(self, mail: OutboundEmail) -> None:
        """メールを送信する。"""
        msg = EmailMessage()
        msg["From"] = self._from_email
        msg["To"] = mail.recipient
        msg["Subject"] = mail.subject
        msg.set_content(mail.body)

        with smtplib.SMTP(self._host, self._port, timeout=self._timeout_seconds) as smtp:
            if self._use_starttls:
                smtp.starttls()
            if self._username:
                smtp.login(self._username, self._password or "")
            smtp.send_message(msg)


@dataclass(slots=True)
class InvitationRecord:
    """招待レコード."""

    invitation_id: str
    tenant_id: str
    recipient_email: str
    inviter_display_name: str | None
    challenge_method: ChallengeMethod
    created_at: datetime
    expires_at: datetime
    notification_sent_at: datetime | None = None
    challenge_sent_at: datetime | None = None
    challenge_sent_count: int = 0
    token_hash: str | None = None
    otp_hash: str | None = None
    consumed_at: datetime | None = None

    def is_expired(self, now: datetime) -> bool:
        """期限切れ判定."""
        return now >= self.expires_at


class TenantInvitationService:
    """多租戸招待サービス."""

    def __init__(
        self,
        *,
        email_transport: EmailTransport | None = None,
        now_fn: Callable[[], datetime] | None = None,
    ) -> None:
        self._logger = logging.getLogger(__name__)
        self._now_fn = now_fn or (lambda: datetime.now(UTC))
        self._records: dict[str, InvitationRecord] = {}
        self._token_pepper = os.getenv("PLATFORM_INVITE_TOKEN_PEPPER", "")
        self._login_base_url = os.getenv(
            "PLATFORM_INVITE_LOGIN_BASE_URL",
            "https://login.example.invalid/tenant-login",
        )
        self._support_contact = os.getenv(
            "PLATFORM_INVITE_SUPPORT_CONTACT",
            "support@example.invalid",
        )
        self._ttl_minutes = int(os.getenv("PLATFORM_INVITE_TTL_MINUTES", "15"))
        self._transport = email_transport or self._build_default_transport()

    def issue_invitation(
        self,
        *,
        tenant_id: str,
        recipient_email: str,
        inviter_display_name: str | None,
        challenge_method: ChallengeMethod,
        auto_send_challenge: bool,
    ) -> dict[str, object]:
        """招待を発行し、通知メールを送信する。"""
        now = self._now()
        invitation_id = uuid4().hex
        record = InvitationRecord(
            invitation_id=invitation_id,
            tenant_id=tenant_id.strip(),
            recipient_email=recipient_email.strip(),
            inviter_display_name=(inviter_display_name or "").strip() or None,
            challenge_method=challenge_method,
            created_at=now,
            expires_at=now + timedelta(minutes=self._ttl_minutes),
        )
        self._records[invitation_id] = record
        self._send_notification_mail(record)

        challenge_sent = False
        if auto_send_challenge:
            self.send_login_challenge(invitation_id)
            challenge_sent = True

        return self.get_invitation_status(invitation_id) | {
            "notification_email_sent": True,
            "challenge_email_sent": challenge_sent,
        }

    def send_login_challenge(
        self,
        invitation_id: str,
        *,
        challenge_method: ChallengeMethod | None = None,
    ) -> dict[str, object]:
        """ログイン手段メールを送信する。"""
        record = self._require_active_record(invitation_id)
        method = challenge_method or record.challenge_method
        now = self._now()

        if method == "link":
            token = secrets.token_urlsafe(32)
            record.token_hash = self._hash_secret(token)
            record.otp_hash = None
            body = self._build_link_mail_body(record, self._build_login_url(token))
            category = "login_link"
        else:
            otp_code = f"{secrets.randbelow(1_000_000):06d}"
            record.otp_hash = self._hash_secret(otp_code)
            record.token_hash = None
            body = self._build_otp_mail_body(record, otp_code)
            category = "otp_code"

        record.challenge_method = method
        record.challenge_sent_at = now
        record.challenge_sent_count += 1
        self._transport.send(
            OutboundEmail(
                recipient=record.recipient_email,
                subject="[AgentFlow] ログイン情報 (別送)",
                body=body,
                category=category,
                sent_at=now,
            )
        )
        return self.get_invitation_status(invitation_id)

    def consume_invitation(
        self,
        *,
        invitation_id: str,
        token: str | None = None,
        otp_code: str | None = None,
    ) -> dict[str, object]:
        """ワンタイム URL または OTP を検証する。"""
        record = self._require_active_record(invitation_id)
        supplied_token = (token or "").strip()
        supplied_otp = (otp_code or "").strip()

        if bool(supplied_token) == bool(supplied_otp):
            message = "token または otp_code のどちらか一方を指定してください"
            raise InviteServiceError(
                message,
                code="INVALID_CHALLENGE_INPUT",
                status_code=400,
            )

        if supplied_token:
            is_valid_token = record.token_hash is not None and self._is_hash_match(record.token_hash, supplied_token)
            if not is_valid_token:
                message = "ワンタイムリンクが無効です"
                raise InviteServiceError(
                    message,
                    code="INVALID_TOKEN",
                    status_code=401,
                )
        elif record.otp_hash is None or not self._is_hash_match(record.otp_hash, supplied_otp):
            message = "OTP が無効です"
            raise InviteServiceError(
                message,
                code="INVALID_OTP",
                status_code=401,
            )

        now = self._now()
        record.consumed_at = now
        record.token_hash = None
        record.otp_hash = None
        return {
            "accepted": True,
            "invitation_id": invitation_id,
            "tenant_id": record.tenant_id,
            "consumed_at": now.isoformat(),
            "access_ticket": f"invite_{secrets.token_urlsafe(24)}",
        }

    def get_invitation_status(self, invitation_id: str) -> dict[str, object]:
        """招待状態を返す。"""
        record = self._records.get(invitation_id)
        if record is None:
            message = f"Invitation not found: {invitation_id}"
            raise InviteServiceError(
                message,
                code="INVITATION_NOT_FOUND",
                status_code=404,
            )

        now = self._now()
        return {
            "invitation_id": record.invitation_id,
            "tenant_id": record.tenant_id,
            "recipient": self._mask_email(record.recipient_email),
            "challenge_method": record.challenge_method,
            "expires_at": record.expires_at.isoformat(),
            "is_expired": record.is_expired(now),
            "notification_sent": record.notification_sent_at is not None,
            "challenge_sent": record.challenge_sent_at is not None,
            "challenge_sent_count": record.challenge_sent_count,
            "consumed": record.consumed_at is not None,
            "consumed_at": record.consumed_at.isoformat() if record.consumed_at else None,
        }

    def list_outbox(self) -> list[dict[str, object]]:
        """インメモリ送信箱を返す。"""
        if not isinstance(self._transport, InMemoryEmailTransport):
            return []
        return [
            {
                "recipient": message.recipient,
                "subject": message.subject,
                "body": message.body,
                "category": message.category,
                "sent_at": message.sent_at.isoformat(),
            }
            for message in self._transport.messages
        ]

    def _require_active_record(self, invitation_id: str) -> InvitationRecord:
        record = self._records.get(invitation_id)
        if record is None:
            message = f"Invitation not found: {invitation_id}"
            raise InviteServiceError(
                message,
                code="INVITATION_NOT_FOUND",
                status_code=404,
            )

        now = self._now()
        if record.consumed_at is not None:
            message = "招待は既に使用済みです"
            raise InviteServiceError(
                message,
                code="INVITATION_ALREADY_CONSUMED",
                status_code=409,
            )
        if record.is_expired(now):
            message = "招待の有効期限が切れています"
            raise InviteServiceError(
                message,
                code="INVITATION_EXPIRED",
                status_code=410,
            )
        return record

    def _build_default_transport(self) -> EmailTransport:
        mode = os.getenv("PLATFORM_INVITE_EMAIL_TRANSPORT", "memory").strip().lower()
        if mode != "smtp":
            return InMemoryEmailTransport()

        host = os.getenv("PLATFORM_INVITE_SMTP_HOST", "").strip()
        from_email = os.getenv("PLATFORM_INVITE_FROM_EMAIL", "").strip()
        if not host or not from_email:
            self._logger.warning("SMTP 設定が不足しているため in-memory transport へフォールバックします")
            return InMemoryEmailTransport()

        return SMTPEmailTransport(
            host=host,
            port=int(os.getenv("PLATFORM_INVITE_SMTP_PORT", "587")),
            username=os.getenv("PLATFORM_INVITE_SMTP_USERNAME", "").strip() or None,
            password=os.getenv("PLATFORM_INVITE_SMTP_PASSWORD", "").strip() or None,
            from_email=from_email,
            use_starttls=os.getenv("PLATFORM_INVITE_SMTP_STARTTLS", "true").lower() == "true",
            timeout_seconds=int(os.getenv("PLATFORM_INVITE_SMTP_TIMEOUT_SECONDS", "10")),
        )

    def _send_notification_mail(self, record: InvitationRecord) -> None:
        now = self._now()
        record.notification_sent_at = now
        self._transport.send(
            OutboundEmail(
                recipient=record.recipient_email,
                subject="[AgentFlow] 招待通知",
                body=self._build_notification_mail_body(record),
                category="invitation_notice",
                sent_at=now,
            )
        )

    def _build_notification_mail_body(self, record: InvitationRecord) -> str:
        inviter_line = f"- 招待者: {record.inviter_display_name}\n" if record.inviter_display_name else ""
        return (
            "AgentFlow への招待を受け付けました。\n"
            "このメールにはログイン URL / OTP を含めていません。\n"
            "ログイン情報は別メールで送信されます。\n\n"
            f"{inviter_line}"
            f"- 有効期限: {record.expires_at.isoformat()}\n"
            f"- サポート: {self._support_contact}\n\n"
            "心当たりがない場合は、このメールを破棄してサポートへ連絡してください。\n"
        )

    def _build_link_mail_body(self, record: InvitationRecord, login_url: str) -> str:
        return (
            "以下は別送のワンタイムログイン URL です。\n"
            "この URL は 1 回のみ有効で、有効期限を過ぎると失効します。\n\n"
            f"{login_url}\n\n"
            f"有効期限: {record.expires_at.isoformat()}\n"
            "このメールを転送しないでください。\n"
        )

    def _build_otp_mail_body(self, record: InvitationRecord, otp_code: str) -> str:
        return (
            "以下は別送のワンタイムコードです。\n"
            "このコードは 1 回のみ有効で、有効期限を過ぎると失効します。\n\n"
            f"OTP: {otp_code}\n\n"
            f"有効期限: {record.expires_at.isoformat()}\n"
            "このメールを転送しないでください。\n"
        )

    def _build_login_url(self, token: str) -> str:
        parsed = urlsplit(self._login_base_url)
        query = dict(parse_qsl(parsed.query, keep_blank_values=True))
        query["token"] = token
        return urlunsplit(
            (
                parsed.scheme,
                parsed.netloc,
                parsed.path,
                urlencode(query, doseq=True),
                parsed.fragment,
            )
        )

    def _hash_secret(self, raw_value: str) -> str:
        return hashlib.sha256(f"{self._token_pepper}:{raw_value}".encode()).hexdigest()

    def _is_hash_match(self, expected_hash: str, supplied_raw: str) -> bool:
        return hmac.compare_digest(expected_hash, self._hash_secret(supplied_raw))

    def _mask_email(self, email: str) -> str:
        if "@" not in email:
            return "***"
        local, domain = email.split("@", 1)
        visible_prefix_len = 1 if len(local) <= _MASK_LOCAL_SHORT_THRESHOLD else _MASK_LOCAL_SHORT_THRESHOLD
        return f"{local[:visible_prefix_len]}***@{domain}"

    def _now(self) -> datetime:
        now = self._now_fn()
        if now.tzinfo is None:
            return now.replace(tzinfo=UTC)
        return now.astimezone(UTC)


__all__ = [
    "ChallengeMethod",
    "EmailTransport",
    "InMemoryEmailTransport",
    "InvitationRecord",
    "InviteServiceError",
    "OutboundEmail",
    "SMTPEmailTransport",
    "TenantInvitationService",
    "TenantRecord",
    "TenantService",
]
