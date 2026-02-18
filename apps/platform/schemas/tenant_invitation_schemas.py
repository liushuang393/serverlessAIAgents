"""Tenant 招待 API スキーマ."""

from __future__ import annotations

import re
from typing import Literal

from pydantic import BaseModel, Field, field_validator, model_validator


_EMAIL_RE = re.compile(r"^[^@\s]+@[^@\s]+\.[^@\s]+$")

ChallengeMethod = Literal["otp", "link"]


class TenantInvitationCreateRequest(BaseModel):
    """招待作成リクエスト."""

    tenant_id: str = Field(
        ...,
        min_length=1,
        max_length=120,
        description="対象テナントID",
    )
    recipient_email: str = Field(
        ...,
        min_length=5,
        max_length=320,
        description="招待先メールアドレス",
    )
    inviter_display_name: str | None = Field(
        default=None,
        max_length=120,
        description="招待者表示名",
    )
    challenge_method: ChallengeMethod = Field(default="otp", description="別送するログイン手段")
    auto_send_challenge: bool = Field(
        default=True,
        description="招待作成直後にログイン手段メールを送信する",
    )

    @field_validator("recipient_email")
    @classmethod
    def validate_email(cls, value: str) -> str:
        email = value.strip().lower()
        if not _EMAIL_RE.match(email):
            msg = f"メール形式が不正です: {value}"
            raise ValueError(msg)
        return email

    @field_validator("tenant_id")
    @classmethod
    def validate_tenant_id(cls, value: str) -> str:
        normalized = value.strip()
        if not normalized:
            msg = "tenant_id は必須です"
            raise ValueError(msg)
        return normalized

    @field_validator("inviter_display_name")
    @classmethod
    def normalize_inviter(cls, value: str | None) -> str | None:
        if value is None:
            return None
        normalized = value.strip()
        return normalized or None


class TenantInvitationChallengeRequest(BaseModel):
    """ログイン手段メール送信リクエスト."""

    challenge_method: ChallengeMethod | None = Field(
        default=None,
        description="指定時は送信方式を上書き",
    )


class TenantInvitationConsumeRequest(BaseModel):
    """招待消費リクエスト."""

    invitation_id: str = Field(..., min_length=1, description="招待ID")
    token: str | None = Field(default=None, description="ワンタイムURLトークン")
    otp_code: str | None = Field(default=None, description="ワンタイムOTP")

    @model_validator(mode="after")
    def validate_challenge_fields(self) -> TenantInvitationConsumeRequest:
        token = (self.token or "").strip()
        otp_code = (self.otp_code or "").strip()
        if bool(token) == bool(otp_code):
            msg = "token または otp_code のどちらか一方を指定してください"
            raise ValueError(msg)
        self.token = token or None
        self.otp_code = otp_code or None
        return self


class TenantInvitationCreateResponse(BaseModel):
    """招待作成レスポンス."""

    invitation_id: str
    tenant_id: str
    recipient: str
    challenge_method: ChallengeMethod
    expires_at: str
    is_expired: bool
    notification_sent: bool
    challenge_sent: bool
    challenge_sent_count: int
    consumed: bool
    consumed_at: str | None = None
    notification_email_sent: bool
    challenge_email_sent: bool


class TenantInvitationStatusResponse(BaseModel):
    """招待状態レスポンス."""

    invitation_id: str
    tenant_id: str
    recipient: str
    challenge_method: ChallengeMethod
    expires_at: str
    is_expired: bool
    notification_sent: bool
    challenge_sent: bool
    challenge_sent_count: int
    consumed: bool
    consumed_at: str | None = None


class TenantInvitationConsumeResponse(BaseModel):
    """招待消費レスポンス."""

    accepted: bool
    invitation_id: str
    tenant_id: str
    consumed_at: str
    access_ticket: str
