"""TenantInvitationService のユニットテスト."""

from __future__ import annotations

import re

import pytest
from apps.platform.services.tenant_invitation import (
    InviteServiceError,
    TenantInvitationService,
)


_TOKEN_RE = re.compile(r"token=([A-Za-z0-9_-]+)")
_OTP_RE = re.compile(r"OTP:\s*(\d{6})")


def _extract_token(body: str) -> str:
    match = _TOKEN_RE.search(body)
    assert match is not None
    return match.group(1)


def _extract_otp(body: str) -> str:
    match = _OTP_RE.search(body)
    assert match is not None
    return match.group(1)


def test_issue_link_invitation_sends_two_separate_emails() -> None:
    """リンク方式は通知メールとログインメールを分離する."""
    service = TenantInvitationService()
    created = service.issue_invitation(
        tenant_id="tenant-a",
        recipient_email="user@example.com",
        inviter_display_name="Operator",
        challenge_method="link",
        auto_send_challenge=True,
    )

    assert created["challenge_email_sent"] is True
    outbox = service.list_outbox()
    assert len(outbox) == 2
    first = outbox[0]["body"]
    second = outbox[1]["body"]

    assert "token=" not in first
    assert "OTP:" not in first
    assert "http://" not in first
    assert "https://" not in first
    assert "token=" in second


def test_link_invitation_is_one_time() -> None:
    """リンクは1回のみ有効."""
    service = TenantInvitationService()
    created = service.issue_invitation(
        tenant_id="tenant-a",
        recipient_email="user@example.com",
        inviter_display_name=None,
        challenge_method="link",
        auto_send_challenge=True,
    )
    invitation_id = str(created["invitation_id"])
    token = _extract_token(str(service.list_outbox()[1]["body"]))

    consumed = service.consume_invitation(invitation_id=invitation_id, token=token)
    assert consumed["accepted"] is True

    with pytest.raises(InviteServiceError) as exc_info:
        service.consume_invitation(invitation_id=invitation_id, token=token)
    assert exc_info.value.code == "INVITATION_ALREADY_CONSUMED"


def test_resend_link_invalidates_old_token() -> None:
    """リンク再送時は旧トークンを失効させる."""
    service = TenantInvitationService()
    created = service.issue_invitation(
        tenant_id="tenant-a",
        recipient_email="user@example.com",
        inviter_display_name=None,
        challenge_method="link",
        auto_send_challenge=False,
    )
    invitation_id = str(created["invitation_id"])

    service.send_login_challenge(invitation_id)
    token_first = _extract_token(str(service.list_outbox()[-1]["body"]))
    service.send_login_challenge(invitation_id)
    token_second = _extract_token(str(service.list_outbox()[-1]["body"]))
    assert token_first != token_second

    with pytest.raises(InviteServiceError) as exc_info:
        service.consume_invitation(invitation_id=invitation_id, token=token_first)
    assert exc_info.value.code == "INVALID_TOKEN"

    consumed = service.consume_invitation(invitation_id=invitation_id, token=token_second)
    assert consumed["accepted"] is True


def test_otp_flow_works() -> None:
    """OTP 方式で招待を消費できる."""
    service = TenantInvitationService()
    created = service.issue_invitation(
        tenant_id="tenant-b",
        recipient_email="otp-user@example.com",
        inviter_display_name=None,
        challenge_method="otp",
        auto_send_challenge=True,
    )
    invitation_id = str(created["invitation_id"])
    otp_code = _extract_otp(str(service.list_outbox()[1]["body"]))

    consumed = service.consume_invitation(invitation_id=invitation_id, otp_code=otp_code)
    assert consumed["accepted"] is True
