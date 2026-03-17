"""Tenant invitations router のユニットテスト."""

from __future__ import annotations

import re
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from fastapi.testclient import TestClient


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


class TestTenantInvitationsRouter:
    """`/api/studios/framework/tenants/invitations` テスト."""

    def test_create_link_invitation_and_consume(self, phase3_test_client: TestClient) -> None:
        create = phase3_test_client.post(
            "/api/studios/framework/tenants/invitations",
            json={
                "tenant_id": "tenant-x",
                "recipient_email": "link-user@example.com",
                "challenge_method": "link",
                "auto_send_challenge": True,
            },
        )
        assert create.status_code == 200
        created = create.json()
        invitation_id = created["invitation_id"]
        assert created["notification_email_sent"] is True
        assert created["challenge_email_sent"] is True

        outbox_resp = phase3_test_client.get("/api/studios/framework/tenants/invitations/outbox")
        assert outbox_resp.status_code == 200
        outbox = outbox_resp.json()
        assert outbox["total"] == 2
        assert outbox["messages"][0]["category"] == "invitation_notice"
        assert outbox["messages"][1]["category"] == "login_link"

        token = _extract_token(outbox["messages"][1]["body"])
        consume = phase3_test_client.post(
            "/api/studios/framework/tenants/invitations/consume",
            json={"invitation_id": invitation_id, "token": token},
        )
        assert consume.status_code == 200
        assert consume.json()["accepted"] is True

        consume_again = phase3_test_client.post(
            "/api/studios/framework/tenants/invitations/consume",
            json={"invitation_id": invitation_id, "token": token},
        )
        assert consume_again.status_code == 409
        assert consume_again.json()["detail"]["error_code"] == "INVITATION_ALREADY_CONSUMED"

    def test_otp_challenge_can_be_sent_later(self, phase3_test_client: TestClient) -> None:
        create = phase3_test_client.post(
            "/api/studios/framework/tenants/invitations",
            json={
                "tenant_id": "tenant-y",
                "recipient_email": "otp-user@example.com",
                "challenge_method": "otp",
                "auto_send_challenge": False,
            },
        )
        assert create.status_code == 200
        invitation_id = create.json()["invitation_id"]

        outbox_initial_resp = phase3_test_client.get(
            "/api/studios/framework/tenants/invitations/outbox",
        )
        outbox_initial = outbox_initial_resp.json()
        assert outbox_initial["total"] == 1
        assert outbox_initial["messages"][0]["category"] == "invitation_notice"

        resend = phase3_test_client.post(
            f"/api/studios/framework/tenants/invitations/{invitation_id}/challenge",
            json={},
        )
        assert resend.status_code == 200
        assert resend.json()["challenge_sent"] is True

        outbox_after_resp = phase3_test_client.get(
            "/api/studios/framework/tenants/invitations/outbox",
        )
        outbox_after = outbox_after_resp.json()
        assert outbox_after["total"] == 2
        assert outbox_after["messages"][1]["category"] == "otp_code"
        otp_code = _extract_otp(outbox_after["messages"][1]["body"])

        consume = phase3_test_client.post(
            "/api/studios/framework/tenants/invitations/consume",
            json={"invitation_id": invitation_id, "otp_code": otp_code},
        )
        assert consume.status_code == 200
        assert consume.json()["accepted"] is True

    def test_unknown_invitation_returns_404(self, phase3_test_client: TestClient) -> None:
        resp = phase3_test_client.get(
            "/api/studios/framework/tenants/invitations/not-found-invitation",
        )
        assert resp.status_code == 404
        assert resp.json()["detail"]["error_code"] == "INVITATION_NOT_FOUND"
