"""Tenant Invitations Router.

POST /api/studios/framework/tenants/invitations                - 招待作成
POST /api/studios/framework/tenants/invitations/{id}/challenge - ログイン手段を別送
POST /api/studios/framework/tenants/invitations/consume        - 招待消費
GET  /api/studios/framework/tenants/invitations/{id}           - 招待状態取得
GET  /api/studios/framework/tenants/invitations/outbox         - 開発用送信箱確認
"""

from __future__ import annotations

import os
from typing import Any

from apps.platform.schemas.tenant_invitation_schemas import (
    TenantInvitationChallengeRequest,
    TenantInvitationConsumeRequest,
    TenantInvitationConsumeResponse,
    TenantInvitationCreateRequest,
    TenantInvitationCreateResponse,
    TenantInvitationStatusResponse,
)
from apps.platform.services.tenant_invitation import (
    InviteServiceError,
    TenantInvitationService,
)
from fastapi import APIRouter, HTTPException


router = APIRouter(
    prefix="/api/studios/framework/tenants/invitations",
    tags=["tenant-invitations"],
)

_service: TenantInvitationService | None = None


def init_tenant_invitation_services(service: TenantInvitationService) -> None:
    """サービスインスタンスを設定."""
    global _service
    _service = service


def _get_service() -> TenantInvitationService:
    if _service is None:
        msg = "TenantInvitationService が未初期化です"
        raise RuntimeError(msg)
    return _service


def _raise_http_error(exc: InviteServiceError) -> HTTPException:
    return HTTPException(
        status_code=exc.status_code,
        detail={
            "message": str(exc),
            "error_code": exc.code,
        },
    )


@router.post("", response_model=TenantInvitationCreateResponse)
async def create_invitation(
    request: TenantInvitationCreateRequest,
) -> TenantInvitationCreateResponse:
    """招待を作成し、通知メールと (任意で) ログイン手段メールを送信する."""
    service = _get_service()
    try:
        payload = service.issue_invitation(
            tenant_id=request.tenant_id,
            recipient_email=request.recipient_email,
            inviter_display_name=request.inviter_display_name,
            challenge_method=request.challenge_method,
            auto_send_challenge=request.auto_send_challenge,
        )
    except InviteServiceError as exc:
        raise _raise_http_error(exc) from None
    return TenantInvitationCreateResponse.model_validate(payload)


@router.post("/{invitation_id}/challenge", response_model=TenantInvitationStatusResponse)
async def send_challenge_email(
    invitation_id: str,
    request: TenantInvitationChallengeRequest,
) -> TenantInvitationStatusResponse:
    """ログイン手段メール (2通目) を送信する."""
    service = _get_service()
    try:
        payload = service.send_login_challenge(
            invitation_id,
            challenge_method=request.challenge_method,
        )
    except InviteServiceError as exc:
        raise _raise_http_error(exc) from None
    return TenantInvitationStatusResponse.model_validate(payload)


@router.post("/consume", response_model=TenantInvitationConsumeResponse)
async def consume_invitation(
    request: TenantInvitationConsumeRequest,
) -> TenantInvitationConsumeResponse:
    """ワンタイム URL / OTP を検証して招待を消費する."""
    service = _get_service()
    try:
        payload = service.consume_invitation(
            invitation_id=request.invitation_id,
            token=request.token,
            otp_code=request.otp_code,
        )
    except InviteServiceError as exc:
        raise _raise_http_error(exc) from None
    return TenantInvitationConsumeResponse.model_validate(payload)


@router.get("/outbox", response_model=dict[str, Any])
async def get_invitation_outbox() -> dict[str, Any]:
    """開発/テスト向けに送信箱を返す."""
    enabled = os.getenv("PLATFORM_INVITE_ENABLE_OUTBOX_ENDPOINT", "false").lower() == "true"
    if not enabled:
        raise HTTPException(
            status_code=404,
            detail={
                "message": "Outbox endpoint is disabled",
                "error_code": "OUTBOX_ENDPOINT_DISABLED",
            },
        )
    service = _get_service()
    messages = service.list_outbox()
    return {"messages": messages, "total": len(messages)}


@router.get("/{invitation_id}", response_model=TenantInvitationStatusResponse)
async def get_invitation_status(invitation_id: str) -> TenantInvitationStatusResponse:
    """招待状態を取得する."""
    service = _get_service()
    try:
        payload = service.get_invitation_status(invitation_id)
    except InviteServiceError as exc:
        raise _raise_http_error(exc) from None
    return TenantInvitationStatusResponse.model_validate(payload)
