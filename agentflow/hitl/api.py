"""HITL REST API エンドポイント.

FastAPI を使用した承認ワークフローの REST API。

エンドポイント:
    - GET  /hitl/requests           - 保留中の承認リクエスト一覧
    - GET  /hitl/requests/{id}      - 承認リクエスト詳細
    - POST /hitl/requests/{id}/approve - リクエストを承認
    - POST /hitl/requests/{id}/reject  - リクエストを拒否
    - POST /hitl/resume/{thread_id}    - ワークフローを再開

使用例:
    >>> from fastapi import FastAPI
    >>> from agentflow.hitl.api import create_hitl_router
    >>>
    >>> app = FastAPI()
    >>> hitl_router = create_hitl_router(approval_manager)
    >>> app.include_router(hitl_router, prefix="/api/v1")
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from fastapi import APIRouter

    from agentflow.hitl.approval_manager import ApprovalManager

logger = logging.getLogger(__name__)


# =============================================================================
# API リクエスト/レスポンスモデル
# =============================================================================


class ApproveRequest(BaseModel):
    """承認リクエストボディ."""

    approver: str | None = Field(None, description="承認者")
    comment: str | None = Field(None, description="コメント")
    modifications: dict[str, Any] = Field(default_factory=dict, description="修正内容")


class RejectRequest(BaseModel):
    """拒否リクエストボディ."""

    rejector: str | None = Field(None, description="拒否者")
    reason: str | None = Field(None, description="拒否理由")


class ResumeRequest(BaseModel):
    """再開リクエストボディ."""

    command_type: str = Field(..., description="コマンドタイプ (approve/reject/update)")
    value: Any = Field(None, description="コマンド値")
    issuer: str | None = Field(None, description="発行者")


class ApprovalRequestResponse(BaseModel):
    """承認リクエストレスポンス."""

    id: str
    action: str
    reason: str
    priority: str
    context: dict[str, Any]
    requester: str | None
    created_at: str


class ApprovalListResponse(BaseModel):
    """承認リクエスト一覧レスポンス."""

    requests: list[ApprovalRequestResponse]
    total: int


class APIResponse(BaseModel):
    """汎用 API レスポンス."""

    success: bool
    message: str
    data: dict[str, Any] = Field(default_factory=dict)


# =============================================================================
# Router ファクトリー
# =============================================================================


def create_hitl_router(
    approval_manager: ApprovalManager,
    *,
    prefix: str = "/hitl",
    tags: list[str] | None = None,
) -> APIRouter:
    """HITL 用の FastAPI Router を作成.

    Args:
        approval_manager: ApprovalManager インスタンス
        prefix: ルートプレフィックス
        tags: OpenAPI タグ

    Returns:
        設定済みの APIRouter
    """
    from fastapi import APIRouter, HTTPException

    router = APIRouter(prefix=prefix, tags=tags or ["hitl"])

    @router.get("/requests", response_model=ApprovalListResponse)
    async def list_pending_requests() -> ApprovalListResponse:
        """保留中の承認リクエスト一覧を取得."""
        requests = approval_manager.get_pending_requests()
        return ApprovalListResponse(
            requests=[
                ApprovalRequestResponse(
                    id=req.id,
                    action=req.action,
                    reason=req.reason,
                    priority=req.priority,
                    context=req.context,
                    requester=req.requester,
                    created_at=req.created_at.isoformat(),
                )
                for req in requests
            ],
            total=len(requests),
        )

    @router.get("/requests/{request_id}", response_model=ApprovalRequestResponse)
    async def get_request(request_id: str) -> ApprovalRequestResponse:
        """承認リクエスト詳細を取得."""
        req = approval_manager.get_request(request_id)
        if req is None:
            raise HTTPException(status_code=404, detail="Request not found")
        return ApprovalRequestResponse(
            id=req.id,
            action=req.action,
            reason=req.reason,
            priority=req.priority,
            context=req.context,
            requester=req.requester,
            created_at=req.created_at.isoformat(),
        )

    @router.post("/requests/{request_id}/approve", response_model=APIResponse)
    async def approve_request(request_id: str, body: ApproveRequest) -> APIResponse:
        """リクエストを承認."""
        success = await approval_manager.approve(
            request_id,
            approver=body.approver,
            comment=body.comment,
            modifications=body.modifications,
        )
        if not success:
            raise HTTPException(status_code=404, detail="Request not found")
        return APIResponse(success=True, message="Request approved")

    @router.post("/requests/{request_id}/reject", response_model=APIResponse)
    async def reject_request(request_id: str, body: RejectRequest) -> APIResponse:
        """リクエストを拒否."""
        success = await approval_manager.reject(
            request_id,
            rejector=body.rejector,
            reason=body.reason,
        )
        if not success:
            raise HTTPException(status_code=404, detail="Request not found")
        return APIResponse(success=True, message="Request rejected")

    @router.post("/requests/{request_id}/escalate", response_model=APIResponse)
    async def escalate_request(request_id: str) -> APIResponse:
        """リクエストをエスカレート."""
        await approval_manager.escalate(request_id)
        return APIResponse(success=True, message="Request escalated")

    return router
