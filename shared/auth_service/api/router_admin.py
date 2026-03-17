"""管理画面向けユーザー管理 API ルーター.

プレフィックス: /auth/admin
管理者（admin）権限が必要。
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime

from fastapi import APIRouter, Depends, HTTPException, Query, status
from sqlalchemy import func, select

from shared.auth_service.api.dependencies import require_permission
from shared.auth_service.api.schemas import UserInfo
from shared.auth_service.api.schemas_authorization import (
    AdminResetPasswordRequest,
    PaginatedUsersResponse,
    UserAdminResponse,
    UserUpdateRequest,
)
from shared.auth_service.core.authorization import get_authorization_service
from shared.auth_service.core.password import PasswordManager
from shared.auth_service.db.session import get_db_session
from shared.auth_service.models.user import UserAccount


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/auth/admin", tags=["管理"])


# ---------------------------------------------------------------------------
# ユーザー管理
# ---------------------------------------------------------------------------


@router.get("/users", response_model=PaginatedUsersResponse)
async def list_users(
    page: int = Query(1, ge=1, description="ページ番号"),
    page_size: int = Query(20, ge=1, le=100, description="ページサイズ"),
    _user: UserInfo = Depends(require_permission("*")),
) -> PaginatedUsersResponse:
    """ユーザー一覧（ページネーション付き）."""
    authz = get_authorization_service()

    async with get_db_session() as session:
        # 総件数
        total_result = await session.scalar(select(func.count(UserAccount.id)))
        total = total_result or 0

        # ページネーション
        offset = (page - 1) * page_size
        result = await session.execute(
            select(UserAccount)
            .order_by(UserAccount.created_at.desc())
            .offset(offset)
            .limit(page_size)
        )
        accounts = result.scalars().all()

        users: list[UserAdminResponse] = []
        for account in accounts:
            roles = await authz.get_user_roles(account.id)
            permissions = await authz.get_user_permissions(account.id)
            users.append(_account_to_admin_response(account, roles, permissions))

        return PaginatedUsersResponse(
            users=users,
            total=total,
            page=page,
            page_size=page_size,
        )


@router.get("/users/{user_id}", response_model=UserAdminResponse)
async def get_user(
    user_id: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> UserAdminResponse:
    """ユーザー詳細."""
    authz = get_authorization_service()

    async with get_db_session() as session:
        account = await session.get(UserAccount, user_id)
        if account is None:
            raise HTTPException(status_code=404, detail="ユーザーが見つかりません")

        roles = await authz.get_user_roles(user_id)
        permissions = await authz.get_user_permissions(user_id)
        return _account_to_admin_response(account, roles, permissions)


@router.put("/users/{user_id}", response_model=UserAdminResponse)
async def update_user(
    user_id: str,
    req: UserUpdateRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> UserAdminResponse:
    """ユーザー更新（role, is_active 等）."""
    authz = get_authorization_service()

    async with get_db_session() as session:
        account = await session.get(UserAccount, user_id)
        if account is None:
            raise HTTPException(status_code=404, detail="ユーザーが見つかりません")

        if req.display_name is not None:
            account.display_name = req.display_name
        if req.department is not None:
            account.department = req.department
        if req.position is not None:
            account.position = req.position
        if req.role is not None:
            account.role = req.role
        if req.is_active is not None:
            account.is_active = req.is_active
        account.updated_at = datetime.now(tz=UTC)
        await session.commit()

        # キャッシュ無効化
        authz.invalidate_cache(user_id)

        roles = await authz.get_user_roles(user_id)
        permissions = await authz.get_user_permissions(user_id)
        return _account_to_admin_response(account, roles, permissions)


@router.delete("/users/{user_id}")
async def deactivate_user(
    user_id: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ユーザー無効化（論理削除）."""
    async with get_db_session() as session:
        account = await session.get(UserAccount, user_id)
        if account is None:
            raise HTTPException(status_code=404, detail="ユーザーが見つかりません")

        account.is_active = False
        account.updated_at = datetime.now(tz=UTC)
        await session.commit()

        get_authorization_service().invalidate_cache(user_id)
        return {"message": f"ユーザー '{account.username}' を無効化しました"}


@router.post("/users/{user_id}/reset-password")
async def admin_reset_password(
    user_id: str,
    req: AdminResetPasswordRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """管理者によるパスワードリセット."""
    pwd = PasswordManager()
    is_valid, reason = pwd.validate_password_strength(req.new_password)
    if not is_valid:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=reason)

    async with get_db_session() as session:
        account = await session.get(UserAccount, user_id)
        if account is None:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="ユーザーが見つかりません")
        if account.auth_source != "local_db":
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail="外部認証ユーザーのパスワードはリセットできません",
            )

        salt = pwd.generate_salt()
        account.password_salt = salt
        account.password_hash = pwd.hash_password(req.new_password, salt)
        account.updated_at = datetime.now(tz=UTC)
        await session.commit()

        return {"message": f"ユーザー '{account.username}' のパスワードをリセットしました"}


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _account_to_admin_response(
    account: UserAccount,
    roles: list[str],
    permissions: list[str],
) -> UserAdminResponse:
    """UserAccount を管理用レスポンスに変換."""
    return UserAdminResponse(
        user_id=account.id,
        username=account.username,
        display_name=account.display_name,
        department=account.department,
        position=account.position,
        role=account.role,
        roles=roles,
        email=account.email,
        is_active=account.is_active,
        auth_source=account.auth_source,
        mfa_enabled=account.mfa_enabled,
        permissions=permissions,
        created_at=account.created_at.isoformat() if account.created_at else None,
        last_login_at=account.last_login_at.isoformat() if account.last_login_at else None,
    )
