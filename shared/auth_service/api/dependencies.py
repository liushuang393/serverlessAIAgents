"""auth_service API 認可依存関係.

JWT の permissions クレームでローカルチェックする FastAPI 依存関係を提供。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from fastapi import Depends, Header, HTTPException, status

from shared.auth_service.core.authorization import AuthorizationService
from shared.auth_service.service import get_auth_service


if TYPE_CHECKING:
    from collections.abc import Callable

    from shared.auth_service.api.schemas import UserInfo

logger = logging.getLogger(__name__)


async def get_current_user_from_token(
    authorization: str | None = Header(None),
) -> UserInfo | None:
    """Authorization ヘッダーからユーザーを取得.

    router.py の get_current_user と同じロジックだが、循環インポートを避けるために
    service 層に直接アクセスする。Cookie 認証はこちらでは省略。
    """
    svc = get_auth_service()
    if authorization:
        user = await svc.verify_access_token(authorization)
        if user is not None:
            return user
    return None


async def require_auth_dependency(
    user: UserInfo | None = Depends(get_current_user_from_token),
) -> UserInfo:
    """認証必須依存関係."""
    if user is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="認証が必要です",
            headers={"WWW-Authenticate": "Bearer"},
        )
    return user


def require_permission(*permissions: str) -> Callable[..., Any]:
    """パーミッション必須依存関係ファクトリ.

    JWT の permissions クレームでローカルチェックを行い、
    マッチしない場合は 403 を返す。

    Args:
        permissions: 必要なパーミッション名（複数可、いずれか1つを持っていれば OK）

    Returns:
        FastAPI 依存関数
    """

    async def _check(user: UserInfo = Depends(require_auth_dependency)) -> UserInfo:
        user_permissions = user.permissions or []
        for required in permissions:
            if any(AuthorizationService.match_permission(held, required) for held in user_permissions):
                return user
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail=f"権限が不足しています。必要なパーミッション: {', '.join(permissions)}",
        )

    return _check


def require_admin() -> Callable[..., Any]:
    """管理者権限必須依存関係.

    require_permission("*") のショートカット。

    Returns:
        FastAPI 依存関数
    """
    return require_permission("*")
