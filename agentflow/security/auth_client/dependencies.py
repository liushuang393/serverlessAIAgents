"""auth_client FastAPI 依存関係ヘルパー.

FastAPI の Depends() で使用できる認証依存関係を提供する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from fastapi import Cookie, Depends, Header, HTTPException, status

from agentflow.security.auth_client.client import AuthClient, RemoteUser


if TYPE_CHECKING:
    from collections.abc import Callable


logger = logging.getLogger(__name__)

# モジュールレベルのデフォルト AuthClient（環境変数から設定）
_default_client: AuthClient | None = None


def _get_default_client() -> AuthClient:
    """デフォルト AuthClient を取得（遅延初期化）."""
    global _default_client
    if _default_client is None:
        _default_client = AuthClient()
    return _default_client


def set_default_client(client: AuthClient) -> None:
    """アプリ起動時にデフォルトクライアントを設定.

    Args:
        client: AuthClient インスタンス
    """
    global _default_client
    _default_client = client


async def get_current_user(
    authorization: str | None = Header(None),
    auth_session: str | None = Cookie(None, alias="auth_session"),
) -> RemoteUser | None:
    """現在のユーザーを取得（任意）.

    JWT トークンまたはセッション Cookie からユーザーを解決する。
    未認証の場合は None を返す（エラーなし）。

    Args:
        authorization: Authorization ヘッダー値
        auth_session: セッション Cookie

    Returns:
        RemoteUser または None
    """
    client = _get_default_client()

    if authorization and authorization.startswith("Bearer "):
        token = authorization[7:]
        user = await client.verify_token(token)
        if user is not None:
            return user

    return None


async def require_auth(
    user: RemoteUser | None = Depends(get_current_user),
) -> RemoteUser:
    """認証必須依存関係.

    未認証の場合は 401 エラーを返す。

    Args:
        user: get_current_user の結果

    Returns:
        認証済み RemoteUser

    Raises:
        HTTPException: 未認証の場合（401）
    """
    if user is None:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="認証が必要です",
            headers={"WWW-Authenticate": "Bearer"},
        )
    return user


def require_role(*roles: str) -> Callable[..., Any]:
    """ロール必須依存関係ファクトリ.

    指定したロールのいずれかを持つユーザーのみアクセスを許可する。

    Args:
        roles: 許可するロール名（複数可）

    Returns:
        FastAPI 依存関係関数

    使用例:
        @router.get("/admin")
        async def admin(user=Depends(require_role("admin"))):
            return {"ok": True}

        @router.get("/staff")
        async def staff(user=Depends(require_role("admin", "manager"))):
            return {"ok": True}
    """

    async def _check(user: RemoteUser = Depends(require_auth)) -> RemoteUser:
        if user.role not in roles:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"権限が不足しています。必要なロール: {', '.join(roles)}",
            )
        return user

    return _check
