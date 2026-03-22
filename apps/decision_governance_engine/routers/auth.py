import logging
from datetime import datetime
from typing import Any

from fastapi import APIRouter, Cookie, Depends, HTTPException, Response, status
from pydantic import BaseModel, Field

from apps.decision_governance_engine.utils.auth_client import AuthClient


logger = logging.getLogger("decision_api.auth")

router = APIRouter(prefix="/api/auth", tags=["認証"])

# AuthClient の初期化
auth_client = AuthClient()

# ========================================
# スキーマ定義
# ========================================


class LoginRequest(BaseModel):
    """ログインリクエスト."""

    username: str = Field(..., min_length=1, max_length=100, description="ユーザー名")
    password: str = Field(..., min_length=1, max_length=100, description="パスワード")


class UserInfo(BaseModel):
    """ユーザー情報."""

    user_id: str
    username: str
    display_name: str
    department: str = ""
    position: str = ""
    created_at: str = ""


class AuthResponse(BaseModel):
    """認証レスポンス."""

    success: bool
    message: str
    user: UserInfo | None = None


# ========================================
# 依存関係
# ========================================


async def get_current_user(
    session_token: str | None = Cookie(None, alias="session_token"),
) -> UserInfo | None:
    """現在のユーザーを取得 (Auth Service 経由)."""
    if not session_token:
        return None

    result = await auth_client.get_me(session_token)
    if not result.get("success") or "user" not in result:
        return None

    u = result["user"]
    return UserInfo(
        user_id=u.get("id") or u.get("user_id"),
        username=u["username"],
        display_name=u.get("display_name", u["username"]),
        department=u.get("department", ""),
        position=u.get("position", ""),
        created_at=u.get("created_at", ""),
    )


def require_auth(user: UserInfo | None = Depends(get_current_user)) -> UserInfo:
    """認証必須の依存関係."""
    if not user:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="認証が必要です")
    return user


# ========================================
# エンドポイント
# ========================================


@router.post("/login", response_model=AuthResponse)
async def login(req: LoginRequest, response: Response) -> AuthResponse:
    """ログイン処理 (Auth Service へプロキシ)."""
    result = await auth_client.login(req.username, req.password)

    if not result.get("success"):
        return AuthResponse(success=False, message=result.get("message", "ログインに失敗しました"))

    # Auth Service から返されたユーザー情報
    u = result["user"]
    session_token = result.get("session_token")

    user = UserInfo(
        user_id=u.get("id") or u.get("user_id"),
        username=u["username"],
        display_name=u.get("display_name", u["username"]),
        department=u.get("department", ""),
        position=u.get("position", ""),
        created_at=u.get("created_at", datetime.now().isoformat()),
    )

    # Cookie設定 (Auth Service のトークンをそのまま利用)
    if session_token:
        response.set_cookie(
            key="session_token",
            value=session_token,
            httponly=True,
            samesite="lax",
            max_age=86400 * 7,  # 7日間
        )

    logger.info(f"User logged in via Auth Service: {user.username}")
    return AuthResponse(success=True, message="ログイン成功", user=user)


@router.post("/logout")
async def logout(
    response: Response,
    session_token: str | None = Cookie(None, alias="session_token"),
) -> dict[str, Any]:
    """ログアウト処理."""
    if session_token:
        await auth_client.logout(session_token)

    response.delete_cookie(key="session_token")
    return {"success": True, "message": "ログアウトしました"}


@router.get("/me", response_model=AuthResponse)
async def get_current_user_info(
    user: UserInfo | None = Depends(get_current_user),
) -> AuthResponse:
    """現在のユーザー情報を取得."""
    if not user:
        return AuthResponse(success=False, message="未認証", user=None)
    return AuthResponse(success=True, message="認証済み", user=user)
