"""認証関連APIルーター.

エンドポイント:
    - POST /api/auth/login: ログイン
    - POST /api/auth/logout: ログアウト
    - GET /api/auth/me: 現在のユーザー情報
"""

import hashlib
import logging
import secrets
from datetime import datetime
from typing import Any

from fastapi import APIRouter, Cookie, Depends, Response
from pydantic import BaseModel, Field


logger = logging.getLogger("decision_api.auth")

router = APIRouter(prefix="/api/auth", tags=["認証"])


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
    display_name: str  # 表示名（署名用）
    department: str = ""  # 部署
    position: str = ""  # 役職
    created_at: str = ""


class AuthResponse(BaseModel):
    """認証レスポンス."""

    success: bool
    message: str
    user: UserInfo | None = None


# ========================================
# デモ用ストレージ（本番ではDB/Redis使用）
# ========================================

DEMO_USERS = {
    "admin": {
        "password_hash": hashlib.sha256(b"admin123").hexdigest(),
        "display_name": "管理者 太郎",
        "department": "経営企画部",
        "position": "部長",
    },
    "tanaka": {
        "password_hash": hashlib.sha256(b"tanaka123").hexdigest(),
        "display_name": "田中 一郎",
        "department": "技術開発本部",
        "position": "シニアマネージャー",
    },
    "suzuki": {
        "password_hash": hashlib.sha256(b"suzuki123").hexdigest(),
        "display_name": "鈴木 花子",
        "department": "事業戦略室",
        "position": "リーダー",
    },
    "yamamoto": {
        "password_hash": hashlib.sha256(b"yamamoto123").hexdigest(),
        "display_name": "山本 健太",
        "department": "DX推進部",
        "position": "マネージャー",
    },
}

# セッションストレージ（本番ではRedis等使用）
_sessions: dict[str, UserInfo] = {}


# ========================================
# 依存関係
# ========================================

def verify_password(username: str, password: str) -> bool:
    """パスワードを検証."""
    if username not in DEMO_USERS:
        return False
    stored_hash = DEMO_USERS[username]["password_hash"]
    input_hash = hashlib.sha256(password.encode()).hexdigest()
    return secrets.compare_digest(stored_hash, input_hash)


def get_current_user(
    session_token: str | None = Cookie(None, alias="session_token"),
) -> UserInfo | None:
    """現在のユーザーを取得."""
    if not session_token:
        return None
    return _sessions.get(session_token)


def require_auth(user: UserInfo | None = Depends(get_current_user)) -> UserInfo:
    """認証必須の依存関係."""
    from fastapi import HTTPException
    if not user:
        raise HTTPException(status_code=401, detail="認証が必要です")
    return user


# ========================================
# エンドポイント
# ========================================

@router.post("/login", response_model=AuthResponse)
async def login(req: LoginRequest, response: Response) -> AuthResponse:
    """ログイン処理."""
    if not verify_password(req.username, req.password):
        return AuthResponse(success=False, message="ユーザー名またはパスワードが正しくありません")

    # セッショントークン生成
    session_token = secrets.token_urlsafe(32)
    user_data = DEMO_USERS[req.username]

    user = UserInfo(
        user_id=f"user-{req.username}",
        username=req.username,
        display_name=user_data["display_name"],
        department=user_data["department"],
        position=user_data["position"],
        created_at=datetime.now().isoformat(),
    )

    # セッション保存
    _sessions[session_token] = user

    # Cookie設定
    response.set_cookie(
        key="session_token",
        value=session_token,
        httponly=True,
        samesite="lax",
        max_age=86400 * 7,  # 7日間
    )

    logger.info(f"User logged in: {req.username}")
    return AuthResponse(success=True, message="ログイン成功", user=user)


@router.post("/logout")
async def logout(
    response: Response,
    user: UserInfo | None = Depends(get_current_user),
) -> dict[str, Any]:
    """ログアウト処理."""
    response.delete_cookie(key="session_token")

    if user:
        # セッションストレージから削除
        tokens_to_remove = [k for k, v in _sessions.items() if v.user_id == user.user_id]
        for token in tokens_to_remove:
            _sessions.pop(token, None)
        logger.info(f"User logged out: {user.username}")

    return {"success": True, "message": "ログアウトしました"}


@router.get("/me", response_model=AuthResponse)
async def get_current_user_info(
    user: UserInfo | None = Depends(get_current_user),
) -> AuthResponse:
    """現在のユーザー情報を取得."""
    if not user:
        return AuthResponse(success=False, message="未認証", user=None)
    return AuthResponse(success=True, message="認証済み", user=user)
