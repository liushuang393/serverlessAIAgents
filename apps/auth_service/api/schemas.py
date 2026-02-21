"""auth_service API Pydantic スキーマ定義.

リクエスト・レスポンスモデルを定義する。
"""

from __future__ import annotations

from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# ユーザー情報
# ---------------------------------------------------------------------------


class UserInfo(BaseModel):
    """ユーザー情報レスポンス."""

    user_id: str = Field(..., description="ユーザーID")
    username: str = Field(..., description="ユーザー名")
    display_name: str = Field(..., description="表示名")
    department: str = Field("", description="部署")
    position: str = Field("", description="役職")
    role: str = Field("employee", description="ロール")
    email: str | None = Field(None, description="メールアドレス")
    mfa_enabled: bool = Field(False, description="MFA 有効フラグ")


# ---------------------------------------------------------------------------
# 認証リクエスト
# ---------------------------------------------------------------------------


class LoginRequest(BaseModel):
    """ログインリクエスト."""

    username: str = Field(..., min_length=1, max_length=100, description="ユーザー名")
    password: str = Field(..., min_length=1, max_length=100, description="パスワード")
    totp_code: str | None = Field(None, description="TOTP MFA コード（6桁）")


class RegisterRequest(BaseModel):
    """ユーザー登録リクエスト."""

    username: str = Field(
        ...,
        min_length=1,
        max_length=100,
        pattern=r"^[a-zA-Z0-9_-]+$",
        description="ユーザー名（英数字、_、- のみ）",
    )
    password: str = Field(..., min_length=8, max_length=100, description="パスワード（8文字以上）")
    display_name: str = Field(..., min_length=1, max_length=100, description="表示名")
    department: str | None = Field(None, max_length=100, description="部署")
    position: str | None = Field(None, max_length=100, description="役職")
    email: str | None = Field(None, description="メールアドレス")


class TokenRefreshRequest(BaseModel):
    """トークンリフレッシュリクエスト."""

    refresh_token: str = Field(..., description="リフレッシュトークン")


# ---------------------------------------------------------------------------
# 認証レスポンス
# ---------------------------------------------------------------------------


class AuthResponse(BaseModel):
    """認証レスポンス."""

    success: bool = Field(..., description="成功フラグ")
    message: str = Field(..., description="メッセージ")
    user: UserInfo | None = Field(None, description="ユーザー情報")
    access_token: str | None = Field(None, description="アクセストークン")
    refresh_token: str | None = Field(None, description="リフレッシュトークン")
    token_type: str = Field("bearer", description="トークンタイプ")
    expires_in: int | None = Field(None, description="アクセストークン有効期限（秒）")


class TokenResponse(BaseModel):
    """トークンレスポンス."""

    access_token: str = Field(..., description="アクセストークン")
    refresh_token: str = Field(..., description="リフレッシュトークン")
    token_type: str = Field("bearer", description="トークンタイプ")
    expires_in: int = Field(..., description="有効期限（秒）")


# ---------------------------------------------------------------------------
# パスワード操作
# ---------------------------------------------------------------------------


class ChangePasswordRequest(BaseModel):
    """パスワード変更リクエスト."""

    current_password: str = Field(..., min_length=1, max_length=100, description="現在のパスワード")
    new_password: str = Field(..., min_length=8, max_length=100, description="新しいパスワード")


class ForgotPasswordRequest(BaseModel):
    """パスワード再設定要求リクエスト."""

    username: str = Field(..., min_length=1, max_length=100, description="ユーザー名")


class ResetPasswordRequest(BaseModel):
    """パスワード再設定リクエスト."""

    reset_token: str = Field(..., min_length=1, max_length=256, description="再設定トークン")
    new_password: str = Field(..., min_length=8, max_length=100, description="新しいパスワード")


# ---------------------------------------------------------------------------
# プロフィール
# ---------------------------------------------------------------------------


class ProfileUpdateRequest(BaseModel):
    """プロフィール更新リクエスト."""

    display_name: str | None = Field(None, min_length=1, max_length=100, description="表示名")
    department: str | None = Field(None, max_length=100, description="部署")
    position: str | None = Field(None, max_length=100, description="役職")


# ---------------------------------------------------------------------------
# MFA
# ---------------------------------------------------------------------------


class MfaSetupResponse(BaseModel):
    """MFA 設定開始レスポンス."""

    secret: str = Field(..., description="MFA シークレット")
    uri: str = Field(..., description="OTP Auth URI（QR コード用）")


class MfaVerifyRequest(BaseModel):
    """MFA コード検証リクエスト."""

    totp_code: str = Field(..., min_length=6, max_length=6, description="TOTP コード（6桁）")


# ---------------------------------------------------------------------------
# 汎用
# ---------------------------------------------------------------------------


class MessageResponse(BaseModel):
    """汎用メッセージレスポンス."""

    success: bool = Field(..., description="成功フラグ")
    message: str = Field(..., description="メッセージ")
