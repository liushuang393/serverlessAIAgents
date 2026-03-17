"""認可 API Pydantic スキーマ定義.

ロール・パーミッション・リソースアクセス管理のリクエスト/レスポンスモデル。
"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# ロール
# ---------------------------------------------------------------------------


class RoleResponse(BaseModel):
    """ロール情報レスポンス."""

    name: str = Field(..., description="ロール名")
    display_name: str = Field(..., description="表示名")
    description: str = Field("", description="説明")
    is_system: bool = Field(False, description="システムロールフラグ")
    priority: int = Field(0, description="優先度")
    permissions: list[str] = Field(default_factory=list, description="パーミッション一覧")


class RoleCreateRequest(BaseModel):
    """ロール作成リクエスト."""

    name: str = Field(..., min_length=1, max_length=100, description="ロール名（英数字、_、-）")
    display_name: str = Field(..., min_length=1, max_length=200, description="表示名")
    description: str = Field("", max_length=500, description="説明")
    priority: int = Field(0, ge=0, le=999, description="優先度")


class RoleUpdateRequest(BaseModel):
    """ロール更新リクエスト."""

    display_name: str | None = Field(None, min_length=1, max_length=200, description="表示名")
    description: str | None = Field(None, max_length=500, description="説明")
    priority: int | None = Field(None, ge=0, le=999, description="優先度")


# ---------------------------------------------------------------------------
# パーミッション
# ---------------------------------------------------------------------------


class PermissionResponse(BaseModel):
    """パーミッション情報レスポンス."""

    name: str = Field(..., description="パーミッション名")
    display_name: str = Field(..., description="表示名")
    description: str = Field("", description="説明")
    resource_type: str = Field("", description="リソース種別")
    action: str = Field("", description="アクション")
    is_system: bool = Field(False, description="システムパーミッションフラグ")


# ---------------------------------------------------------------------------
# ロール割り当て
# ---------------------------------------------------------------------------


class AssignRoleRequest(BaseModel):
    """ロール割り当てリクエスト."""

    role_name: str = Field(..., min_length=1, max_length=100, description="割り当てるロール名")


class AssignPermissionRequest(BaseModel):
    """パーミッション割り当てリクエスト."""

    permission_name: str = Field(..., min_length=1, max_length=200, description="パーミッション名")


# ---------------------------------------------------------------------------
# 認可チェック
# ---------------------------------------------------------------------------


class AuthorizationCheckRequest(BaseModel):
    """認可チェックリクエスト."""

    permission: str = Field(..., min_length=1, max_length=200, description="チェックするパーミッション")
    user_id: str | None = Field(None, description="対象ユーザーID（省略時は自分自身）")


class AuthorizationCheckResponse(BaseModel):
    """認可チェックレスポンス."""

    allowed: bool = Field(..., description="許可フラグ")
    permission: str = Field(..., description="チェックしたパーミッション")
    user_id: str = Field(..., description="対象ユーザーID")


class ResourceCheckRequest(BaseModel):
    """リソースアクセスチェックリクエスト."""

    resource_type: str = Field(..., min_length=1, max_length=100, description="リソース種別")
    resource_id: str = Field(..., min_length=1, max_length=200, description="リソースID")
    required_level: str = Field("read", description="要求レベル（none/read/write/admin）")
    user_id: str | None = Field(None, description="対象ユーザーID（省略時は自分自身）")


class ResourceCheckResponse(BaseModel):
    """リソースアクセスチェックレスポンス."""

    allowed: bool = Field(..., description="許可フラグ")
    reason: str = Field(..., description="判定理由")
    level: str = Field(..., description="実際のパーミッションレベル")
    resource_type: str = Field(..., description="リソース種別")
    resource_id: str = Field(..., description="リソースID")


# ---------------------------------------------------------------------------
# リソースパーミッション
# ---------------------------------------------------------------------------


class ResourcePermissionRequest(BaseModel):
    """リソースパーミッション作成リクエスト."""

    role_name: str = Field(..., min_length=1, max_length=100, description="ロール名")
    resource_type: str = Field(..., min_length=1, max_length=100, description="リソース種別")
    resource_id: str = Field(..., min_length=1, max_length=200, description="リソースID")
    permission_level: str = Field("read", description="パーミッションレベル（none/read/write/admin）")
    conditions: str | None = Field(None, description="JSON 条件")


class ResourcePermissionResponse(BaseModel):
    """リソースパーミッション情報レスポンス."""

    id: str = Field(..., description="ID")
    role_name: str = Field(..., description="ロール名")
    resource_type: str = Field(..., description="リソース種別")
    resource_id: str = Field(..., description="リソースID")
    permission_level: str = Field(..., description="パーミッションレベル")
    conditions: str | None = Field(None, description="JSON 条件")


# ---------------------------------------------------------------------------
# ユーザーパーミッション
# ---------------------------------------------------------------------------


class UserPermissionsResponse(BaseModel):
    """ユーザーパーミッション情報レスポンス."""

    user_id: str = Field(..., description="ユーザーID")
    roles: list[str] = Field(default_factory=list, description="ロール一覧")
    permissions: list[str] = Field(default_factory=list, description="パーミッション一覧")


# ---------------------------------------------------------------------------
# 管理 API
# ---------------------------------------------------------------------------


class UserAdminResponse(BaseModel):
    """管理用ユーザー情報レスポンス."""

    user_id: str = Field(..., description="ユーザーID")
    username: str = Field(..., description="ユーザー名")
    display_name: str = Field(..., description="表示名")
    department: str = Field("", description="部署")
    position: str = Field("", description="役職")
    role: str = Field("employee", description="レガシーロール")
    roles: list[str] = Field(default_factory=list, description="ロール一覧")
    email: str | None = Field(None, description="メールアドレス")
    is_active: bool = Field(True, description="有効フラグ")
    auth_source: str = Field("local_db", description="認証ソース")
    mfa_enabled: bool = Field(False, description="MFA 有効フラグ")
    permissions: list[str] = Field(default_factory=list, description="パーミッション一覧")
    created_at: str | None = Field(None, description="作成日時")
    last_login_at: str | None = Field(None, description="最終ログイン日時")


class UserUpdateRequest(BaseModel):
    """管理用ユーザー更新リクエスト."""

    display_name: str | None = Field(None, min_length=1, max_length=100, description="表示名")
    department: str | None = Field(None, max_length=100, description="部署")
    position: str | None = Field(None, max_length=100, description="役職")
    role: str | None = Field(None, max_length=50, description="レガシーロール")
    is_active: bool | None = Field(None, description="有効フラグ")


class AdminResetPasswordRequest(BaseModel):
    """管理者パスワードリセットリクエスト."""

    new_password: str = Field(..., min_length=8, max_length=100, description="新パスワード")


class PaginatedUsersResponse(BaseModel):
    """ページネーション付きユーザー一覧レスポンス."""

    users: list[UserAdminResponse] = Field(..., description="ユーザー一覧")
    total: int = Field(..., description="全件数")
    page: int = Field(..., description="現在のページ")
    page_size: int = Field(..., description="ページサイズ")


# ---------------------------------------------------------------------------
# リソース定義
# ---------------------------------------------------------------------------


class ResourceDefinitionRequest(BaseModel):
    """リソース定義作成/更新リクエスト."""

    resource_type: str = Field(..., min_length=1, max_length=100, description="リソース種別")
    resource_id: str = Field(..., min_length=1, max_length=200, description="リソースID")
    display_name: str = Field(..., min_length=1, max_length=200, description="表示名")
    app_name: str = Field("", max_length=100, description="所属 App 名")
    scope: str = Field("", max_length=100, description="スコープ名")
    backend_key: str = Field("shared", max_length=100, description="バックエンドキー")
    metadata: dict[str, Any] | None = Field(None, description="追加メタデータ（JSON）")


class ResourceDefinitionResponse(BaseModel):
    """リソース定義レスポンス."""

    id: str = Field(..., description="ID")
    resource_type: str = Field(..., description="リソース種別")
    resource_id: str = Field(..., description="リソースID")
    display_name: str = Field(..., description="表示名")
    app_name: str = Field("", description="所属 App 名")
    scope: str = Field("", description="スコープ名")
    backend_key: str = Field("shared", description="バックエンドキー")
    metadata: dict[str, Any] | None = Field(None, description="追加メタデータ")
    is_active: bool = Field(True, description="有効フラグ")


# ---------------------------------------------------------------------------
# スコープ解決
# ---------------------------------------------------------------------------


class ResolveScopesResponse(BaseModel):
    """スコープ解決レスポンス."""

    role: str = Field(..., description="ロール名")
    app_name: str = Field(..., description="App 名")
    resource_type: str = Field("vector_db", description="リソース種別")
    scopes: list[dict[str, Any]] = Field(default_factory=list, description="許可スコープ一覧")
