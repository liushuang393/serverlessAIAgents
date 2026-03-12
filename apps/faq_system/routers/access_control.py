"""コレクション別アクセス制御 API."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth, require_role
from apps.faq_system.backend.security.permission_config import KBPermission, PermissionConfig
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any


router = APIRouter(tags=["AccessControl"])

# ---------------------------------------------------------------------------
# シングルトン PermissionConfig
# ---------------------------------------------------------------------------

_permission_config: PermissionConfig | None = None


def _get_permission_config() -> PermissionConfig:
    global _permission_config
    if _permission_config is None:
        _permission_config = PermissionConfig()
    return _permission_config


# ---------------------------------------------------------------------------
# リクエスト / レスポンスモデル
# ---------------------------------------------------------------------------


class UpdateCollectionRolesRequest(BaseModel):
    """コレクションのアクセス可能ロール更新リクエスト."""

    allowed_roles: list[str] = Field(..., description="アクセス許可ロール一覧")


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------

# マトリクス表示対象の KB タイプ
_KB_TYPES = ("internal", "external", "confidential")


def _build_matrix(config: PermissionConfig) -> dict[str, dict[str, bool]]:
    """ロール x KB タイプのアクセスマトリクスを生成."""
    matrix: dict[str, dict[str, bool]] = {}
    for role_name in config.list_roles():
        role_perms = config.get_role_permissions(role_name)
        if role_perms is None:
            continue
        row: dict[str, bool] = {}
        for kb_type in _KB_TYPES:
            try:
                perm = KBPermission(f"{kb_type}:read")
                row[kb_type] = perm in role_perms.kb_permissions
            except ValueError:
                row[kb_type] = False
        matrix[role_name] = row
    return matrix


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.get("/api/access/matrix")
async def get_access_matrix(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ロール x KB タイプのアクセスマトリクスを返す（認証必須）."""
    config = _get_permission_config()
    matrix = _build_matrix(config)
    return {"matrix": matrix}


@router.patch("/api/access/collections/{collection_name}/roles")
async def update_collection_roles(
    collection_name: str,
    request: UpdateCollectionRolesRequest,
    _user: UserInfo = Depends(require_role("admin")),
) -> dict[str, Any]:
    """コレクションのアクセス可能ロールを更新（admin のみ）."""
    config = _get_permission_config()

    # collection_name が既知の KB タイプか検証
    if collection_name not in _KB_TYPES:
        raise HTTPException(
            status_code=400,
            detail=f"Unknown KB type: {collection_name}. Valid types: {', '.join(_KB_TYPES)}",
        )

    # 各ロールの kb_permissions を更新
    read_perm = KBPermission(f"{collection_name}:read")
    for role_name in config.list_roles():
        role_perms = config.get_role_permissions(role_name)
        if role_perms is None:
            continue
        if role_name in request.allowed_roles:
            # 権限を付与
            if read_perm not in role_perms.kb_permissions:
                role_perms.kb_permissions.append(read_perm)
        else:
            # 権限を削除
            if read_perm in role_perms.kb_permissions:
                role_perms.kb_permissions.remove(read_perm)

    matrix = _build_matrix(config)
    return {"updated": True, "collection_name": collection_name, "matrix": matrix}
