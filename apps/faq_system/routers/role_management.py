"""KB 権限管理 API（admin 専用・auth_service 連携）.

ロール定義は auth_service 側で管理される。
FAQ システムでは auth_service のロールに対して KB 権限のみを管理する。
全エンドポイントは admin ロール必須。
"""

from __future__ import annotations

import logging
import os
from typing import TYPE_CHECKING, Any

import httpx
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field
from sqlalchemy import delete, select

from apps.faq_system.backend.auth.dependencies import require_role
from apps.faq_system.backend.db.models import RoleKBPermission
from apps.faq_system.backend.db.session import get_db_session


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any

logger = logging.getLogger(__name__)
router = APIRouter(tags=["RoleManagement"])

# 有効な KB タイプとアクション
_VALID_KB_TYPES = {"internal", "external", "confidential"}
_VALID_ACTIONS = {"read", "write"}

# auth_service 未接続時のフォールバックロール一覧
_FALLBACK_ROLES: list[dict[str, Any]] = [
    {"name": "admin", "display_name": "管理者", "description": "システム管理者", "is_system": True},
    {"name": "manager", "display_name": "管理職", "description": "管理職", "is_system": False},
    {"name": "employee", "display_name": "社員", "description": "一般社員", "is_system": False},
    {"name": "analyst", "display_name": "分析者", "description": "データ分析者", "is_system": False},
    {"name": "hr_admin", "display_name": "人事管理者", "description": "人事管理者", "is_system": False},
    {"name": "guest", "display_name": "ゲスト", "description": "未認証ユーザー", "is_system": True},
]


# ---------------------------------------------------------------------------
# リクエスト / レスポンスモデル
# ---------------------------------------------------------------------------


class UpdateKBPermissionsRequest(BaseModel):
    """KB 権限更新リクエスト."""

    kb_permissions: list[dict[str, str]] = Field(
        ...,
        description="KB 権限リスト [{kb_type, action}]",
    )


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _validate_kb_permissions(perms: list[dict[str, str]]) -> list[tuple[str, str]]:
    """KB 権限入力を検証し、タプルリストに変換."""
    validated: list[tuple[str, str]] = []
    for perm in perms:
        kb_type = perm.get("kb_type", "")
        action = perm.get("action", "")
        if kb_type not in _VALID_KB_TYPES:
            msg = f"無効な KB タイプ: {kb_type}。有効値: {', '.join(_VALID_KB_TYPES)}"
            raise HTTPException(status_code=400, detail=msg)
        if action not in _VALID_ACTIONS:
            msg = f"無効なアクション: {action}。有効値: {', '.join(_VALID_ACTIONS)}"
            raise HTTPException(status_code=400, detail=msg)
        validated.append((kb_type, action))
    return validated


async def _fetch_roles_from_auth_service(token: str) -> list[dict[str, Any]]:
    """auth_service からロール一覧を取得。接続不可時はフォールバック."""
    base_url = os.getenv("AUTH_SERVICE_URL", "").rstrip("/")
    if not base_url:
        logger.warning("AUTH_SERVICE_URL 未設定。フォールバックロール使用。")
        return _FALLBACK_ROLES

    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            resp = await client.get(
                f"{base_url}/auth/authorization/roles",
                headers={"Authorization": f"Bearer {token}"},
            )
            if resp.status_code != 200:
                logger.warning(
                    "auth_service ロール取得失敗: status=%d。フォールバック使用。",
                    resp.status_code,
                )
                return _FALLBACK_ROLES
            data = resp.json()
            # auth_service は list[RoleResponse] を返す
            if isinstance(data, list):
                return [dict(r) for r in data]
            return _FALLBACK_ROLES
    except Exception:
        logger.warning("auth_service 接続失敗。フォールバックロール使用。", exc_info=True)
        return _FALLBACK_ROLES


async def _get_kb_perms_for_role(role_name: str) -> list[dict[str, str]]:
    """DB からロールの KB 権限を取得."""
    async with get_db_session() as session:
        result = await session.execute(
            select(RoleKBPermission).where(RoleKBPermission.role_name == role_name),
        )
        return [{"kb_type": p.kb_type, "action": p.action} for p in result.scalars().all()]


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.get("/api/roles")
async def list_roles(
    user: UserInfo = Depends(require_role("admin")),
) -> dict[str, Any]:
    """ロール一覧 + KB 権限を取得（admin のみ）.

    ロール定義は auth_service から取得し、
    各ロールの KB 権限は FAQ ローカル DB から結合して返す。
    """
    # ユーザーのトークンを取得（auth_service 呼び出し用）
    token = getattr(user, "raw_token", "") or ""

    auth_roles = await _fetch_roles_from_auth_service(token)

    # 各ロールに KB 権限を付与
    roles_with_perms: list[dict[str, Any]] = []
    for role in auth_roles:
        role_name = role.get("name", "")
        kb_perms = await _get_kb_perms_for_role(role_name)
        roles_with_perms.append(
            {
                "role_name": role_name,
                "display_name": role.get("display_name", role_name),
                "description": role.get("description", ""),
                "is_system": role.get("is_system", False),
                "kb_permissions": kb_perms,
            }
        )

    return {"roles": roles_with_perms}


@router.patch("/api/roles/{role_name}/kb-permissions")
async def update_kb_permissions(
    role_name: str,
    request: UpdateKBPermissionsRequest,
    _user: UserInfo = Depends(require_role("admin")),
) -> dict[str, Any]:
    """ロールの KB 権限を更新（admin のみ）.

    既存の KB 権限を全削除し、リクエストの内容で再作成する。
    """
    validated_perms = _validate_kb_permissions(request.kb_permissions)

    async with get_db_session() as session:
        # 既存権限を全削除
        await session.execute(
            delete(RoleKBPermission).where(RoleKBPermission.role_name == role_name),
        )
        await session.flush()

        # 新規権限を追加
        for kb_type, action in validated_perms:
            session.add(
                RoleKBPermission(role_name=role_name, kb_type=kb_type, action=action),
            )
        await session.commit()

    # 更新後の権限を返す
    kb_perms = await _get_kb_perms_for_role(role_name)
    return {"updated": True, "role_name": role_name, "kb_permissions": kb_perms}
