"""コレクション別アクセス制御 API（読み取り専用）.

role_kb_permissions テーブルからマトリクスを取得。
ロール定義は auth_service 側で管理される。
KB 権限の変更は /api/roles/{role_name}/kb-permissions エンドポイントで行う。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, Depends
from sqlalchemy import select

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.db.models import RoleKBPermission
from apps.faq_system.backend.db.session import get_db_session


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any

logger = logging.getLogger(__name__)

router = APIRouter(tags=["AccessControl"])

# マトリクス表示対象の KB タイプ
_KB_TYPES = ("internal", "external", "confidential")


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _get_permission_config() -> Any:
    """PermissionConfig の遅延取得."""
    from apps.faq_system.backend.security.permission_config import PermissionConfig

    return PermissionConfig()


def _build_matrix(config: Any) -> dict[str, dict[str, bool]]:
    """PermissionConfig からロール x KB タイプのマトリクスを生成."""
    from apps.faq_system.backend.security.permission_config import KBPermission

    matrix: dict[str, dict[str, bool]] = {}
    for role_name in config.list_roles():
        role_perms = config.get_role_permissions(role_name)
        if role_perms is None:
            continue
        row: dict[str, bool] = {}
        for kb_type in _KB_TYPES:
            try:
                permission = KBPermission(f"{kb_type}:read")
                row[kb_type] = permission in role_perms.kb_permissions
            except ValueError:
                row[kb_type] = False
        matrix[role_name] = row
    return matrix


async def _build_matrix_from_db() -> dict[str, dict[str, bool]]:
    """DB からロール × KB タイプのアクセスマトリクスを生成.

    role_kb_permissions テーブルの role_name（auth_service のロール名）を
    キーとして、read 権限の有無をマトリクス化する。
    """
    matrix: dict[str, dict[str, bool]] = {}
    try:
        async with get_db_session() as session:
            result = await session.execute(
                select(RoleKBPermission).where(RoleKBPermission.action == "read"),
            )
            perms = list(result.scalars().all())

            # ロール名ごとにグループ化
            role_kb_map: dict[str, set[str]] = {}
            for p in perms:
                if p.role_name not in role_kb_map:
                    role_kb_map[p.role_name] = set()
                role_kb_map[p.role_name].add(p.kb_type)

            for role_name, kb_types in role_kb_map.items():
                row: dict[str, bool] = {}
                for kb_type in _KB_TYPES:
                    row[kb_type] = kb_type in kb_types
                matrix[role_name] = row
    except Exception:
        logger.warning("DB からロール権限を読み込めません。フォールバック使用。", exc_info=True)
        matrix = _build_matrix(_get_permission_config())
    return matrix


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.get("/api/access/matrix")
async def get_access_matrix(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ロール x KB タイプのアクセスマトリクスを返す（認証必須・読み取り専用）."""
    matrix = await _build_matrix_from_db()
    return {"matrix": matrix}
