"""認可テーブルのデフォルトシード.

ロール（admin/manager/employee）、パーミッション、ロール-パーミッションマッピングを初期化。
既存 UserAccount.role からの自動同期も行う。
"""

from __future__ import annotations

import logging
import secrets
from typing import Any

from shared.auth_service.db.session import get_db_session
from shared.auth_service.models.authorization import (
    Permission,
    ResourceDefinition,
    ResourcePermission,
    Role,
    RolePermission,
    UserRole,
)
from shared.auth_service.models.user import UserAccount
from sqlalchemy import select


logger = logging.getLogger(__name__)

# デフォルトロール定義
DEFAULT_ROLES: list[dict[str, str | int | bool]] = [
    {
        "name": "admin",
        "display_name": "管理者",
        "description": "全権限を持つシステム管理者",
        "is_system": True,
        "priority": 100,
    },
    {
        "name": "manager",
        "display_name": "マネージャー",
        "description": "チーム管理と分析機能を利用可能",
        "is_system": False,
        "priority": 50,
    },
    {
        "name": "employee",
        "display_name": "一般社員",
        "description": "基本的な閲覧権限",
        "is_system": False,
        "priority": 10,
    },
]

# デフォルトパーミッション定義
DEFAULT_PERMISSIONS: list[dict[str, str | bool]] = [
    {"name": "*", "display_name": "全権限", "resource_type": "*", "action": "*", "is_system": True},
    {"name": "users:read", "display_name": "ユーザー閲覧", "resource_type": "users", "action": "read"},
    {"name": "users:write", "display_name": "ユーザー編集", "resource_type": "users", "action": "write"},
    {"name": "users:delete", "display_name": "ユーザー削除", "resource_type": "users", "action": "delete"},
    {"name": "users:admin", "display_name": "ユーザー管理", "resource_type": "users", "action": "admin"},
    {"name": "roles:read", "display_name": "ロール閲覧", "resource_type": "roles", "action": "read"},
    {"name": "roles:write", "display_name": "ロール編集", "resource_type": "roles", "action": "write"},
    {"name": "roles:admin", "display_name": "ロール管理", "resource_type": "roles", "action": "admin"},
    {"name": "faq:read", "display_name": "FAQ 閲覧", "resource_type": "faq", "action": "read"},
    {"name": "faq:write", "display_name": "FAQ 編集", "resource_type": "faq", "action": "write"},
    {"name": "faq:admin", "display_name": "FAQ 管理", "resource_type": "faq", "action": "admin"},
    {"name": "analytics:read", "display_name": "分析閲覧", "resource_type": "analytics", "action": "read"},
    {"name": "analytics:write", "display_name": "分析編集", "resource_type": "analytics", "action": "write"},
    {"name": "system:read", "display_name": "システム閲覧", "resource_type": "system", "action": "read"},
    {"name": "system:admin", "display_name": "システム管理", "resource_type": "system", "action": "admin"},
]

# ロール → パーミッション マッピング
ROLE_PERMISSIONS_MAP: dict[str, list[str]] = {
    "admin": ["*"],
    "manager": [
        "users:read", "roles:read",
        "faq:read", "faq:write",
        "analytics:read", "analytics:write",
        "system:read",
    ],
    "employee": [
        "users:read", "faq:read", "analytics:read",
    ],
}


def _gen_id(prefix: str) -> str:
    """プレフィックス付き一意 ID を生成."""
    return f"{prefix}-{secrets.token_hex(12)}"


async def seed_authorization() -> None:
    """認可テーブルのデフォルトデータをシード（冪等）.

    ロールが1件でも存在すればスキップする。
    """
    async with get_db_session() as session:
        # 既存ロールがあればスキップ
        existing_role = await session.scalar(select(Role.id).limit(1))
        if existing_role is not None:
            logger.debug("認可データが既に存在するためシードをスキップ")
            return

        # ロール作成
        role_map: dict[str, Role] = {}
        for role_def in DEFAULT_ROLES:
            role = Role(
                id=_gen_id("role"),
                name=str(role_def["name"]),
                display_name=str(role_def["display_name"]),
                description=str(role_def.get("description", "")),
                is_system=bool(role_def.get("is_system", False)),
                priority=int(role_def.get("priority", 0)),
            )
            session.add(role)
            role_map[role.name] = role

        # パーミッション作成
        perm_map: dict[str, Permission] = {}
        for perm_def in DEFAULT_PERMISSIONS:
            perm = Permission(
                id=_gen_id("perm"),
                name=str(perm_def["name"]),
                display_name=str(perm_def["display_name"]),
                description=str(perm_def.get("description", "")),
                resource_type=str(perm_def.get("resource_type", "")),
                action=str(perm_def.get("action", "")),
                is_system=bool(perm_def.get("is_system", False)),
            )
            session.add(perm)
            perm_map[perm.name] = perm

        # ロール-パーミッション割り当て
        for role_name, perm_names in ROLE_PERMISSIONS_MAP.items():
            role = role_map.get(role_name)
            if role is None:
                continue
            for perm_name in perm_names:
                perm = perm_map.get(perm_name)
                if perm is None:
                    continue
                session.add(RolePermission(
                    id=_gen_id("rp"),
                    role_id=role.id,
                    permission_id=perm.id,
                ))

        # 既存 UserAccount.role から user_roles を自動同期
        result = await session.execute(select(UserAccount))
        users = result.scalars().all()
        for user in users:
            role = role_map.get(user.role)
            if role is None:
                continue
            session.add(UserRole(
                id=_gen_id("ur"),
                user_id=user.id,
                role_id=role.id,
                assigned_by="system",
            ))

        await session.commit()
        logger.info(
            "認可データをシードしました: ロール=%d, パーミッション=%d",
            len(role_map),
            len(perm_map),
        )


# デフォルト FAQ リソース定義
FAQ_RESOURCE_DEFINITIONS: list[dict[str, Any]] = [
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__common",
        "display_name": "FAQ 共通ナレッジ",
        "app_name": "faq_system",
        "scope": "common",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}", "vector_provider": "qdrant"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__manager",
        "display_name": "FAQ マネージャー専用",
        "app_name": "faq_system",
        "scope": "manager",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__sales",
        "display_name": "FAQ 営業専用",
        "app_name": "faq_system",
        "scope": "sales",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__employee",
        "display_name": "FAQ 一般社員",
        "app_name": "faq_system",
        "scope": "employee",
        "backend_key": "shared",
        "metadata": {"collection_tpl": "{app}__{tenant}__{scope}"},
    },
    {
        "resource_type": "vector_db",
        "resource_id": "faq__default__confidential",
        "display_name": "FAQ 機密",
        "app_name": "faq_system",
        "scope": "confidential",
        "backend_key": "confidential",
        "metadata": {
            "collection_tpl": "{app}__{tenant}__{scope}",
            "vector_provider": "qdrant",
            "vector_url": "http://faq-qdrant-confidential:6333",
        },
    },
]

# ロール → 許可リソース マッピング（resource_permissions 用）
FAQ_ROLE_RESOURCE_MAP: dict[str, list[str]] = {
    "admin": [
        "faq__default__common", "faq__default__manager",
        "faq__default__sales", "faq__default__employee",
        "faq__default__confidential",
    ],
    "manager": ["faq__default__common", "faq__default__manager"],
    "employee": ["faq__default__common", "faq__default__employee"],
}


async def seed_faq_resource_definitions() -> None:
    """FAQ 用リソース定義と resource_permissions をシード（冪等）."""
    import json as _json

    async with get_db_session() as session:
        # 既存チェック
        existing = await session.scalar(
            select(ResourceDefinition.id).where(
                ResourceDefinition.app_name == "faq_system"
            ).limit(1)
        )
        if existing is not None:
            logger.debug("FAQ リソース定義が既に存在するためスキップ")
            return

        # リソース定義作成
        for rd_def in FAQ_RESOURCE_DEFINITIONS:
            rd = ResourceDefinition(
                id=_gen_id("rd"),
                resource_type=rd_def["resource_type"],
                resource_id=rd_def["resource_id"],
                display_name=rd_def["display_name"],
                app_name=rd_def["app_name"],
                scope=rd_def["scope"],
                backend_key=rd_def["backend_key"],
                metadata_json=_json.dumps(rd_def.get("metadata")),
            )
            session.add(rd)

        # resource_permissions 作成
        for role_name, resource_ids in FAQ_ROLE_RESOURCE_MAP.items():
            role = await session.scalar(
                select(Role).where(Role.name == role_name)
            )
            if role is None:
                continue
            for resource_id in resource_ids:
                level = "admin" if role_name == "admin" else "read"
                session.add(ResourcePermission(
                    id=_gen_id("rsp"),
                    role_id=role.id,
                    resource_type="vector_db",
                    resource_id=resource_id,
                    permission_level=level,
                ))

        await session.commit()
        logger.info("FAQ リソース定義をシードしました: %d 件", len(FAQ_RESOURCE_DEFINITIONS))
