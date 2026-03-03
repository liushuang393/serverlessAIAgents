"""認可 API ルーター.

ロール・パーミッション・リソースパーミッションの CRUD とアクセスチェック。
プレフィックス: /auth/authorization
"""

from __future__ import annotations

import logging
import secrets
from datetime import UTC, datetime
from typing import Any

from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy import delete, select

from apps.auth_service.api.dependencies import require_auth_dependency, require_permission
from apps.auth_service.api.schemas import UserInfo
from apps.auth_service.api.schemas_authorization import (
    AssignPermissionRequest,
    AssignRoleRequest,
    AuthorizationCheckRequest,
    AuthorizationCheckResponse,
    PermissionResponse,
    ResourceCheckRequest,
    ResourceCheckResponse,
    ResourceDefinitionRequest,
    ResourceDefinitionResponse,
    ResourcePermissionRequest,
    ResourcePermissionResponse,
    ResolveScopesResponse,
    RoleCreateRequest,
    RoleResponse,
    RoleUpdateRequest,
    UserPermissionsResponse,
)
from apps.auth_service.core.authorization import AuthorizationService, get_authorization_service
from apps.auth_service.db.session import get_db_session
from apps.auth_service.models.authorization import (
    Permission,
    ResourceDefinition,
    ResourcePermission,
    Role,
    RolePermission,
    UserRole,
)


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/auth/authorization", tags=["認可"])


def _gen_id(prefix: str) -> str:
    return f"{prefix}-{secrets.token_hex(12)}"


# ---------------------------------------------------------------------------
# ロール CRUD
# ---------------------------------------------------------------------------


@router.get("/roles", response_model=list[RoleResponse])
async def list_roles(
    _user: UserInfo = Depends(require_permission("*")),
) -> list[RoleResponse]:
    """ロール一覧を取得."""
    async with get_db_session() as session:
        result = await session.execute(select(Role).order_by(Role.priority.desc()))
        roles = result.scalars().all()

        responses: list[RoleResponse] = []
        for role in roles:
            perm_result = await session.execute(
                select(Permission.name)
                .join(RolePermission, RolePermission.permission_id == Permission.id)
                .where(RolePermission.role_id == role.id)
            )
            perm_names = [row[0] for row in perm_result.all()]
            responses.append(RoleResponse(
                name=role.name,
                display_name=role.display_name,
                description=role.description,
                is_system=role.is_system,
                priority=role.priority,
                permissions=perm_names,
            ))
        return responses


@router.get("/roles/{name}", response_model=RoleResponse)
async def get_role(
    name: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> RoleResponse:
    """ロール詳細を取得."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        perm_result = await session.execute(
            select(Permission.name)
            .join(RolePermission, RolePermission.permission_id == Permission.id)
            .where(RolePermission.role_id == role.id)
        )
        perm_names = [row[0] for row in perm_result.all()]
        return RoleResponse(
            name=role.name,
            display_name=role.display_name,
            description=role.description,
            is_system=role.is_system,
            priority=role.priority,
            permissions=perm_names,
        )


@router.post("/roles", response_model=RoleResponse, status_code=status.HTTP_201_CREATED)
async def create_role(
    req: RoleCreateRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> RoleResponse:
    """ロールを作成."""
    async with get_db_session() as session:
        existing = await session.scalar(select(Role).where(Role.name == req.name))
        if existing is not None:
            raise HTTPException(status_code=409, detail="同名のロールが既に存在します")

        role = Role(
            id=_gen_id("role"),
            name=req.name,
            display_name=req.display_name,
            description=req.description,
            is_system=False,
            priority=req.priority,
        )
        session.add(role)
        await session.commit()

        return RoleResponse(
            name=role.name,
            display_name=role.display_name,
            description=role.description,
            is_system=role.is_system,
            priority=role.priority,
            permissions=[],
        )


@router.put("/roles/{name}", response_model=RoleResponse)
async def update_role(
    name: str,
    req: RoleUpdateRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> RoleResponse:
    """ロールを更新."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")
        if role.is_system:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="システムロールは変更できません",
            )

        if req.display_name is not None:
            role.display_name = req.display_name
        if req.description is not None:
            role.description = req.description
        if req.priority is not None:
            role.priority = req.priority
        role.updated_at = datetime.now(tz=UTC)
        await session.commit()

        perm_result = await session.execute(
            select(Permission.name)
            .join(RolePermission, RolePermission.permission_id == Permission.id)
            .where(RolePermission.role_id == role.id)
        )
        perm_names = [row[0] for row in perm_result.all()]
        return RoleResponse(
            name=role.name,
            display_name=role.display_name,
            description=role.description,
            is_system=role.is_system,
            priority=role.priority,
            permissions=perm_names,
        )


@router.delete("/roles/{name}")
async def delete_role(
    name: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ロールを削除（is_system=False のみ）."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")
        if role.is_system:
            raise HTTPException(status_code=403, detail="システムロールは削除できません")

        await session.delete(role)
        await session.commit()
        return {"message": f"ロール '{name}' を削除しました"}


# ---------------------------------------------------------------------------
# パーミッション一覧
# ---------------------------------------------------------------------------


@router.get("/permissions", response_model=list[PermissionResponse])
async def list_permissions(
    _user: UserInfo = Depends(require_permission("*")),
) -> list[PermissionResponse]:
    """パーミッション一覧を取得."""
    async with get_db_session() as session:
        result = await session.execute(select(Permission).order_by(Permission.name))
        permissions = result.scalars().all()
        return [
            PermissionResponse(
                name=p.name,
                display_name=p.display_name,
                description=p.description,
                resource_type=p.resource_type,
                action=p.action,
                is_system=p.is_system,
            )
            for p in permissions
        ]


# ---------------------------------------------------------------------------
# ロール-パーミッション管理
# ---------------------------------------------------------------------------


@router.post("/roles/{name}/permissions", status_code=status.HTTP_201_CREATED)
async def assign_permission_to_role(
    name: str,
    req: AssignPermissionRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ロールにパーミッションを割り当て."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        perm = await session.scalar(
            select(Permission).where(Permission.name == req.permission_name)
        )
        if perm is None:
            raise HTTPException(status_code=404, detail="パーミッションが見つかりません")

        existing = await session.scalar(
            select(RolePermission).where(
                RolePermission.role_id == role.id,
                RolePermission.permission_id == perm.id,
            )
        )
        if existing is not None:
            raise HTTPException(status_code=409, detail="既に割り当て済みです")

        session.add(RolePermission(
            id=_gen_id("rp"),
            role_id=role.id,
            permission_id=perm.id,
        ))
        await session.commit()

        # キャッシュ無効化
        get_authorization_service().invalidate_cache()
        return {"message": f"パーミッション '{req.permission_name}' をロール '{name}' に割り当てました"}


@router.delete("/roles/{name}/permissions/{perm_name}")
async def remove_permission_from_role(
    name: str,
    perm_name: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ロールからパーミッションを削除."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        perm = await session.scalar(
            select(Permission).where(Permission.name == perm_name)
        )
        if perm is None:
            raise HTTPException(status_code=404, detail="パーミッションが見つかりません")

        result = await session.execute(
            delete(RolePermission).where(
                RolePermission.role_id == role.id,
                RolePermission.permission_id == perm.id,
            )
        )
        if result.rowcount == 0:  # type: ignore[union-attr]
            raise HTTPException(status_code=404, detail="割り当てが見つかりません")

        await session.commit()
        get_authorization_service().invalidate_cache()
        return {"message": f"パーミッション '{perm_name}' をロール '{name}' から削除しました"}


# ---------------------------------------------------------------------------
# ユーザーロール管理
# ---------------------------------------------------------------------------


@router.get("/users/{user_id}/roles")
async def get_user_roles(
    user_id: str,
    _user: UserInfo = Depends(require_permission("users:read")),
) -> dict[str, Any]:
    """ユーザーのロール一覧を取得."""
    authz = get_authorization_service()
    roles = await authz.get_user_roles(user_id)
    return {"user_id": user_id, "roles": roles}


@router.post("/users/{user_id}/roles", status_code=status.HTTP_201_CREATED)
async def assign_role_to_user(
    user_id: str,
    req: AssignRoleRequest,
    current_user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ユーザーにロールを割り当て."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == req.role_name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        existing = await session.scalar(
            select(UserRole).where(
                UserRole.user_id == user_id,
                UserRole.role_id == role.id,
            )
        )
        if existing is not None:
            raise HTTPException(status_code=409, detail="既に割り当て済みです")

        session.add(UserRole(
            id=_gen_id("ur"),
            user_id=user_id,
            role_id=role.id,
            assigned_by=current_user.user_id,
        ))
        await session.commit()

        get_authorization_service().invalidate_cache(user_id)
        return {"message": f"ロール '{req.role_name}' をユーザー '{user_id}' に割り当てました"}


@router.delete("/users/{user_id}/roles/{role_name}")
async def remove_role_from_user(
    user_id: str,
    role_name: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """ユーザーからロールを解除."""
    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == role_name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        result = await session.execute(
            delete(UserRole).where(
                UserRole.user_id == user_id,
                UserRole.role_id == role.id,
            )
        )
        if result.rowcount == 0:  # type: ignore[union-attr]
            raise HTTPException(status_code=404, detail="割り当てが見つかりません")

        await session.commit()
        get_authorization_service().invalidate_cache(user_id)
        return {"message": f"ロール '{role_name}' をユーザー '{user_id}' から解除しました"}


# ---------------------------------------------------------------------------
# ユーザーパーミッション
# ---------------------------------------------------------------------------


@router.get("/users/{user_id}/permissions", response_model=UserPermissionsResponse)
async def get_user_permissions(
    user_id: str,
    _user: UserInfo = Depends(require_permission("users:read")),
) -> UserPermissionsResponse:
    """ユーザーの有効パーミッション一覧を取得."""
    authz = get_authorization_service()
    roles = await authz.get_user_roles(user_id)
    permissions = await authz.get_user_permissions(user_id)
    return UserPermissionsResponse(
        user_id=user_id,
        roles=roles,
        permissions=permissions,
    )


# ---------------------------------------------------------------------------
# 認可チェック
# ---------------------------------------------------------------------------


@router.post("/check", response_model=AuthorizationCheckResponse)
async def check_authorization(
    req: AuthorizationCheckRequest,
    current_user: UserInfo = Depends(require_auth_dependency),
) -> AuthorizationCheckResponse:
    """認可チェック."""
    target_user_id = req.user_id or current_user.user_id
    # 他ユーザーの認可チェックには管理者権限が必要
    if target_user_id != current_user.user_id:
        if not any(
            AuthorizationService.match_permission(p, "*")
            for p in (current_user.permissions or [])
        ):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="他のユーザーの認可チェックには管理者権限が必要です",
            )
    authz = get_authorization_service()
    allowed = await authz.has_permission(target_user_id, req.permission)
    return AuthorizationCheckResponse(
        allowed=allowed,
        permission=req.permission,
        user_id=target_user_id,
    )


@router.post("/check-resource", response_model=ResourceCheckResponse)
async def check_resource_access(
    req: ResourceCheckRequest,
    current_user: UserInfo = Depends(require_auth_dependency),
) -> ResourceCheckResponse:
    """リソースアクセスチェック."""
    target_user_id = req.user_id or current_user.user_id
    # 他ユーザーのリソースチェックには管理者権限が必要
    if target_user_id != current_user.user_id:
        if not any(
            AuthorizationService.match_permission(p, "*")
            for p in (current_user.permissions or [])
        ):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="他のユーザーのリソースチェックには管理者権限が必要です",
            )
    authz = get_authorization_service()
    result = await authz.check_resource_access(
        user_id=target_user_id,
        resource_type=req.resource_type,
        resource_id=req.resource_id,
        required_level=req.required_level,
    )
    return ResourceCheckResponse(
        allowed=result["allowed"],
        reason=result["reason"],
        level=result["level"],
        resource_type=req.resource_type,
        resource_id=req.resource_id,
    )


# ---------------------------------------------------------------------------
# リソースパーミッション CRUD
# ---------------------------------------------------------------------------


@router.get("/resource-permissions", response_model=list[ResourcePermissionResponse])
async def list_resource_permissions(
    _user: UserInfo = Depends(require_permission("*")),
) -> list[ResourcePermissionResponse]:
    """リソースパーミッション一覧を取得."""
    async with get_db_session() as session:
        result = await session.execute(
            select(ResourcePermission, Role.name)
            .join(Role, ResourcePermission.role_id == Role.id)
            .order_by(ResourcePermission.resource_type, ResourcePermission.resource_id)
        )
        rows = result.all()
        return [
            ResourcePermissionResponse(
                id=rp.id,
                role_name=role_name,
                resource_type=rp.resource_type,
                resource_id=rp.resource_id,
                permission_level=rp.permission_level,
                conditions=rp.conditions,
            )
            for rp, role_name in rows
        ]


@router.post(
    "/resource-permissions",
    response_model=ResourcePermissionResponse,
    status_code=status.HTTP_201_CREATED,
)
async def create_resource_permission(
    req: ResourcePermissionRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> ResourcePermissionResponse:
    """リソースパーミッションを作成."""
    valid_levels = {"none", "read", "write", "admin"}
    if req.permission_level not in valid_levels:
        raise HTTPException(
            status_code=400,
            detail=f"無効なパーミッションレベル。有効値: {', '.join(sorted(valid_levels))}",
        )

    async with get_db_session() as session:
        role = await session.scalar(select(Role).where(Role.name == req.role_name))
        if role is None:
            raise HTTPException(status_code=404, detail="ロールが見つかりません")

        rp = ResourcePermission(
            id=_gen_id("rsp"),
            role_id=role.id,
            resource_type=req.resource_type,
            resource_id=req.resource_id,
            permission_level=req.permission_level,
            conditions=req.conditions,
        )
        session.add(rp)
        await session.commit()

        get_authorization_service().invalidate_cache()
        return ResourcePermissionResponse(
            id=rp.id,
            role_name=req.role_name,
            resource_type=rp.resource_type,
            resource_id=rp.resource_id,
            permission_level=rp.permission_level,
            conditions=rp.conditions,
        )


@router.delete("/resource-permissions/{rp_id}")
async def delete_resource_permission(
    rp_id: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """リソースパーミッションを削除."""
    async with get_db_session() as session:
        rp = await session.get(ResourcePermission, rp_id)
        if rp is None:
            raise HTTPException(status_code=404, detail="リソースパーミッションが見つかりません")

        await session.delete(rp)
        await session.commit()

        get_authorization_service().invalidate_cache()
        return {"message": "リソースパーミッションを削除しました"}


# ---------------------------------------------------------------------------
# リソース定義 CRUD
# ---------------------------------------------------------------------------


def _rd_to_response(rd: ResourceDefinition) -> ResourceDefinitionResponse:
    """ResourceDefinition → レスポンス変換."""
    import json as _json

    metadata = None
    if rd.metadata_json:
        try:
            metadata = _json.loads(rd.metadata_json)
        except (ValueError, TypeError):
            metadata = None
    return ResourceDefinitionResponse(
        id=rd.id,
        resource_type=rd.resource_type,
        resource_id=rd.resource_id,
        display_name=rd.display_name,
        app_name=rd.app_name,
        scope=rd.scope,
        backend_key=rd.backend_key,
        metadata=metadata,
        is_active=rd.is_active,
    )


@router.get("/resource-definitions", response_model=list[ResourceDefinitionResponse])
async def list_resource_definitions(
    app_name: str | None = None,
    resource_type: str | None = None,
    _user: UserInfo = Depends(require_auth_dependency),
) -> list[ResourceDefinitionResponse]:
    """リソース定義一覧を取得."""
    async with get_db_session() as session:
        stmt = select(ResourceDefinition).where(ResourceDefinition.is_active.is_(True))
        if app_name:
            stmt = stmt.where(ResourceDefinition.app_name == app_name)
        if resource_type:
            stmt = stmt.where(ResourceDefinition.resource_type == resource_type)
        stmt = stmt.order_by(ResourceDefinition.resource_type, ResourceDefinition.resource_id)
        result = await session.execute(stmt)
        definitions = result.scalars().all()
        return [_rd_to_response(rd) for rd in definitions]


@router.post(
    "/resource-definitions",
    response_model=ResourceDefinitionResponse,
    status_code=status.HTTP_201_CREATED,
)
async def create_resource_definition(
    req: ResourceDefinitionRequest,
    _user: UserInfo = Depends(require_permission("*")),
) -> ResourceDefinitionResponse:
    """リソース定義を作成."""
    import json as _json

    async with get_db_session() as session:
        existing = await session.scalar(
            select(ResourceDefinition).where(
                ResourceDefinition.resource_type == req.resource_type,
                ResourceDefinition.resource_id == req.resource_id,
            )
        )
        if existing is not None:
            raise HTTPException(status_code=409, detail="同一リソース定義が既に存在します")

        rd = ResourceDefinition(
            id=_gen_id("rd"),
            resource_type=req.resource_type,
            resource_id=req.resource_id,
            display_name=req.display_name,
            app_name=req.app_name,
            scope=req.scope,
            backend_key=req.backend_key,
            metadata_json=_json.dumps(req.metadata) if req.metadata else None,
        )
        session.add(rd)
        await session.commit()
        return _rd_to_response(rd)


@router.delete("/resource-definitions/{rd_id}")
async def delete_resource_definition(
    rd_id: str,
    _user: UserInfo = Depends(require_permission("*")),
) -> dict[str, str]:
    """リソース定義を削除."""
    async with get_db_session() as session:
        rd = await session.get(ResourceDefinition, rd_id)
        if rd is None:
            raise HTTPException(status_code=404, detail="リソース定義が見つかりません")
        await session.delete(rd)
        await session.commit()
        return {"message": "リソース定義を削除しました"}


# ---------------------------------------------------------------------------
# スコープ解決
# ---------------------------------------------------------------------------


@router.get("/resolve-scopes", response_model=ResolveScopesResponse)
async def resolve_scopes(
    role: str,
    app_name: str,
    resource_type: str = "vector_db",
    _user: UserInfo = Depends(require_auth_dependency),
) -> ResolveScopesResponse:
    """ロールの許可スコープを解決.

    resource_permissions × resource_definitions を結合し、
    指定ロールがアクセス可能なスコープ + backend 情報を返す。
    """
    authz = get_authorization_service()
    scopes = await authz.resolve_scopes(
        role_name=role, app_name=app_name, resource_type=resource_type
    )
    return ResolveScopesResponse(
        role=role, app_name=app_name, resource_type=resource_type, scopes=scopes
    )
