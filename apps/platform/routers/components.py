"""Components Router - コンポーネント API エンドポイント.

GET /api/components - コンポーネント一覧
POST /api/components - コンポーネント作成
GET /api/components/{component_id} - コンポーネント詳細
PUT /api/components/{component_id} - コンポーネント更新
DELETE /api/components/{component_id} - コンポーネント削除
GET /api/components/{component_id}/dependencies - 依存関係取得
"""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Any

from apps.platform.engine import PlatformEngine
from apps.platform.schemas.component_schemas import (
    ComponentCreateRequest,
    ComponentDependencyGraph,
    ComponentListResponse,
    ComponentResponse,
    ComponentUpdateRequest,
)
from apps.platform.services.component_library import (
    ComponentEntry,
    ComponentType,
)
from fastapi import APIRouter, Depends, HTTPException, Query


router = APIRouter(prefix="/api/components", tags=["components"])

# 依存性注入用のエンジンインスタンス
_engine: PlatformEngine | None = None


def get_engine() -> PlatformEngine:
    """Platform エンジンを取得."""
    global _engine
    if _engine is None:
        _engine = PlatformEngine()
    return _engine


def _entry_to_response(entry: ComponentEntry) -> ComponentResponse:
    """ComponentEntry を ComponentResponse に変換."""
    return ComponentResponse(
        id=entry.id,
        name=entry.name,
        type=entry.type,
        version=entry.version,
        description=entry.description,
        author=entry.author,
        category=entry.category,
        tags=entry.tags,
        visibility=entry.visibility,
        tenant_id=entry.tenant_id,
        source_path=entry.source_path,
        config=entry.config,
        dependencies=entry.dependencies,
        protocols=entry.protocols,
        usage_count=entry.usage_count,
        created_at=entry.created_at,
        updated_at=entry.updated_at,
        metadata=entry.metadata,
    )


@router.get("", response_model=ComponentListResponse)
async def list_components(
    query: str = Query(default="", description="検索クエリ"),
    types: list[ComponentType] | None = Query(default=None, description="タイプフィルター"),
    categories: list[str] | None = Query(default=None, description="カテゴリフィルター"),
    tags: list[str] | None = Query(default=None, description="タグフィルター"),
    limit: int = Query(default=20, ge=1, le=100, description="最大取得数"),
    offset: int = Query(default=0, ge=0, description="オフセット"),
    engine: PlatformEngine = Depends(get_engine),
) -> ComponentListResponse:
    """コンポーネント一覧を取得.

    Args:
        query: 検索クエリ
        types: タイプフィルター
        categories: カテゴリフィルター
        tags: タグフィルター
        limit: 最大取得数
        offset: オフセット
        engine: Platform エンジン

    Returns:
        コンポーネントリスト
    """
    entries = engine.search_components(
        query=query,
        types=types,
        categories=categories,
        tags=tags,
        limit=limit + 1,  # has_more 判定用
    )

    # ページネーション
    has_more = len(entries) > limit
    paginated = entries[offset : offset + limit]

    return ComponentListResponse(
        items=[_entry_to_response(e) for e in paginated],
        total=len(entries) - (1 if has_more else 0),
        limit=limit,
        offset=offset,
    )


@router.post("", response_model=ComponentResponse, status_code=201)
async def create_component(
    request: ComponentCreateRequest,
    engine: PlatformEngine = Depends(get_engine),
) -> ComponentResponse:
    """コンポーネントを作成.

    Args:
        request: 作成リクエスト
        engine: Platform エンジン

    Returns:
        作成されたコンポーネント

    Raises:
        HTTPException: 作成失敗時
    """
    # ComponentEntry を作成
    entry = ComponentEntry(
        id=engine._library.generate_id(request.name, request.type),
        name=request.name,
        type=request.type,
        version=request.version,
        description=request.description,
        category=request.category,
        tags=request.tags,
        visibility=request.visibility,
        source_path=request.source_path,
        source_code=request.source_code,
        config=request.config,
        dependencies=request.dependencies,
        protocols=request.protocols,
        metadata=request.metadata,
    )

    try:
        engine.register_component(entry)
        return _entry_to_response(entry)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("/{component_id}", response_model=ComponentResponse)
async def get_component(
    component_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> ComponentResponse:
    """コンポーネント詳細を取得.

    Args:
        component_id: コンポーネントID
        engine: Platform エンジン

    Returns:
        コンポーネント詳細

    Raises:
        HTTPException: コンポーネントが見つからない場合
    """
    entry = engine.get_component(component_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Component not found: {component_id}")
    return _entry_to_response(entry)


@router.put("/{component_id}", response_model=ComponentResponse)
async def update_component(
    component_id: str,
    request: ComponentUpdateRequest,
    engine: PlatformEngine = Depends(get_engine),
) -> ComponentResponse:
    """コンポーネントを更新.

    Args:
        component_id: コンポーネントID
        request: 更新リクエスト
        engine: Platform エンジン

    Returns:
        更新されたコンポーネント

    Raises:
        HTTPException: コンポーネントが見つからない場合
    """
    entry = engine.get_component(component_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Component not found: {component_id}")

    # 更新フィールドを適用
    if request.name is not None:
        entry.name = request.name
    if request.version is not None:
        entry.version = request.version
    if request.description is not None:
        entry.description = request.description
    if request.category is not None:
        entry.category = request.category
    if request.tags is not None:
        entry.tags = request.tags
    if request.visibility is not None:
        entry.visibility = request.visibility
    if request.source_code is not None:
        entry.source_code = request.source_code
    if request.config is not None:
        entry.config = request.config
    if request.dependencies is not None:
        entry.dependencies = request.dependencies
    if request.protocols is not None:
        entry.protocols = request.protocols
    if request.metadata is not None:
        entry.metadata.update(request.metadata)

    entry.updated_at = datetime.now(UTC)

    # 再登録（上書き）
    engine.register_component(entry, overwrite=True)
    return _entry_to_response(entry)


@router.delete("/{component_id}")
async def delete_component(
    component_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """コンポーネントを削除.

    Args:
        component_id: コンポーネントID
        engine: Platform エンジン

    Returns:
        削除結果

    Raises:
        HTTPException: コンポーネントが見つからない場合
    """
    entry = engine.get_component(component_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Component not found: {component_id}")

    # 被依存チェック
    dependents = engine._library.get_dependents(component_id)
    if dependents:
        raise HTTPException(
            status_code=400,
            detail=f"Component is used by: {[d.id for d in dependents]}",
        )

    # 削除
    success = engine._library.unregister(component_id)
    return {"success": success, "component_id": component_id}


@router.get("/{component_id}/dependencies", response_model=ComponentDependencyGraph)
async def get_dependencies(
    component_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> ComponentDependencyGraph:
    """依存関係を取得.

    Args:
        component_id: コンポーネントID
        engine: Platform エンジン

    Returns:
        依存関係グラフ

    Raises:
        HTTPException: コンポーネントが見つからない場合
    """
    entry = engine.get_component(component_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Component not found: {component_id}")

    dependencies = engine._library.get_dependencies(component_id)
    dependents = engine._library.get_dependents(component_id)

    # 推移的依存を計算
    transitive: set[str] = set()
    stack = [d.id for d in dependencies]
    while stack:
        dep_id = stack.pop()
        if dep_id not in transitive:
            transitive.add(dep_id)
            dep_entry = engine.get_component(dep_id)
            if dep_entry:
                stack.extend(dep_entry.dependencies)

    return ComponentDependencyGraph(
        component_id=component_id,
        dependencies=[d.id for d in dependencies],
        dependents=[d.id for d in dependents],
        transitive_dependencies=list(transitive),
    )
