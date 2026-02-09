"""Gallery Router - Gallery API エンドポイント.

GET /api/gallery/search - 検索
GET /api/gallery/featured - 推荐リスト
GET /api/gallery/{item_id} - アイテム詳細
POST /api/gallery/install - マーケットプレイスからインストール
"""

from __future__ import annotations

from typing import Any

from apps.platform.engine import PlatformEngine
from apps.platform.schemas.gallery_schemas import (
    FeaturedResponse,
    GalleryFilter,
    GalleryItem,
    GalleryItemType,
    GallerySearchRequest,
    GallerySearchResponse,
)
from fastapi import APIRouter, Depends, HTTPException, Query


router = APIRouter(prefix="/api/gallery", tags=["gallery"])

# 依存性注入用のエンジンインスタンス
_engine: PlatformEngine | None = None


def get_engine() -> PlatformEngine:
    """Platform エンジンを取得."""
    global _engine
    if _engine is None:
        _engine = PlatformEngine()
    return _engine


@router.post("/search", response_model=GallerySearchResponse)
async def search_gallery(
    request: GallerySearchRequest,
    engine: PlatformEngine = Depends(get_engine),
) -> GallerySearchResponse:
    """Gallery検索.

    Args:
        request: 検索リクエスト
        engine: Platform エンジン

    Returns:
        検索結果
    """
    return await engine.search_gallery(request)


@router.get("/search", response_model=GallerySearchResponse)
async def search_gallery_get(
    query: str = Query(..., min_length=1, description="検索クエリ"),
    types: list[GalleryItemType] | None = Query(default=None, description="タイプフィルター"),
    categories: list[str] | None = Query(default=None, description="カテゴリフィルター"),
    protocols: list[str] | None = Query(default=None, description="プロトコルフィルター"),
    tags: list[str] | None = Query(default=None, description="タグフィルター"),
    limit: int = Query(default=20, ge=1, le=100, description="最大結果数"),
    offset: int = Query(default=0, ge=0, description="オフセット"),
    sort_by: str = Query(default="relevance", description="ソート項目"),
    sort_order: str = Query(default="desc", description="ソート順序"),
    engine: PlatformEngine = Depends(get_engine),
) -> GallerySearchResponse:
    """Gallery検索（GET）.

    Args:
        query: 検索クエリ
        types: タイプフィルター
        categories: カテゴリフィルター
        protocols: プロトコルフィルター
        tags: タグフィルター
        limit: 最大結果数
        offset: オフセット
        sort_by: ソート項目
        sort_order: ソート順序
        engine: Platform エンジン

    Returns:
        検索結果
    """
    request = GallerySearchRequest(
        query=query,
        filter=GalleryFilter(
            types=types,
            categories=categories,
            protocols=protocols,
            tags=tags,
        ),
        limit=limit,
        offset=offset,
        sort_by=sort_by,
        sort_order=sort_order,
    )
    return await engine.search_gallery(request)


@router.get("/featured", response_model=FeaturedResponse)
async def get_featured(
    engine: PlatformEngine = Depends(get_engine),
) -> FeaturedResponse:
    """推荐リストを取得.

    Args:
        engine: Platform エンジン

    Returns:
        推荐レスポンス
    """
    return await engine.get_featured()


@router.get("/{item_id}", response_model=GalleryItem)
async def get_gallery_item(
    item_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> GalleryItem:
    """Galleryアイテム詳細を取得.

    Args:
        item_id: アイテムID
        engine: Platform エンジン

    Returns:
        Galleryアイテム

    Raises:
        HTTPException: アイテムが見つからない場合
    """
    item = await engine.get_gallery_item(item_id)
    if item is None:
        raise HTTPException(status_code=404, detail=f"Item not found: {item_id}")
    return item


@router.post("/install")
async def install_from_marketplace(
    item_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """マーケットプレイスからインストール.

    Args:
        item_id: アイテムID
        engine: Platform エンジン

    Returns:
        インストール結果

    Raises:
        HTTPException: インストール失敗時
    """
    try:
        entry = await engine.install_from_marketplace(item_id)
        return {
            "success": True,
            "component_id": entry.id,
            "name": entry.name,
            "version": entry.version,
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Installation failed: {e}")
