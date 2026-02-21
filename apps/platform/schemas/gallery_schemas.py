"""Gallery Schemas - Gallery API のリクエスト/レスポンスモデル."""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class GalleryItemType(str, Enum):
    """Galleryアイテムタイプ."""

    AGENT = "agent"
    FLOW = "flow"
    TOOL = "tool"
    SKILL = "skill"
    ENGINE = "engine"
    TEMPLATE = "template"
    APP = "app"


class GalleryFilter(BaseModel):
    """検索フィルター."""

    types: list[GalleryItemType] | None = Field(default=None, description="アイテムタイプフィルター")
    categories: list[str] | None = Field(default=None, description="カテゴリフィルター")
    protocols: list[str] | None = Field(default=None, description="プロトコルフィルター (mcp, a2a, agui, a2ui)")
    authors: list[str] | None = Field(default=None, description="作成者フィルター")
    tags: list[str] | None = Field(default=None, description="タグフィルター")
    min_rating: float | None = Field(default=None, ge=0.0, le=5.0, description="最低評価")
    verified_only: bool = Field(default=False, description="検証済みのみ")
    include_local: bool = Field(default=True, description="ローカルを含む")
    include_marketplace: bool = Field(default=True, description="マーケットプレイスを含む")


class GallerySearchRequest(BaseModel):
    """Gallery検索リクエスト."""

    query: str = Field(..., min_length=1, max_length=200, description="検索クエリ")
    filter: GalleryFilter = Field(default_factory=GalleryFilter, description="フィルター条件")
    limit: int = Field(default=20, ge=1, le=100, description="最大結果数")
    offset: int = Field(default=0, ge=0, description="オフセット")
    sort_by: str = Field(default="relevance", description="ソート項目")
    sort_order: str = Field(default="desc", description="ソート順序 (asc/desc)")


class GalleryItem(BaseModel):
    """Galleryアイテム."""

    id: str = Field(..., description="一意識別子")
    name: str = Field(..., description="表示名")
    type: GalleryItemType = Field(..., description="アイテムタイプ")
    version: str = Field(..., description="バージョン")
    description: str = Field(default="", description="説明")
    author: str = Field(default="", description="作成者")
    category: str = Field(default="", description="カテゴリ")
    tags: list[str] = Field(default_factory=list, description="タグ")
    protocols: list[str] = Field(default_factory=list, description="サポートプロトコル")
    icon: str = Field(default="📦", description="アイコン")
    rating: float = Field(default=0.0, ge=0.0, le=5.0, description="評価")
    downloads: int = Field(default=0, ge=0, description="ダウンロード数")
    verified: bool = Field(default=False, description="検証済みフラグ")
    source: str = Field(default="local", description="ソース (local/marketplace)")
    preview_url: str | None = Field(default=None, description="プレビューURL")
    documentation_url: str | None = Field(default=None, description="ドキュメントURL")
    created_at: datetime | None = Field(default=None, description="作成日時")
    updated_at: datetime | None = Field(default=None, description="更新日時")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")


class GallerySearchResponse(BaseModel):
    """Gallery検索レスポンス."""

    items: list[GalleryItem] = Field(default_factory=list, description="検索結果")
    total: int = Field(default=0, description="総件数")
    limit: int = Field(default=20, description="取得件数")
    offset: int = Field(default=0, description="オフセット")
    query: str = Field(default="", description="検索クエリ")
    has_more: bool = Field(default=False, description="追加結果あり")


class FeaturedItem(BaseModel):
    """推荐アイテム."""

    item: GalleryItem = Field(..., description="Galleryアイテム")
    featured_reason: str = Field(default="", description="推荐理由")
    priority: int = Field(default=0, description="優先度")


class FeaturedResponse(BaseModel):
    """推荐リストレスポンス."""

    featured: list[FeaturedItem] = Field(default_factory=list, description="推荐リスト")
    categories: list[str] = Field(default_factory=list, description="カテゴリ一覧")
    popular_tags: list[str] = Field(default_factory=list, description="人気タグ")
