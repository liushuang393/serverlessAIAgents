"""Component Schemas - コンポーネントAPI のリクエスト/レスポンスモデル."""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ComponentType(str, Enum):
    """コンポーネントタイプ."""

    AGENT = "agent"
    FLOW = "flow"
    TOOL = "tool"
    SKILL = "skill"
    ENGINE = "engine"
    TEMPLATE = "template"


class ComponentVisibility(str, Enum):
    """コンポーネント可視性."""

    PRIVATE = "private"  # 所有者のみ
    TENANT = "tenant"  # テナント内共有
    PUBLIC = "public"  # 全体公開


class ComponentCreateRequest(BaseModel):
    """コンポーネント作成リクエスト."""

    name: str = Field(..., min_length=1, max_length=100, description="コンポーネント名")
    type: ComponentType = Field(..., description="コンポーネントタイプ")
    version: str = Field(default="1.0.0", description="バージョン")
    description: str = Field(default="", max_length=1000, description="説明")
    category: str = Field(default="general", description="カテゴリ")
    tags: list[str] = Field(default_factory=list, description="タグ")
    visibility: ComponentVisibility = Field(default=ComponentVisibility.PRIVATE, description="可視性")
    source_code: str | None = Field(default=None, description="ソースコード")
    source_path: str | None = Field(default=None, description="ソースパス")
    config: dict[str, Any] = Field(default_factory=dict, description="設定")
    dependencies: list[str] = Field(default_factory=list, description="依存コンポーネント")
    protocols: list[str] = Field(default_factory=list, description="サポートプロトコル")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")


class ComponentUpdateRequest(BaseModel):
    """コンポーネント更新リクエスト."""

    name: str | None = Field(default=None, description="コンポーネント名")
    version: str | None = Field(default=None, description="バージョン")
    description: str | None = Field(default=None, description="説明")
    category: str | None = Field(default=None, description="カテゴリ")
    tags: list[str] | None = Field(default=None, description="タグ")
    visibility: ComponentVisibility | None = Field(default=None, description="可視性")
    source_code: str | None = Field(default=None, description="ソースコード")
    config: dict[str, Any] | None = Field(default=None, description="設定")
    dependencies: list[str] | None = Field(default=None, description="依存コンポーネント")
    protocols: list[str] | None = Field(default=None, description="サポートプロトコル")
    metadata: dict[str, Any] | None = Field(default=None, description="追加メタデータ")


class ComponentResponse(BaseModel):
    """コンポーネントレスポンス."""

    id: str = Field(..., description="一意識別子")
    name: str = Field(..., description="コンポーネント名")
    type: ComponentType = Field(..., description="コンポーネントタイプ")
    version: str = Field(..., description="バージョン")
    description: str = Field(default="", description="説明")
    author: str = Field(default="", description="作成者")
    category: str = Field(default="", description="カテゴリ")
    tags: list[str] = Field(default_factory=list, description="タグ")
    visibility: ComponentVisibility = Field(..., description="可視性")
    tenant_id: str | None = Field(default=None, description="テナントID")
    source_path: str | None = Field(default=None, description="ソースパス")
    config: dict[str, Any] = Field(default_factory=dict, description="設定")
    dependencies: list[str] = Field(default_factory=list, description="依存コンポーネント")
    protocols: list[str] = Field(default_factory=list, description="サポートプロトコル")
    usage_count: int = Field(default=0, description="使用回数")
    created_at: datetime = Field(..., description="作成日時")
    updated_at: datetime = Field(..., description="更新日時")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")


class ComponentListResponse(BaseModel):
    """コンポーネントリストレスポンス."""

    items: list[ComponentResponse] = Field(default_factory=list, description="コンポーネント一覧")
    total: int = Field(default=0, description="総件数")
    limit: int = Field(default=20, description="取得件数")
    offset: int = Field(default=0, description="オフセット")


class ComponentDependencyGraph(BaseModel):
    """コンポーネント依存関係グラフ."""

    component_id: str = Field(..., description="コンポーネントID")
    dependencies: list[str] = Field(default_factory=list, description="直接依存")
    dependents: list[str] = Field(default_factory=list, description="被依存")
    transitive_dependencies: list[str] = Field(default_factory=list, description="推移的依存")
