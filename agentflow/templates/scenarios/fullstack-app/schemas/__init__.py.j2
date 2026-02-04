# -*- coding: utf-8 -*-
"""{{ app_name }} Pydantic スキーマ.

API リクエスト/レスポンスのバリデーションスキーマを定義。
"""
from datetime import datetime
from typing import Any, Generic, TypeVar
from uuid import UUID

from pydantic import BaseModel, Field


# ========================================
# 共通スキーマ
# ========================================

class BaseSchema(BaseModel):
    """ベーススキーマ."""
    
    model_config = {
        "from_attributes": True,  # ORM モード有効化
        "populate_by_name": True,
    }


class TimestampSchema(BaseSchema):
    """タイムスタンプ付きスキーマ."""
    
    created_at: datetime = Field(..., description="作成日時")
    updated_at: datetime = Field(..., description="更新日時")


# ========================================
# ページネーション
# ========================================

T = TypeVar("T")


class PaginatedResponse(BaseModel, Generic[T]):
    """ページネーションレスポンス."""
    
    items: list[T] = Field(..., description="データ一覧")
    total: int = Field(..., description="総件数")
    page: int = Field(..., description="現在ページ")
    page_size: int = Field(..., description="ページサイズ")
    total_pages: int = Field(..., description="総ページ数")
    
    @classmethod
    def create(
        cls,
        items: list[T],
        total: int,
        page: int,
        page_size: int,
    ) -> "PaginatedResponse[T]":
        """ページネーションレスポンスを作成."""
        total_pages = (total + page_size - 1) // page_size if page_size > 0 else 0
        return cls(
            items=items,
            total=total,
            page=page,
            page_size=page_size,
            total_pages=total_pages,
        )


# ========================================
# エラーレスポンス
# ========================================

class ErrorDetail(BaseModel):
    """エラー詳細."""
    
    code: str = Field(..., description="エラーコード")
    message: str = Field(..., description="エラーメッセージ")
    field: str | None = Field(None, description="エラー発生フィールド")


class ErrorResponse(BaseModel):
    """エラーレスポンス."""
    
    error: str = Field(..., description="エラー種別")
    message: str = Field(..., description="エラーメッセージ")
    details: list[ErrorDetail] | None = Field(None, description="詳細エラー")
    request_id: str | None = Field(None, description="リクエスト ID")


# ========================================
# サンプルエンティティスキーマ
# ========================================

class ExampleEntityBase(BaseSchema):
    """サンプルエンティティ基本スキーマ."""
    
    name: str = Field(..., min_length=1, max_length=255, description="名前")
    description: str | None = Field(None, max_length=1000, description="説明")
    metadata: dict[str, Any] | None = Field(None, description="メタデータ")


class ExampleEntityCreate(ExampleEntityBase):
    """サンプルエンティティ作成リクエスト."""
    pass


class ExampleEntityUpdate(BaseSchema):
    """サンプルエンティティ更新リクエスト."""
    
    name: str | None = Field(None, min_length=1, max_length=255, description="名前")
    description: str | None = Field(None, max_length=1000, description="説明")
    metadata: dict[str, Any] | None = Field(None, description="メタデータ")


class ExampleEntityResponse(ExampleEntityBase, TimestampSchema):
    """サンプルエンティティレスポンス."""
    
    id: UUID = Field(..., description="ID")
    status: str = Field(..., description="ステータス")


__all__ = [
    "BaseSchema",
    "TimestampSchema",
    "PaginatedResponse",
    "ErrorDetail",
    "ErrorResponse",
    "ExampleEntityBase",
    "ExampleEntityCreate",
    "ExampleEntityUpdate",
    "ExampleEntityResponse",
]

