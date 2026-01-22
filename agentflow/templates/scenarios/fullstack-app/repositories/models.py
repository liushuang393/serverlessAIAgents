# -*- coding: utf-8 -*-
"""{{ app_name }} SQLAlchemy モデル.

全エンティティのベースモデルと共通フィールドを定義。
"""
from datetime import datetime
from typing import Any
from uuid import UUID

from sqlalchemy import DateTime, String, func
from sqlalchemy.dialects.postgresql import UUID as PG_UUID, JSONB
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


class Base(DeclarativeBase):
    """SQLAlchemy ベースクラス.
    
    全モデルはこのクラスを継承する。
    """
    
    # 型マッピング（PostgreSQL 固有型）
    type_annotation_map = {
        dict[str, Any]: JSONB,
    }


class TimestampMixin:
    """タイムスタンプ Mixin.
    
    created_at, updated_at, deleted_at を自動追加。
    """
    
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        server_default=func.current_timestamp(),
        nullable=False,
    )
    
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        server_default=func.current_timestamp(),
        onupdate=func.current_timestamp(),
        nullable=False,
    )
    
    deleted_at: Mapped[datetime | None] = mapped_column(
        DateTime(timezone=True),
        nullable=True,
    )


class UUIDMixin:
    """UUID 主キー Mixin."""
    
    id: Mapped[UUID] = mapped_column(
        PG_UUID(as_uuid=True),
        primary_key=True,
        server_default=func.gen_random_uuid(),
    )


# ========================================
# エンティティモデル例
# ========================================

class ExampleEntity(Base, UUIDMixin, TimestampMixin):
    """サンプルエンティティ.
    
    このモデルを参考に実際のエンティティを定義する。
    不要な場合は削除すること。
    """
    
    __tablename__ = "example_entities"
    
    # 必須フィールド
    name: Mapped[str] = mapped_column(String(255), nullable=False)
    status: Mapped[str] = mapped_column(String(50), nullable=False, default="active")
    
    # オプションフィールド
    description: Mapped[str | None] = mapped_column(String(1000), nullable=True)
    
    # JSONB フィールド（柔軟なメタデータ格納用）
    metadata: Mapped[dict[str, Any] | None] = mapped_column(JSONB, nullable=True)
    
    def __repr__(self) -> str:
        """文字列表現."""
        return f"ExampleEntity(id={self.id}, name={self.name!r}, status={self.status!r})"

