"""商品データモデル - Schema.org Product 準拠.

AI商取引向けに最適化された商品データ構造。
Schema.org Product スキーマに準拠しつつ、AI推薦に必要な拡張属性を追加。

参考:
- https://schema.org/Product
- Google UCP (Universal Commerce Protocol)
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class ProductCategory(str, Enum):
    """商品カテゴリ."""

    ELECTRONICS = "electronics"
    FASHION = "fashion"
    HOME = "home"
    BEAUTY = "beauty"
    FOOD = "food"
    SPORTS = "sports"
    BOOKS = "books"
    TOYS = "toys"
    AUTOMOTIVE = "automotive"
    OTHER = "other"


class Product(BaseModel):
    """AI商取引向け商品データモデル.

    Schema.org Product 準拠 + AI推薦用拡張属性。
    UCP（Universal Commerce Protocol）互換。

    Attributes:
        product_id: 商品一意識別子
        name: 商品名
        description: 商品説明
        brand: ブランド名
        category: カテゴリ
        price: 価格
        currency: 通貨コード（ISO 4217）
        availability: 在庫状況
        image_urls: 画像URL一覧
        ai_features: AI理解用特徴ベクトル
        intent_keywords: 意図検索キーワード
        recommendation_score: 推薦スコア
        ucp_metadata: UCP標準メタデータ

    Example:
        >>> product = Product(
        ...     product_id="prod-001",
        ...     name="ワイヤレスイヤホン Pro",
        ...     description="高音質ノイズキャンセリング",
        ...     brand="TechBrand",
        ...     category=ProductCategory.ELECTRONICS,
        ...     price=15800.0,
        ...     currency="JPY",
        ... )
    """

    # 基本情報（Schema.org 準拠）
    product_id: str = Field(..., description="商品一意識別子")
    name: str = Field(..., description="商品名", max_length=200)
    description: str = Field(default="", description="商品説明", max_length=5000)
    brand: str = Field(default="", description="ブランド名")
    category: ProductCategory = Field(
        default=ProductCategory.OTHER, description="商品カテゴリ"
    )

    # 価格・在庫
    price: float = Field(..., ge=0, description="価格")
    currency: str = Field(default="JPY", description="通貨コード（ISO 4217）")
    availability: str = Field(default="in_stock", description="在庫状況")
    stock_quantity: int | None = Field(default=None, ge=0, description="在庫数")

    # メディア
    image_urls: list[str] = Field(default_factory=list, description="画像URL一覧")
    video_urls: list[str] = Field(default_factory=list, description="動画URL一覧")

    # 属性
    attributes: dict[str, Any] = Field(
        default_factory=dict, description="商品属性（サイズ、色など）"
    )
    tags: list[str] = Field(default_factory=list, description="タグ一覧")

    # AI推薦用拡張属性
    ai_features: dict[str, Any] = Field(
        default_factory=dict, description="AI理解用特徴（埋め込みベクトル等）"
    )
    intent_keywords: list[str] = Field(
        default_factory=list, description="意図検索キーワード"
    )
    recommendation_score: float = Field(
        default=0.0, ge=0.0, le=1.0, description="推薦スコア"
    )

    # UCP互換フィールド
    ucp_metadata: dict[str, Any] = Field(
        default_factory=dict, description="UCP標準メタデータ"
    )

    # メタ情報
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    updated_at: datetime = Field(default_factory=datetime.now, description="更新日時")

    model_config = {
        "frozen": False,  # 更新可能
        "extra": "allow",  # 追加フィールド許可
    }

    def to_schema_org(self) -> dict[str, Any]:
        """Schema.org Product 形式に変換.

        Returns:
            Schema.org 準拠の辞書
        """
        return {
            "@context": "https://schema.org",
            "@type": "Product",
            "productID": self.product_id,
            "name": self.name,
            "description": self.description,
            "brand": {"@type": "Brand", "name": self.brand} if self.brand else None,
            "category": self.category.value,
            "offers": {
                "@type": "Offer",
                "price": self.price,
                "priceCurrency": self.currency,
                "availability": f"https://schema.org/{self.availability}",
            },
            "image": self.image_urls,
        }

    def to_ucp(self) -> dict[str, Any]:
        """UCP（Universal Commerce Protocol）形式に変換.

        Returns:
            UCP 準拠の辞書
        """
        return {
            "id": self.product_id,
            "name": self.name,
            "description": self.description,
            "brand": self.brand,
            "category": self.category.value,
            "price": {"amount": self.price, "currency": self.currency},
            "availability": self.availability,
            "ai_attributes": {
                "features": self.ai_features,
                "intent_keywords": self.intent_keywords,
                "recommendation_score": self.recommendation_score,
            },
            "metadata": self.ucp_metadata,
        }

