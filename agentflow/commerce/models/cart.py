"""カートデータモデル.

AI商取引向けカート管理データ構造。
UCP（Universal Commerce Protocol）互換。
"""

from __future__ import annotations

from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field, computed_field


class CartItem(BaseModel):
    """カートアイテムモデル.

    Attributes:
        product_id: 商品ID
        quantity: 数量
        unit_price: 単価
        applied_offers: 適用オファーID一覧
    """

    product_id: str = Field(..., description="商品ID")
    product_name: str = Field(default="", description="商品名")
    quantity: int = Field(default=1, ge=1, description="数量")
    unit_price: float = Field(..., ge=0, description="単価")
    currency: str = Field(default="JPY", description="通貨コード")

    # オファー適用
    applied_offers: list[str] = Field(
        default_factory=list, description="適用オファーID一覧"
    )
    discount_amount: float = Field(default=0.0, ge=0, description="割引額")

    # メタ情報
    attributes: dict[str, Any] = Field(
        default_factory=dict, description="商品属性（サイズ、色など）"
    )
    added_at: datetime = Field(default_factory=datetime.now, description="追加日時")

    model_config = {"frozen": False, "extra": "allow"}

    @computed_field  # type: ignore[prop-decorator]
    @property
    def subtotal(self) -> float:
        """小計を計算.

        Returns:
            小計（割引適用後）
        """
        return (self.unit_price * self.quantity) - self.discount_amount


class Cart(BaseModel):
    """カートモデル.

    AI商取引向けカート管理。
    UCP（Universal Commerce Protocol）互換。

    Attributes:
        cart_id: カート一意識別子
        user_id: ユーザーID
        items: カートアイテム一覧
        applied_offers: カート全体に適用されたオファー
    """

    cart_id: str = Field(..., description="カート一意識別子")
    user_id: str | None = Field(default=None, description="ユーザーID")
    session_id: str | None = Field(default=None, description="セッションID")

    # アイテム
    items: list[CartItem] = Field(default_factory=list, description="カートアイテム一覧")

    # オファー
    applied_offers: list[str] = Field(
        default_factory=list, description="カート全体に適用されたオファーID"
    )
    cart_discount: float = Field(default=0.0, ge=0, description="カート割引額")

    # 配送
    shipping_cost: float = Field(default=0.0, ge=0, description="配送料")
    shipping_method: str | None = Field(default=None, description="配送方法")

    # 通貨
    currency: str = Field(default="JPY", description="通貨コード")

    # メタ情報
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    updated_at: datetime = Field(default_factory=datetime.now, description="更新日時")
    expires_at: datetime | None = Field(default=None, description="有効期限")

    # AI属性
    ai_recommendations: list[str] = Field(
        default_factory=list, description="AI推薦商品ID一覧"
    )

    model_config = {"frozen": False, "extra": "allow"}

    @computed_field  # type: ignore[prop-decorator]
    @property
    def subtotal(self) -> float:
        """商品小計を計算.

        Returns:
            商品小計
        """
        return sum(item.subtotal for item in self.items)

    @computed_field  # type: ignore[prop-decorator]
    @property
    def total(self) -> float:
        """合計金額を計算.

        Returns:
            合計金額（割引・配送料込み）
        """
        return self.subtotal - self.cart_discount + self.shipping_cost

    @computed_field  # type: ignore[prop-decorator]
    @property
    def item_count(self) -> int:
        """アイテム数を計算.

        Returns:
            アイテム数
        """
        return sum(item.quantity for item in self.items)

    def add_item(self, item: CartItem) -> None:
        """アイテムを追加.

        Args:
            item: 追加するアイテム
        """
        # 既存アイテムがあれば数量を加算
        for existing in self.items:
            if existing.product_id == item.product_id:
                existing.quantity += item.quantity
                self.updated_at = datetime.now()
                return
        self.items.append(item)
        self.updated_at = datetime.now()

    def remove_item(self, product_id: str) -> bool:
        """アイテムを削除.

        Args:
            product_id: 削除する商品ID

        Returns:
            削除成功した場合True
        """
        for i, item in enumerate(self.items):
            if item.product_id == product_id:
                self.items.pop(i)
                self.updated_at = datetime.now()
                return True
        return False

    def clear(self) -> None:
        """カートをクリア."""
        self.items.clear()
        self.applied_offers.clear()
        self.cart_discount = 0.0
        self.updated_at = datetime.now()

