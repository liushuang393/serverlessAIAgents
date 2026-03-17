"""オファーデータモデル - 意図広告（Direct Offers）対応.

Google Gemini の意図広告に対応したオファーデータ構造。
ユーザーの購買意図に基づいて最適なオファーを提示。

参考:
- https://schema.org/Offer
- Google Direct Offers (意図広告)
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class OfferType(str, Enum):
    """オファータイプ."""

    DISCOUNT = "discount"           # 割引
    COUPON = "coupon"               # クーポン
    BUNDLE = "bundle"               # バンドル
    FREE_SHIPPING = "free_shipping" # 送料無料
    CASHBACK = "cashback"           # キャッシュバック
    LOYALTY = "loyalty"             # ロイヤリティポイント
    FLASH_SALE = "flash_sale"       # タイムセール
    FIRST_PURCHASE = "first_purchase"  # 初回購入特典


class Offer(BaseModel):
    """オファーデータモデル.

    Schema.org Offer 準拠 + 意図広告用拡張。

    Attributes:
        offer_id: オファー一意識別子
        product_id: 対象商品ID
        offer_type: オファータイプ
        discount_value: 割引値（金額または割合）
        discount_type: 割引タイプ（amount/percent）
        conditions: 適用条件
        valid_from: 有効開始日時
        valid_until: 有効終了日時
        intent_match_score: 意図マッチスコア
    """

    offer_id: str = Field(..., description="オファー一意識別子")
    product_id: str | None = Field(default=None, description="対象商品ID")
    offer_type: OfferType = Field(..., description="オファータイプ")

    # 割引情報
    discount_value: float = Field(default=0.0, ge=0, description="割引値")
    discount_type: str = Field(default="percent", description="割引タイプ")
    original_price: float | None = Field(default=None, ge=0, description="元価格")
    final_price: float | None = Field(default=None, ge=0, description="最終価格")

    # 条件
    conditions: dict[str, Any] = Field(
        default_factory=dict, description="適用条件"
    )
    min_purchase_amount: float | None = Field(
        default=None, ge=0, description="最低購入金額"
    )
    max_uses: int | None = Field(default=None, ge=1, description="最大使用回数")

    # 有効期間
    valid_from: datetime | None = Field(default=None, description="有効開始日時")
    valid_until: datetime | None = Field(default=None, description="有効終了日時")

    # 意図広告用属性
    intent_match_score: float = Field(
        default=0.0, ge=0.0, le=1.0, description="意図マッチスコア"
    )
    target_intents: list[str] = Field(
        default_factory=list, description="ターゲット意図キーワード"
    )

    # メタ情報
    description: str = Field(default="", description="オファー説明")
    terms: str = Field(default="", description="利用規約")

    model_config = {"frozen": False, "extra": "allow"}

    def is_valid(self) -> bool:
        """オファーが有効かどうかを判定.

        Returns:
            有効な場合True
        """
        now = datetime.now()
        if self.valid_from and now < self.valid_from:
            return False
        return not (self.valid_until and now > self.valid_until)

    def calculate_discount(self, price: float) -> float:
        """割引額を計算.

        Args:
            price: 元価格

        Returns:
            割引額
        """
        if self.discount_type == "percent":
            return price * (self.discount_value / 100)
        return min(self.discount_value, price)


class DirectOffer(BaseModel):
    """意図広告（Direct Offer）モデル.

    Google Gemini の意図広告に対応。
    ユーザーの購買意図が高まったタイミングで提示される最適オファー。

    Attributes:
        offer: 基本オファー情報
        intent_context: 意図コンテキスト
        presentation_timing: 提示タイミング
        ai_generated: AI生成フラグ
    """

    offer: Offer = Field(..., description="基本オファー情報")
    intent_context: dict[str, Any] = Field(
        default_factory=dict, description="意図コンテキスト"
    )
    presentation_timing: str = Field(
        default="immediate", description="提示タイミング"
    )
    ai_generated: bool = Field(default=False, description="AI生成フラグ")
    confidence: float = Field(
        default=0.0, ge=0.0, le=1.0, description="推薦信頼度"
    )

    # 表示情報
    headline: str = Field(default="", description="見出し")
    call_to_action: str = Field(default="今すぐ購入", description="CTA文言")
    urgency_message: str | None = Field(default=None, description="緊急性メッセージ")

    model_config = {"frozen": False, "extra": "allow"}

