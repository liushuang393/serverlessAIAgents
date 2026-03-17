"""取引データモデル.

AI商取引向け取引管理データ構造。
UCP（Universal Commerce Protocol）互換。
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class TransactionStatus(str, Enum):
    """取引ステータス."""

    PENDING = "pending"           # 保留中
    PROCESSING = "processing"     # 処理中
    COMPLETED = "completed"       # 完了
    FAILED = "failed"             # 失敗
    CANCELLED = "cancelled"       # キャンセル
    REFUNDED = "refunded"         # 返金済み
    PARTIALLY_REFUNDED = "partially_refunded"  # 一部返金


class PaymentMethod(str, Enum):
    """支払い方法."""

    CREDIT_CARD = "credit_card"
    DEBIT_CARD = "debit_card"
    BANK_TRANSFER = "bank_transfer"
    DIGITAL_WALLET = "digital_wallet"
    CRYPTOCURRENCY = "cryptocurrency"
    BUY_NOW_PAY_LATER = "buy_now_pay_later"
    CASH_ON_DELIVERY = "cash_on_delivery"


class Transaction(BaseModel):
    """取引モデル.

    AI商取引向け取引管理。
    UCP（Universal Commerce Protocol）互換。

    Attributes:
        transaction_id: 取引一意識別子
        cart_id: カートID
        user_id: ユーザーID
        status: 取引ステータス
        total_amount: 合計金額
        payment_method: 支払い方法
    """

    transaction_id: str = Field(..., description="取引一意識別子")
    cart_id: str = Field(..., description="カートID")
    user_id: str | None = Field(default=None, description="ユーザーID")

    # ステータス
    status: TransactionStatus = Field(
        default=TransactionStatus.PENDING, description="取引ステータス"
    )

    # 金額
    subtotal: float = Field(..., ge=0, description="商品小計")
    discount_amount: float = Field(default=0.0, ge=0, description="割引額")
    shipping_cost: float = Field(default=0.0, ge=0, description="配送料")
    tax_amount: float = Field(default=0.0, ge=0, description="税額")
    total_amount: float = Field(..., ge=0, description="合計金額")
    currency: str = Field(default="JPY", description="通貨コード")

    # 支払い
    payment_method: PaymentMethod | None = Field(
        default=None, description="支払い方法"
    )
    payment_id: str | None = Field(default=None, description="決済ID")
    payment_provider: str | None = Field(default=None, description="決済プロバイダー")

    # 配送
    shipping_address: dict[str, Any] = Field(
        default_factory=dict, description="配送先住所"
    )
    billing_address: dict[str, Any] = Field(
        default_factory=dict, description="請求先住所"
    )

    # 適用オファー
    applied_offers: list[str] = Field(
        default_factory=list, description="適用オファーID一覧"
    )

    # タイムスタンプ
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")
    updated_at: datetime = Field(default_factory=datetime.now, description="更新日時")
    completed_at: datetime | None = Field(default=None, description="完了日時")

    # AI属性
    ai_assisted: bool = Field(default=False, description="AI支援フラグ")
    intent_id: str | None = Field(default=None, description="関連意図ID")
    recommendation_source: str | None = Field(
        default=None, description="推薦ソース"
    )

    # UCP互換
    ucp_metadata: dict[str, Any] = Field(
        default_factory=dict, description="UCP標準メタデータ"
    )

    model_config = {"frozen": False, "extra": "allow"}

    def complete(self) -> None:
        """取引を完了."""
        self.status = TransactionStatus.COMPLETED
        self.completed_at = datetime.now()
        self.updated_at = datetime.now()

    def cancel(self) -> None:
        """取引をキャンセル."""
        self.status = TransactionStatus.CANCELLED
        self.updated_at = datetime.now()

    def fail(self, reason: str | None = None) -> None:
        """取引を失敗にする.

        Args:
            reason: 失敗理由
        """
        self.status = TransactionStatus.FAILED
        self.updated_at = datetime.now()
        if reason:
            self.ucp_metadata["failure_reason"] = reason

    def to_ucp(self) -> dict[str, Any]:
        """UCP形式に変換.

        Returns:
            UCP準拠の辞書
        """
        return {
            "id": self.transaction_id,
            "cart_id": self.cart_id,
            "user_id": self.user_id,
            "status": self.status.value,
            "amounts": {
                "subtotal": self.subtotal,
                "discount": self.discount_amount,
                "shipping": self.shipping_cost,
                "tax": self.tax_amount,
                "total": self.total_amount,
                "currency": self.currency,
            },
            "payment": {
                "method": self.payment_method.value if self.payment_method else None,
                "id": self.payment_id,
                "provider": self.payment_provider,
            },
            "ai_attributes": {
                "assisted": self.ai_assisted,
                "intent_id": self.intent_id,
                "recommendation_source": self.recommendation_source,
            },
            "metadata": self.ucp_metadata,
        }

