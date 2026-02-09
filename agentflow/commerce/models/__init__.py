"""AI商取引データモデル.

Schema.org準拠 + AI最適化属性を持つデータモデルを定義。
UCP（Universal Commerce Protocol）互換フォーマット。

設計原則:
- Pydantic v2 による型安全なデータ検証
- Schema.org Product/Offer 準拠
- AI推薦に必要な拡張属性
- 不変性（Immutable）を推奨
"""

from agentflow.commerce.models.cart import Cart, CartItem
from agentflow.commerce.models.intent import IntentConfidence, IntentType, PurchaseIntent
from agentflow.commerce.models.offer import DirectOffer, Offer, OfferType
from agentflow.commerce.models.product import Product, ProductCategory
from agentflow.commerce.models.transaction import PaymentMethod, Transaction, TransactionStatus


__all__ = [
    # カート
    "Cart",
    "CartItem",
    "DirectOffer",
    "IntentConfidence",
    "IntentType",
    # オファー
    "Offer",
    "OfferType",
    "PaymentMethod",
    # 商品
    "Product",
    "ProductCategory",
    # 意図
    "PurchaseIntent",
    # 取引
    "Transaction",
    "TransactionStatus",
]

