# -*- coding: utf-8 -*-
"""AI商取引データモデル.

Schema.org準拠 + AI最適化属性を持つデータモデルを定義。
UCP（Universal Commerce Protocol）互換フォーマット。

設計原則:
- Pydantic v2 による型安全なデータ検証
- Schema.org Product/Offer 準拠
- AI推薦に必要な拡張属性
- 不変性（Immutable）を推奨
"""

from agentflow.commerce.models.product import Product, ProductCategory
from agentflow.commerce.models.offer import Offer, OfferType, DirectOffer
from agentflow.commerce.models.cart import Cart, CartItem
from agentflow.commerce.models.transaction import Transaction, TransactionStatus, PaymentMethod
from agentflow.commerce.models.intent import PurchaseIntent, IntentType, IntentConfidence

__all__ = [
    # 商品
    "Product",
    "ProductCategory",
    # オファー
    "Offer",
    "OfferType",
    "DirectOffer",
    # カート
    "Cart",
    "CartItem",
    # 取引
    "Transaction",
    "TransactionStatus",
    "PaymentMethod",
    # 意図
    "PurchaseIntent",
    "IntentType",
    "IntentConfidence",
]

