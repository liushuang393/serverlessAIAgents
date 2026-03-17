"""AI商取引データモデル（shared 正本）.

Schema.org準拠 + AI最適化属性を持つデータモデル。
UCP（Universal Commerce Protocol）互換フォーマット。
"""

from contracts.protocol.commerce.models.cart import Cart, CartItem
from contracts.protocol.commerce.models.intent import IntentConfidence, IntentType, PurchaseIntent
from contracts.protocol.commerce.models.offer import DirectOffer, Offer, OfferType
from contracts.protocol.commerce.models.product import Product, ProductCategory
from contracts.protocol.commerce.models.transaction import (
    PaymentMethod,
    Transaction,
    TransactionStatus,
)

__all__ = [
    "Cart",
    "CartItem",
    "DirectOffer",
    "IntentConfidence",
    "IntentType",
    "Offer",
    "OfferType",
    "PaymentMethod",
    "Product",
    "ProductCategory",
    "PurchaseIntent",
    "Transaction",
    "TransactionStatus",
]

