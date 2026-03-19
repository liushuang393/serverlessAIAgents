"""Commerce domain public surface (contracts から re-export).

正規配置は contracts/protocol/commerce/。
domain 層の後方互換のため re-export する。
"""

from contracts.protocol.commerce import (
    Cart,
    CartItem,
    DirectOffer,
    FlowContext,
    FlowStatus,
    FlowStepType,
    ICart,
    ICommerceAI,
    ICommerceFlow,
    ICommerceStep,
    IDealRecommender,
    IIntentAnalyzer,
    IOffer,
    IOfferProvider,
    IPayment,
    IProduct,
    ITransaction,
    IntentConfidence,
    IntentType,
    Offer,
    OfferType,
    PaymentMethod,
    Product,
    ProductCategory,
    PurchaseIntent,
    Transaction,
    TransactionStatus,
)


__all__ = [
    # interfaces
    "FlowContext",
    "FlowStatus",
    "FlowStepType",
    "ICart",
    "ICommerceAI",
    "ICommerceFlow",
    "ICommerceStep",
    "IDealRecommender",
    "IIntentAnalyzer",
    "IOffer",
    "IOfferProvider",
    "IPayment",
    "IProduct",
    "ITransaction",
    # models
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
