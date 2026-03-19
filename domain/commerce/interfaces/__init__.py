"""Commerce domain interfaces (contracts から re-export).

正規配置は contracts/protocol/commerce/interfaces/。
domain 層の後方互換のため re-export する。
"""

from contracts.protocol.commerce.interfaces import (
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
)


__all__ = [
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
]
