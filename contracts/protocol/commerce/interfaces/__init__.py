"""Commerce domain interfaces (contracts 層の正規配置)."""

from contracts.protocol.commerce.interfaces.agents import IDealRecommender, IIntentAnalyzer, IOfferProvider
from contracts.protocol.commerce.interfaces.ai import ICommerceAI
from contracts.protocol.commerce.interfaces.core import ICart, IOffer, IPayment, IProduct, ITransaction
from contracts.protocol.commerce.interfaces.flow import FlowContext, FlowStatus, FlowStepType, ICommerceFlow, ICommerceStep

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
