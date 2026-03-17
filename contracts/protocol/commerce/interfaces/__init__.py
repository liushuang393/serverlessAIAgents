"""AI商取引抽象インターフェース（shared 正本）.

高凝集・低結合の原則に基づいた抽象インターフェース定義。
依存性注入（DI）パターンで実装を差し替え可能。
"""

from contracts.protocol.commerce.interfaces.agents import (
    IDealRecommender,
    IIntentAnalyzer,
    IOfferProvider,
)
from contracts.protocol.commerce.interfaces.ai import (
    ICommerceAI,
)
from contracts.protocol.commerce.interfaces.core import (
    ICart,
    IOffer,
    IPayment,
    IProduct,
    ITransaction,
)
from contracts.protocol.commerce.interfaces.flow import (
    ICommerceFlow,
    ICommerceStep,
)

__all__ = [
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

