"""AI商取引抽象インターフェース.

高凝集・低結合の原則に基づいた抽象インターフェース定義。
依存性注入（DI）パターンで実装を差し替え可能。

設計原則:
- 単一責任原則（SRP）: 各インターフェースは1つの責任のみ
- インターフェース分離原則（ISP）: 必要最小限のメソッドのみ定義
- 依存性逆転原則（DIP）: 具象ではなく抽象に依存

インターフェース分類:
1. コアインターフェース: IProduct, IOffer, ICart, ITransaction, IPayment
2. エージェントインターフェース: IIntentAnalyzer, IOfferProvider, IDealRecommender
3. フローインターフェース: ICommerceFlow, ICommerceStep
4. AIインターフェース: ICommerceAI
"""

from agentflow.commerce.interfaces.agents import (
    IDealRecommender,
    IIntentAnalyzer,
    IOfferProvider,
)
from agentflow.commerce.interfaces.ai import (
    ICommerceAI,
)
from agentflow.commerce.interfaces.core import (
    ICart,
    IOffer,
    IPayment,
    IProduct,
    ITransaction,
)
from agentflow.commerce.interfaces.flow import (
    ICommerceFlow,
    ICommerceStep,
)


__all__ = [
    "ICart",
    # AIインターフェース
    "ICommerceAI",
    # フローインターフェース
    "ICommerceFlow",
    "ICommerceStep",
    "IDealRecommender",
    # エージェントインターフェース
    "IIntentAnalyzer",
    "IOffer",
    "IOfferProvider",
    "IPayment",
    # コアインターフェース
    "IProduct",
    "ITransaction",
]

