# -*- coding: utf-8 -*-
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

from agentflow.commerce.interfaces.core import (
    IProduct,
    IOffer,
    ICart,
    ITransaction,
    IPayment,
)

from agentflow.commerce.interfaces.agents import (
    IIntentAnalyzer,
    IOfferProvider,
    IDealRecommender,
)

from agentflow.commerce.interfaces.flow import (
    ICommerceFlow,
    ICommerceStep,
)

from agentflow.commerce.interfaces.ai import (
    ICommerceAI,
)

__all__ = [
    # コアインターフェース
    "IProduct",
    "IOffer",
    "ICart",
    "ITransaction",
    "IPayment",
    # エージェントインターフェース
    "IIntentAnalyzer",
    "IOfferProvider",
    "IDealRecommender",
    # フローインターフェース
    "ICommerceFlow",
    "ICommerceStep",
    # AIインターフェース
    "ICommerceAI",
]

