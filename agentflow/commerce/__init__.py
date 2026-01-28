# -*- coding: utf-8 -*-
"""AI商取引（Agentic Commerce）モジュール.

Google AI商取引戦略（UCP、意図広告、AI主導商取引）に対応するための
抽象インターフェースと実装を提供します。

設計原則:
- 高凝集（High Cohesion）: 各インターフェースは単一責任
- 低結合（Low Coupling）: 依存性注入による疎結合設計
- 拡張性: 将来のUCP標準変更に対応可能

モジュール構成:
- interfaces/: 抽象インターフェース定義
- models/: データモデル（Pydantic）
- agents/: エージェント実装
- flows/: フロー定義
- adapters/: 外部サービスアダプター

使用例:
    >>> from agentflow.commerce import (
    ...     IProduct,
    ...     IOffer,
    ...     IIntentAnalyzer,
    ...     CommerceFlow,
    ... )
    >>>
    >>> # 抽象インターフェースを実装
    >>> class MyProductProvider(IProduct):
    ...     async def get_product(self, product_id: str) -> Product:
    ...         ...
"""

from agentflow.commerce.interfaces import (
    # コアインターフェース
    IProduct,
    IOffer,
    ICart,
    ITransaction,
    IPayment,
    # エージェントインターフェース
    IIntentAnalyzer,
    IOfferProvider,
    IDealRecommender,
    # フローインターフェース
    ICommerceFlow,
    ICommerceStep,
    # AIインターフェース
    ICommerceAI,
)

from agentflow.commerce.models import (
    # データモデル
    Product,
    Offer,
    Cart,
    CartItem,
    Transaction,
    PurchaseIntent,
    DirectOffer,
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
    # データモデル
    "Product",
    "Offer",
    "Cart",
    "CartItem",
    "Transaction",
    "PurchaseIntent",
    "DirectOffer",
]

