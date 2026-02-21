"""AI商取引プロバイダー実装.

抽象インターフェースの具体実装を提供。
テスト用モック実装と本番用実装を含む。

設計原則:
- 依存性注入: インターフェースを通じて注入
- 疎結合: 実装の差し替えが容易
- テスト容易性: モック実装でテストを簡素化
"""

from agentflow.commerce.providers.mock import (
    MockCartProvider,
    MockCommerceAI,
    MockDealRecommender,
    MockIntentAnalyzer,
    MockOfferMatcher,
    MockOfferProvider,
    MockPaymentProvider,
    MockProductProvider,
    MockTransactionProvider,
)


__all__ = [
    "MockCartProvider",
    # モックAI
    "MockCommerceAI",
    "MockDealRecommender",
    # モックエージェント
    "MockIntentAnalyzer",
    "MockOfferMatcher",
    "MockOfferProvider",
    "MockPaymentProvider",
    # モックプロバイダー
    "MockProductProvider",
    "MockTransactionProvider",
]
