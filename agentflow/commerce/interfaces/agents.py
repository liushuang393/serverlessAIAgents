"""エージェント抽象インターフェース.

意図分析、オファー提供、取引推薦のエージェントインターフェース。
Google Gemini 意図広告（Direct Offers）対応。

設計原則:
- 単一責任: 各エージェントは1つの責任のみ
- 疎結合: 依存性注入で実装を差し替え可能
- 非同期: すべてのメソッドはasync
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.commerce.models import (
        DirectOffer,
        Offer,
        Product,
        PurchaseIntent,
    )


class IIntentAnalyzer(ABC):
    """購買意図分析インターフェース.

    ユーザー入力から購買意図を抽出・分析。
    意図広告のトリガーとして使用。

    Example:
        >>> class GeminiIntentAnalyzer(IIntentAnalyzer):
        ...     async def analyze(self, user_input: str, context: dict) -> PurchaseIntent:
        ...         # Gemini APIで意図分析
        ...         ...
    """

    @abstractmethod
    async def analyze(
        self,
        user_input: str,
        *,
        conversation_context: list[dict[str, str]] | None = None,
        user_id: str | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析.

        Args:
            user_input: ユーザー入力テキスト
            conversation_context: 会話コンテキスト
            user_id: ユーザーID

        Returns:
            購買意図
        """

    @abstractmethod
    async def extract_entities(
        self,
        user_input: str,
    ) -> list[dict[str, Any]]:
        """エンティティを抽出.

        Args:
            user_input: ユーザー入力テキスト

        Returns:
            抽出エンティティリスト
        """

    @abstractmethod
    async def classify_intent_type(
        self,
        user_input: str,
    ) -> tuple[str, float]:
        """意図タイプを分類.

        Args:
            user_input: ユーザー入力テキスト

        Returns:
            (意図タイプ, 確度スコア)
        """


class IOfferProvider(ABC):
    """オファー提供インターフェース.

    意図に基づいて最適なオファーを提供。
    意図広告（Direct Offers）の中核。

    Example:
        >>> class SmartOfferProvider(IOfferProvider):
        ...     async def get_offers_for_intent(self, intent: PurchaseIntent) -> list[Offer]:
        ...         # 意図に基づくオファー検索
        ...         ...
    """

    @abstractmethod
    async def get_offers_for_intent(
        self,
        intent: PurchaseIntent,
        *,
        limit: int = 5,
    ) -> list[Offer]:
        """意図に基づくオファーを取得.

        Args:
            intent: 購買意図
            limit: 最大件数

        Returns:
            オファーリスト
        """

    @abstractmethod
    async def get_offers_for_products(
        self,
        products: list[Product],
        *,
        user_context: dict[str, Any] | None = None,
    ) -> list[Offer]:
        """商品に対するオファーを取得.

        Args:
            products: 商品リスト
            user_context: ユーザーコンテキスト

        Returns:
            オファーリスト
        """

    @abstractmethod
    async def rank_offers(
        self,
        offers: list[Offer],
        intent: PurchaseIntent,
    ) -> list[Offer]:
        """オファーをランキング.

        Args:
            offers: オファーリスト
            intent: 購買意図

        Returns:
            ランキング済みオファーリスト
        """


class IDealRecommender(ABC):
    """取引推薦インターフェース.

    最適な取引（商品+オファー）を推薦。
    AI商取引の意思決定支援。

    Example:
        >>> class AIRecommender(IDealRecommender):
        ...     async def recommend(self, intent: PurchaseIntent) -> list[DirectOffer]:
        ...         # AI推薦ロジック
        ...         ...
    """

    @abstractmethod
    async def recommend(
        self,
        intent: PurchaseIntent,
        *,
        products: list[Product] | None = None,
        offers: list[Offer] | None = None,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """取引を推薦.

        Args:
            intent: 購買意図
            products: 候補商品リスト
            offers: 候補オファーリスト
            limit: 最大件数

        Returns:
            推薦取引リスト（DirectOffer）
        """

    @abstractmethod
    async def explain_recommendation(
        self,
        recommendation: DirectOffer,
    ) -> str:
        """推薦理由を説明.

        Args:
            recommendation: 推薦取引

        Returns:
            説明テキスト
        """

    @abstractmethod
    async def get_alternatives(
        self,
        recommendation: DirectOffer,
        *,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """代替案を取得.

        Args:
            recommendation: 元の推薦
            limit: 最大件数

        Returns:
            代替推薦リスト
        """
