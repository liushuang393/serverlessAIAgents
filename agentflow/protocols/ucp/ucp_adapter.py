# -*- coding: utf-8 -*-
"""UCPプロトコルアダプター.

AgentFlowのコマースモジュールとUCPプロトコルを接続するアダプター。
抽象インターフェースとUCPクライアント間のブリッジとして機能。

設計原則:
- アダプターパターン: 既存インターフェースとUCPの変換
- 疎結合: UCP固有ロジックをカプセル化
- 拡張性: 新しいUCP機能の追加が容易
"""

from __future__ import annotations

import logging
import uuid
from typing import Any

from agentflow.commerce.interfaces import (
    ICommerceAI,
    IDealRecommender,
    IIntentAnalyzer,
    IOfferProvider,
)
from agentflow.commerce.models import (
    DirectOffer,
    IntentType,
    Offer,
    OfferType,
    Product,
    PurchaseIntent,
)
from agentflow.protocols.ucp.ucp_client import UCPClient
from agentflow.protocols.ucp.ucp_config import UCPConfig


class UCPAdapter:
    """UCPプロトコルアダプター.

    AgentFlowのコマースインターフェースをUCPプロトコルに適応させる。
    UCPクライアントを使用してリモートUCPサービスと通信。

    Example:
        >>> config = UCPConfig.from_env()
        >>> adapter = UCPAdapter(config)
        >>> intent = await adapter.analyze_intent("ノートPCが欲しい")
        >>> offers = await adapter.get_offers_for_intent(intent)
    """

    def __init__(
        self,
        config: UCPConfig,
        *,
        logger: logging.Logger | None = None,
    ) -> None:
        """UCPアダプターを初期化.

        Args:
            config: UCP設定
            logger: ロガーインスタンス（オプション）
        """
        self._config = config
        self._logger = logger or logging.getLogger(__name__)
        self._client = UCPClient(config, logger=self._logger)

    async def close(self) -> None:
        """アダプターをクローズ."""
        await self._client.close()

    async def __aenter__(self) -> "UCPAdapter":
        """非同期コンテキストマネージャーのエントリー."""
        return self

    async def __aexit__(self, *_args: Any) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.close()

    def _map_intent_type(self, ucp_type: str) -> IntentType:
        """UCPの意図タイプをAgentFlowの意図タイプに変換."""
        mapping = {
            "purchase": IntentType.PURCHASE,
            "browse": IntentType.BROWSE,
            "compare": IntentType.COMPARE,
            "research": IntentType.RESEARCH,
            "urgent": IntentType.URGENT,
        }
        return mapping.get(ucp_type.lower(), IntentType.BROWSE)

    def _map_offer_type(self, ucp_type: str) -> OfferType:
        """UCPのオファータイプをAgentFlowのオファータイプに変換."""
        mapping = {
            "discount": OfferType.DISCOUNT,
            "coupon": OfferType.COUPON,
            "bundle": OfferType.BUNDLE,
            "flash_sale": OfferType.FLASH_SALE,
            "loyalty": OfferType.LOYALTY,
            "cashback": OfferType.CASHBACK,
        }
        return mapping.get(ucp_type.lower(), OfferType.DISCOUNT)

    async def analyze_intent(
        self,
        user_input: str,
        *,
        conversation_context: list[dict[str, str]] | None = None,
        user_id: str | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析（IIntentAnalyzer互換）.

        Args:
            user_input: ユーザー入力テキスト
            conversation_context: 会話コンテキスト（オプション）
            user_id: ユーザーID（オプション）

        Returns:
            PurchaseIntentモデル
        """
        response = await self._client.analyze_intent(
            user_input,
            user_id=user_id,
            conversation_history=conversation_context,
        )

        return PurchaseIntent(
            intent_id=response.intent_id,
            user_input=user_input,
            user_id=user_id,
            intent_type=self._map_intent_type(response.intent_type),
            confidence_score=response.confidence,
            product_categories=response.product_categories,
            product_keywords=response.keywords,
            price_range=response.price_range,
            recommended_offer_types=response.recommended_actions,
        )

    async def get_offers_for_intent(
        self,
        intent: PurchaseIntent,
        *,
        limit: int = 5,
    ) -> list[Offer]:
        """意図に基づくオファーを取得（IOfferProvider互換）.

        Args:
            intent: 購買意図
            limit: 最大取得数

        Returns:
            オファーリスト
        """
        response = await self._client.get_offers(
            intent.intent_id,
            keywords=intent.product_keywords,
            limit=limit,
        )

        offers = []
        for offer_data in response.offers:
            offer = Offer(
                offer_id=offer_data.get("offer_id", f"ucp-{uuid.uuid4().hex[:8]}"),
                product_id=offer_data.get("product_id", ""),
                offer_type=self._map_offer_type(offer_data.get("type", "discount")),
                discount_value=offer_data.get("discount_value", 0.0),
                discount_type=offer_data.get("discount_type", "percent"),
                description=offer_data.get("description", ""),
                intent_match_score=offer_data.get("match_score", 0.5),
                target_intents=offer_data.get("target_intents", []),
            )
            offers.append(offer)
        return offers

    async def recommend_deals(
        self,
        intent: PurchaseIntent,
        *,
        products: list[Product] | None = None,
        offers: list[Offer] | None = None,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """取引を推薦（IDealRecommender互換）.

        Args:
            intent: 購買意図
            products: 商品リスト（オプション）
            offers: オファーリスト（オプション）
            limit: 最大推薦数

        Returns:
            DirectOfferリスト
        """
        # オファーがない場合はUCPから取得
        if not offers:
            offers = await self.get_offers_for_intent(intent, limit=limit)

        recommendations = []
        for offer in offers[:limit]:
            direct_offer = DirectOffer(
                offer=offer,
                intent_context={
                    "intent_type": intent.intent_type.value,
                    "confidence": intent.confidence_score,
                    "categories": intent.product_categories,
                },
                ai_generated=True,
                confidence=intent.confidence_score * offer.intent_match_score,
                headline=f"おすすめ: {offer.description or 'お得なオファー'}",
                ucp_metadata={"source": "ucp_adapter"},
            )
            recommendations.append(direct_offer)
        return recommendations

    def create_intent_analyzer(self) -> "UCPIntentAnalyzerAdapter":
        """IIntentAnalyzer互換アダプターを作成.

        Returns:
            UCPIntentAnalyzerAdapter インスタンス
        """
        return UCPIntentAnalyzerAdapter(self)

    def create_offer_provider(self) -> "UCPOfferProviderAdapter":
        """IOfferProvider互換アダプターを作成.

        Returns:
            UCPOfferProviderAdapter インスタンス
        """
        return UCPOfferProviderAdapter(self)

    def create_deal_recommender(self) -> "UCPDealRecommenderAdapter":
        """IDealRecommender互換アダプターを作成.

        Returns:
            UCPDealRecommenderAdapter インスタンス
        """
        return UCPDealRecommenderAdapter(self)


class UCPIntentAnalyzerAdapter(IIntentAnalyzer):
    """UCPベースの意図分析アダプター.

    IIntentAnalyzerインターフェースを実装し、
    UCPAdapterを使用して意図分析を行う。
    """

    def __init__(self, adapter: UCPAdapter) -> None:
        """初期化."""
        self._adapter = adapter

    async def analyze(
        self,
        user_input: str,
        *,
        conversation_context: list[dict[str, str]] | None = None,
        user_id: str | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析."""
        return await self._adapter.analyze_intent(
            user_input,
            conversation_context=conversation_context,
            user_id=user_id,
        )

    async def extract_entities(self, user_input: str) -> list[dict[str, Any]]:
        """エンティティを抽出."""
        intent = await self.analyze(user_input)
        entities = []
        for category in intent.product_categories:
            entities.append({"type": "category", "value": category})
        for keyword in intent.product_keywords or []:
            entities.append({"type": "keyword", "value": keyword})
        return entities

    async def classify_intent_type(self, user_input: str) -> tuple[str, float]:
        """意図タイプを分類."""
        intent = await self.analyze(user_input)
        return intent.intent_type.value, intent.confidence_score


class UCPOfferProviderAdapter(IOfferProvider):
    """UCPベースのオファープロバイダーアダプター.

    IOfferProviderインターフェースを実装し、
    UCPAdapterを使用してオファーを取得する。
    """

    def __init__(self, adapter: UCPAdapter) -> None:
        """初期化."""
        self._adapter = adapter

    async def get_offers_for_intent(
        self,
        intent: PurchaseIntent,
        *,
        limit: int = 5,
    ) -> list[Offer]:
        """意図に基づくオファーを取得."""
        return await self._adapter.get_offers_for_intent(intent, limit=limit)

    async def get_offers_for_products(
        self,
        products: list[Product],
        *,
        user_context: dict[str, Any] | None = None,
    ) -> list[Offer]:
        """商品に対するオファーを取得."""
        # 商品キーワードから意図を構築
        keywords = []
        for product in products:
            keywords.extend(product.intent_keywords)

        dummy_intent = PurchaseIntent(
            intent_id=f"product-intent-{uuid.uuid4().hex[:8]}",
            user_input=" ".join(keywords),
            intent_type=IntentType.PURCHASE,
            confidence_score=0.8,
            product_keywords=keywords,
        )
        return await self._adapter.get_offers_for_intent(dummy_intent)

    async def rank_offers(
        self,
        offers: list[Offer],
        intent: PurchaseIntent,
    ) -> list[Offer]:
        """オファーをランキング."""
        return sorted(offers, key=lambda o: o.intent_match_score, reverse=True)


class UCPDealRecommenderAdapter(IDealRecommender):
    """UCPベースの取引推薦アダプター.

    IDealRecommenderインターフェースを実装し、
    UCPAdapterを使用して取引を推薦する。
    """

    def __init__(self, adapter: UCPAdapter) -> None:
        """初期化."""
        self._adapter = adapter

    async def recommend(
        self,
        intent: PurchaseIntent,
        *,
        products: list[Product] | None = None,
        offers: list[Offer] | None = None,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """取引を推薦."""
        return await self._adapter.recommend_deals(
            intent, products=products, offers=offers, limit=limit
        )

    async def explain_recommendation(self, recommendation: DirectOffer) -> str:
        """推薦理由を説明."""
        confidence = recommendation.confidence
        intent_type = recommendation.intent_context.get("intent_type", "browse")
        return (
            f"このオファーは信頼度{confidence:.0%}で推薦されています。"
            f"あなたの購買意図（{intent_type}）に最適化されています。"
        )

    async def get_alternatives(
        self,
        recommendation: DirectOffer,
        *,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """代替案を取得."""
        # 代替案の取得はUCP経由では制限あり
        return []

