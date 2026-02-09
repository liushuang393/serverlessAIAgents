"""モックプロバイダー実装.

テスト・開発用のモック実装。
抽象インターフェースの参照実装として使用。
"""

from __future__ import annotations

import uuid
from datetime import datetime
from typing import Any

from agentflow.commerce.interfaces import (
    ICart,
    ICommerceAI,
    IDealRecommender,
    IIntentAnalyzer,
    IOffer,
    IOfferProvider,
    IPayment,
    IProduct,
    ITransaction,
)
from agentflow.commerce.models import (
    Cart,
    CartItem,
    DirectOffer,
    IntentType,
    Offer,
    OfferType,
    Product,
    PurchaseIntent,
    Transaction,
    TransactionStatus,
)


class MockProductProvider(IProduct):
    """モック商品プロバイダー.

    テスト用のインメモリ商品ストア。

    Example:
        >>> provider = MockProductProvider()
        >>> provider.add_product(Product(product_id="p1", name="Test", price=100))
        >>> product = await provider.get_product("p1")
    """

    def __init__(self) -> None:
        """初期化."""
        self._products: dict[str, Product] = {}

    def add_product(self, product: Product) -> None:
        """商品を追加."""
        self._products[product.product_id] = product

    async def get_product(self, product_id: str) -> Product | None:
        """商品を取得."""
        return self._products.get(product_id)

    async def search_products(
        self,
        query: str,
        *,
        category: str | None = None,
        price_min: float | None = None,
        price_max: float | None = None,
        limit: int = 20,
    ) -> list[Product]:
        """商品を検索."""
        results = []
        for product in self._products.values():
            # クエリマッチ
            if query.lower() not in product.name.lower():
                continue
            # カテゴリフィルター
            if category and product.category.value != category:
                continue
            # 価格フィルター
            if price_min and product.price < price_min:
                continue
            if price_max and product.price > price_max:
                continue
            results.append(product)
            if len(results) >= limit:
                break
        return results

    async def get_recommendations(
        self,
        user_id: str | None = None,
        context: dict[str, Any] | None = None,
        limit: int = 10,
    ) -> list[Product]:
        """推薦商品を取得."""
        # スコア順でソート
        sorted_products = sorted(
            self._products.values(),
            key=lambda p: p.recommendation_score,
            reverse=True,
        )
        return sorted_products[:limit]


class MockOfferProvider(IOffer):
    """モックオファープロバイダー."""

    def __init__(self) -> None:
        """初期化."""
        self._offers: dict[str, Offer] = {}

    def add_offer(self, offer: Offer) -> None:
        """オファーを追加."""
        self._offers[offer.offer_id] = offer

    async def get_offer(self, offer_id: str) -> Offer | None:
        """オファーを取得."""
        return self._offers.get(offer_id)

    async def get_offers_for_product(
        self,
        product_id: str,
        *,
        user_id: str | None = None,
    ) -> list[Offer]:
        """商品に適用可能なオファーを取得."""
        return [
            offer for offer in self._offers.values()
            if offer.product_id == product_id and offer.is_valid()
        ]

    async def get_offers_for_intent(
        self,
        intent_keywords: list[str],
        *,
        user_context: dict[str, Any] | None = None,
    ) -> list[Offer]:
        """意図に基づくオファーを取得."""
        results = []
        for offer in self._offers.values():
            if not offer.is_valid():
                continue
            # キーワードマッチ
            for keyword in intent_keywords:
                if keyword.lower() in [t.lower() for t in offer.target_intents]:
                    results.append(offer)
                    break
        return results

    async def validate_offer(self, offer_id: str, cart: Cart) -> bool:
        """オファーの適用可否を検証."""
        offer = self._offers.get(offer_id)
        if not offer or not offer.is_valid():
            return False
        # 最低購入金額チェック
        return not (offer.min_purchase_amount and cart.subtotal < offer.min_purchase_amount)


class MockCartProvider(ICart):
    """モックカートプロバイダー."""

    def __init__(self) -> None:
        """初期化."""
        self._carts: dict[str, Cart] = {}

    async def get_cart(self, cart_id: str) -> Cart | None:
        """カートを取得."""
        return self._carts.get(cart_id)

    async def create_cart(
        self,
        user_id: str | None = None,
        session_id: str | None = None,
    ) -> Cart:
        """カートを作成."""
        cart_id = f"cart-{uuid.uuid4().hex[:8]}"
        cart = Cart(cart_id=cart_id, user_id=user_id, session_id=session_id)
        self._carts[cart_id] = cart
        return cart

    async def add_item(self, cart_id: str, item: CartItem) -> Cart:
        """アイテムを追加."""
        cart = self._carts.get(cart_id)
        if not cart:
            msg = f"Cart not found: {cart_id}"
            raise ValueError(msg)
        cart.add_item(item)
        return cart

    async def remove_item(self, cart_id: str, product_id: str) -> Cart:
        """アイテムを削除."""
        cart = self._carts.get(cart_id)
        if not cart:
            msg = f"Cart not found: {cart_id}"
            raise ValueError(msg)
        cart.remove_item(product_id)
        return cart

    async def apply_offer(self, cart_id: str, offer_id: str) -> Cart:
        """オファーを適用."""
        cart = self._carts.get(cart_id)
        if not cart:
            msg = f"Cart not found: {cart_id}"
            raise ValueError(msg)
        cart.applied_offers.append(offer_id)
        return cart


class MockTransactionProvider(ITransaction):
    """モック取引プロバイダー."""

    def __init__(self) -> None:
        """初期化."""
        self._transactions: dict[str, Transaction] = {}

    async def create_transaction(self, cart: Cart) -> Transaction:
        """取引を作成."""
        txn_id = f"txn-{uuid.uuid4().hex[:8]}"
        transaction = Transaction(
            transaction_id=txn_id,
            cart_id=cart.cart_id,
            user_id=cart.user_id,
            subtotal=cart.subtotal,
            discount_amount=cart.cart_discount,
            shipping_cost=cart.shipping_cost,
            total_amount=cart.total,
            applied_offers=cart.applied_offers.copy(),
        )
        self._transactions[txn_id] = transaction
        return transaction

    async def get_transaction(self, transaction_id: str) -> Transaction | None:
        """取引を取得."""
        return self._transactions.get(transaction_id)

    async def update_status(
        self,
        transaction_id: str,
        status: str,
    ) -> Transaction:
        """取引ステータスを更新."""
        transaction = self._transactions.get(transaction_id)
        if not transaction:
            msg = f"Transaction not found: {transaction_id}"
            raise ValueError(msg)
        transaction.status = TransactionStatus(status)
        transaction.updated_at = datetime.now()
        return transaction

    async def complete_transaction(self, transaction_id: str) -> Transaction:
        """取引を完了."""
        transaction = self._transactions.get(transaction_id)
        if not transaction:
            msg = f"Transaction not found: {transaction_id}"
            raise ValueError(msg)
        transaction.complete()
        return transaction


class MockPaymentProvider(IPayment):
    """モック決済プロバイダー."""

    def __init__(self, *, always_succeed: bool = True) -> None:
        """初期化."""
        self._always_succeed = always_succeed
        self._payments: dict[str, dict[str, Any]] = {}

    async def process_payment(
        self,
        transaction: Transaction,
        payment_method: str,
        payment_details: dict[str, Any],
    ) -> dict[str, Any]:
        """決済を処理."""
        payment_id = f"pay-{uuid.uuid4().hex[:8]}"
        result = {
            "payment_id": payment_id,
            "transaction_id": transaction.transaction_id,
            "amount": transaction.total_amount,
            "status": "succeeded" if self._always_succeed else "failed",
            "method": payment_method,
        }
        self._payments[payment_id] = result
        return result

    async def refund(
        self,
        transaction_id: str,
        amount: float | None = None,
    ) -> dict[str, Any]:
        """返金処理."""
        return {
            "refund_id": f"ref-{uuid.uuid4().hex[:8]}",
            "transaction_id": transaction_id,
            "amount": amount,
            "status": "succeeded",
        }

    async def get_payment_status(self, payment_id: str) -> dict[str, Any]:
        """決済ステータスを取得."""
        return self._payments.get(payment_id, {"status": "not_found"})


class MockIntentAnalyzer(IIntentAnalyzer):
    """モック意図分析エージェント."""

    def __init__(self, *, default_confidence: float = 0.8) -> None:
        """初期化."""
        self._default_confidence = default_confidence

    async def analyze(
        self,
        user_input: str,
        *,
        conversation_context: list[dict[str, str]] | None = None,
        user_id: str | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析."""
        intent_type = IntentType.BROWSE
        confidence = self._default_confidence

        # 簡易キーワード分析
        lower_input = user_input.lower()
        if any(w in lower_input for w in ["買いたい", "購入", "注文", "buy"]):
            intent_type = IntentType.PURCHASE
            confidence = 0.9
        elif any(w in lower_input for w in ["比較", "どっち", "compare"]):
            intent_type = IntentType.COMPARE
            confidence = 0.7

        return PurchaseIntent(
            intent_id=f"intent-{uuid.uuid4().hex[:8]}",
            user_input=user_input,
            user_id=user_id,
            intent_type=intent_type,
            confidence_score=confidence,
        )

    async def extract_entities(self, user_input: str) -> list[dict[str, Any]]:
        """エンティティを抽出."""
        return [{"type": "query", "value": user_input}]

    async def classify_intent_type(self, user_input: str) -> tuple[str, float]:
        """意図タイプを分類."""
        intent = await self.analyze(user_input)
        return intent.intent_type.value, intent.confidence_score


class MockOfferMatcher(IOfferProvider):
    """モックオファーマッチャー."""

    def __init__(self, offer_provider: MockOfferProvider) -> None:
        """初期化."""
        self._offer_provider = offer_provider

    async def get_offers_for_intent(
        self,
        intent: PurchaseIntent,
        *,
        limit: int = 5,
    ) -> list[Offer]:
        """意図に基づくオファーを取得."""
        keywords = intent.product_keywords or [intent.user_input]
        offers = await self._offer_provider.get_offers_for_intent(keywords)
        return offers[:limit]

    async def get_offers_for_products(
        self,
        products: list[Product],
        *,
        user_context: dict[str, Any] | None = None,
    ) -> list[Offer]:
        """商品に対するオファーを取得."""
        all_offers = []
        for product in products:
            offers = await self._offer_provider.get_offers_for_product(product.product_id)
            all_offers.extend(offers)
        return all_offers

    async def rank_offers(
        self,
        offers: list[Offer],
        intent: PurchaseIntent,
    ) -> list[Offer]:
        """オファーをランキング."""
        return sorted(offers, key=lambda o: o.intent_match_score, reverse=True)


class MockDealRecommender(IDealRecommender):
    """モック取引推薦エージェント."""

    async def recommend(
        self,
        intent: PurchaseIntent,
        *,
        products: list[Product] | None = None,
        offers: list[Offer] | None = None,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """取引を推薦."""
        recommendations = []
        if offers:
            for offer in offers[:limit]:
                direct_offer = DirectOffer(
                    offer=offer,
                    intent_context={"intent_type": intent.intent_type.value},
                    ai_generated=True,
                    confidence=intent.confidence_score,
                    headline=f"おすすめ: {offer.description or 'お得なオファー'}",
                )
                recommendations.append(direct_offer)
        return recommendations

    async def explain_recommendation(self, recommendation: DirectOffer) -> str:
        """推薦理由を説明."""
        return f"このオファーは信頼度{recommendation.confidence:.0%}で推薦されています。"

    async def get_alternatives(
        self,
        recommendation: DirectOffer,
        *,
        limit: int = 3,
    ) -> list[DirectOffer]:
        """代替案を取得."""
        return []


class MockCommerceAI(ICommerceAI):
    """モックAI商取引エージェント."""

    def __init__(self, intent_analyzer: MockIntentAnalyzer | None = None) -> None:
        """初期化."""
        self._intent_analyzer = intent_analyzer or MockIntentAnalyzer()

    async def analyze_intent(
        self,
        user_input: str,
        *,
        conversation_history: list[dict[str, str]] | None = None,
        user_context: dict[str, Any] | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析."""
        return await self._intent_analyzer.analyze(
            user_input, conversation_context=conversation_history
        )

    async def detect_purchase_signal(self, user_input: str) -> tuple[bool, float]:
        """購買シグナルを検出."""
        intent = await self._intent_analyzer.analyze(user_input)
        is_purchase = intent.intent_type in [IntentType.PURCHASE, IntentType.URGENT]
        return is_purchase, intent.confidence_score

    async def recommend_products(
        self,
        intent: PurchaseIntent,
        *,
        available_products: list[Product] | None = None,
        limit: int = 5,
    ) -> list[Product]:
        """商品を推薦."""
        if not available_products:
            return []
        return sorted(
            available_products,
            key=lambda p: p.recommendation_score,
            reverse=True,
        )[:limit]

    async def explain_product(
        self,
        product: Product,
        *,
        user_context: dict[str, Any] | None = None,
    ) -> str:
        """商品を説明."""
        return f"{product.name}: {product.description}"

    async def generate_offer(
        self,
        product: Product,
        intent: PurchaseIntent,
        *,
        existing_offers: list[Offer] | None = None,
    ) -> DirectOffer:
        """オファーを生成."""
        offer = Offer(
            offer_id=f"ai-offer-{uuid.uuid4().hex[:8]}",
            product_id=product.product_id,
            offer_type=OfferType.DISCOUNT,
            discount_value=10.0,
            discount_type="percent",
            description=f"{product.name}の特別オファー",
        )
        return DirectOffer(
            offer=offer,
            intent_context={"intent_type": intent.intent_type.value},
            ai_generated=True,
            confidence=0.85,
            headline=f"AI推薦: {product.name}が10%OFF",
        )

    async def evaluate_deal(
        self,
        offer: DirectOffer,
        *,
        user_budget: float | None = None,
        alternatives: list[DirectOffer] | None = None,
    ) -> dict[str, Any]:
        """取引を評価."""
        return {
            "score": offer.confidence,
            "recommendation": "buy" if offer.confidence > 0.7 else "consider",
            "reason": "高い信頼度スコアに基づく推薦",
        }

    async def generate_response(
        self,
        user_input: str,
        *,
        context: dict[str, Any] | None = None,
        intent: PurchaseIntent | None = None,
    ) -> str:
        """応答を生成."""
        if intent and intent.is_purchase_ready():
            return "ご購入の準備ができました。決済に進みますか？"
        return "ご質問ありがとうございます。何かお探しですか？"

    async def negotiate_price(
        self,
        product: Product,
        user_offer: float,
        *,
        min_acceptable: float | None = None,
    ) -> dict[str, Any]:
        """価格交渉."""
        min_price = min_acceptable or product.price * 0.8
        if user_offer >= min_price:
            return {"accepted": True, "final_price": user_offer}
        return {
            "accepted": False,
            "counter_offer": min_price,
            "message": f"申し訳ございませんが、{min_price}円が最低価格です。",
        }

