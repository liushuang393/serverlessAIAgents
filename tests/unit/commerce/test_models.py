"""AI商取引データモデルのテスト.

Product, Offer, Cart, Transaction, PurchaseIntent モデルの単体テスト。
"""

from datetime import datetime, timedelta

from agentflow.commerce.models import (
    Cart,
    CartItem,
    DirectOffer,
    IntentConfidence,
    IntentType,
    Offer,
    OfferType,
    PaymentMethod,
    Product,
    ProductCategory,
    PurchaseIntent,
    Transaction,
    TransactionStatus,
)


class TestProduct:
    """Product モデルのテスト."""

    def test_product_creation(self) -> None:
        """商品の作成が正常に動作すること."""
        product = Product(
            product_id="prod-001",
            name="ワイヤレスイヤホン Pro",
            description="高音質ノイズキャンセリング",
            brand="TechBrand",
            category=ProductCategory.ELECTRONICS,
            price=15800.0,
            currency="JPY",
        )

        assert product.product_id == "prod-001"
        assert product.name == "ワイヤレスイヤホン Pro"
        assert product.price == 15800.0
        assert product.category == ProductCategory.ELECTRONICS

    def test_product_to_schema_org(self) -> None:
        """Schema.org形式への変換が正常に動作すること."""
        product = Product(
            product_id="prod-001",
            name="テスト商品",
            brand="TestBrand",
            price=1000.0,
            category=ProductCategory.ELECTRONICS,
        )

        schema = product.to_schema_org()

        assert schema["@context"] == "https://schema.org"
        assert schema["@type"] == "Product"
        assert schema["name"] == "テスト商品"
        assert schema["offers"]["price"] == 1000.0

    def test_product_to_ucp(self) -> None:
        """UCP形式への変換が正常に動作すること."""
        product = Product(
            product_id="prod-001",
            name="テスト商品",
            price=1000.0,
            intent_keywords=["ワイヤレス", "イヤホン"],
            recommendation_score=0.85,
        )

        ucp = product.to_ucp()

        assert ucp["id"] == "prod-001"
        assert ucp["price"]["amount"] == 1000.0
        assert ucp["ai_attributes"]["recommendation_score"] == 0.85


class TestOffer:
    """Offer モデルのテスト."""

    def test_offer_creation(self) -> None:
        """オファーの作成が正常に動作すること."""
        offer = Offer(
            offer_id="offer-001",
            product_id="prod-001",
            offer_type=OfferType.DISCOUNT,
            discount_value=20.0,
            discount_type="percent",
        )

        assert offer.offer_id == "offer-001"
        assert offer.offer_type == OfferType.DISCOUNT
        assert offer.discount_value == 20.0

    def test_offer_is_valid_within_period(self) -> None:
        """有効期間内のオファーがTrueを返すこと."""
        offer = Offer(
            offer_id="offer-001",
            offer_type=OfferType.COUPON,
            valid_from=datetime.now() - timedelta(days=1),
            valid_until=datetime.now() + timedelta(days=1),
        )

        assert offer.is_valid() is True

    def test_offer_is_valid_expired(self) -> None:
        """期限切れのオファーがFalseを返すこと."""
        offer = Offer(
            offer_id="offer-001",
            offer_type=OfferType.COUPON,
            valid_until=datetime.now() - timedelta(days=1),
        )

        assert offer.is_valid() is False

    def test_offer_calculate_discount_percent(self) -> None:
        """パーセント割引の計算が正常に動作すること."""
        offer = Offer(
            offer_id="offer-001",
            offer_type=OfferType.DISCOUNT,
            discount_value=20.0,
            discount_type="percent",
        )

        discount = offer.calculate_discount(10000.0)
        assert discount == 2000.0

    def test_offer_calculate_discount_amount(self) -> None:
        """金額割引の計算が正常に動作すること."""
        offer = Offer(
            offer_id="offer-001",
            offer_type=OfferType.DISCOUNT,
            discount_value=500.0,
            discount_type="amount",
        )

        discount = offer.calculate_discount(10000.0)
        assert discount == 500.0


class TestDirectOffer:
    """DirectOffer モデルのテスト."""

    def test_direct_offer_creation(self) -> None:
        """意図広告オファーの作成が正常に動作すること."""
        base_offer = Offer(
            offer_id="offer-001",
            offer_type=OfferType.FLASH_SALE,
            discount_value=30.0,
        )

        direct_offer = DirectOffer(
            offer=base_offer,
            intent_context={"category": "electronics"},
            ai_generated=True,
            confidence=0.92,
            headline="今だけ30%OFF！",
        )

        assert direct_offer.ai_generated is True
        assert direct_offer.confidence == 0.92
        assert direct_offer.headline == "今だけ30%OFF！"


class TestCart:
    """Cart モデルのテスト."""

    def test_cart_creation(self) -> None:
        """カートの作成が正常に動作すること."""
        cart = Cart(
            cart_id="cart-001",
            user_id="user-001",
        )

        assert cart.cart_id == "cart-001"
        assert cart.user_id == "user-001"
        assert cart.item_count == 0

    def test_cart_add_item(self) -> None:
        """アイテム追加が正常に動作すること."""
        cart = Cart(cart_id="cart-001")
        item = CartItem(
            product_id="prod-001",
            product_name="テスト商品",
            quantity=2,
            unit_price=1000.0,
        )

        cart.add_item(item)

        assert cart.item_count == 2
        assert cart.subtotal == 2000.0

    def test_cart_add_existing_item(self) -> None:
        """既存アイテムの数量が加算されること."""
        cart = Cart(cart_id="cart-001")
        item1 = CartItem(product_id="prod-001", quantity=1, unit_price=1000.0)
        item2 = CartItem(product_id="prod-001", quantity=2, unit_price=1000.0)

        cart.add_item(item1)
        cart.add_item(item2)

        assert len(cart.items) == 1
        assert cart.items[0].quantity == 3

    def test_cart_remove_item(self) -> None:
        """アイテム削除が正常に動作すること."""
        cart = Cart(cart_id="cart-001")
        cart.add_item(CartItem(product_id="prod-001", quantity=1, unit_price=1000.0))

        result = cart.remove_item("prod-001")

        assert result is True
        assert cart.item_count == 0

    def test_cart_total_with_discount(self) -> None:
        """割引適用後の合計が正しく計算されること."""
        cart = Cart(
            cart_id="cart-001",
            cart_discount=500.0,
            shipping_cost=300.0,
        )
        cart.add_item(CartItem(product_id="prod-001", quantity=2, unit_price=1000.0))

        # subtotal=2000, discount=500, shipping=300 → total=1800
        assert cart.total == 1800.0

    def test_cart_clear(self) -> None:
        """カートクリアが正常に動作すること."""
        cart = Cart(cart_id="cart-001", cart_discount=100.0)
        cart.add_item(CartItem(product_id="prod-001", quantity=1, unit_price=1000.0))

        cart.clear()

        assert cart.item_count == 0
        assert cart.cart_discount == 0.0


class TestTransaction:
    """Transaction モデルのテスト."""

    def test_transaction_creation(self) -> None:
        """取引の作成が正常に動作すること."""
        transaction = Transaction(
            transaction_id="txn-001",
            cart_id="cart-001",
            subtotal=10000.0,
            total_amount=10800.0,
            tax_amount=800.0,
        )

        assert transaction.transaction_id == "txn-001"
        assert transaction.status == TransactionStatus.PENDING
        assert transaction.total_amount == 10800.0

    def test_transaction_complete(self) -> None:
        """取引完了が正常に動作すること."""
        transaction = Transaction(
            transaction_id="txn-001",
            cart_id="cart-001",
            subtotal=10000.0,
            total_amount=10000.0,
        )

        transaction.complete()

        assert transaction.status == TransactionStatus.COMPLETED
        assert transaction.completed_at is not None

    def test_transaction_cancel(self) -> None:
        """取引キャンセルが正常に動作すること."""
        transaction = Transaction(
            transaction_id="txn-001",
            cart_id="cart-001",
            subtotal=10000.0,
            total_amount=10000.0,
        )

        transaction.cancel()

        assert transaction.status == TransactionStatus.CANCELLED

    def test_transaction_fail(self) -> None:
        """取引失敗が正常に動作すること."""
        transaction = Transaction(
            transaction_id="txn-001",
            cart_id="cart-001",
            subtotal=10000.0,
            total_amount=10000.0,
        )

        transaction.fail("決済エラー")

        assert transaction.status == TransactionStatus.FAILED
        assert transaction.ucp_metadata["failure_reason"] == "決済エラー"

    def test_transaction_to_ucp(self) -> None:
        """UCP形式への変換が正常に動作すること."""
        transaction = Transaction(
            transaction_id="txn-001",
            cart_id="cart-001",
            subtotal=10000.0,
            total_amount=10800.0,
            payment_method=PaymentMethod.CREDIT_CARD,
            ai_assisted=True,
        )

        ucp = transaction.to_ucp()

        assert ucp["id"] == "txn-001"
        assert ucp["amounts"]["total"] == 10800.0
        assert ucp["payment"]["method"] == "credit_card"
        assert ucp["ai_attributes"]["assisted"] is True


class TestPurchaseIntent:
    """PurchaseIntent モデルのテスト."""

    def test_intent_creation(self) -> None:
        """購買意図の作成が正常に動作すること."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="ワイヤレスイヤホンを探しています",
            intent_type=IntentType.PURCHASE,
            confidence_score=0.85,
        )

        assert intent.intent_id == "intent-001"
        assert intent.intent_type == IntentType.PURCHASE
        assert intent.confidence_score == 0.85

    def test_intent_is_purchase_ready_high_confidence(self) -> None:
        """高確度の購入意図がTrueを返すこと."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="今すぐ買いたい",
            intent_type=IntentType.PURCHASE,
            confidence_score=0.85,
        )

        assert intent.is_purchase_ready() is True

    def test_intent_is_purchase_ready_low_confidence(self) -> None:
        """低確度の購入意図がFalseを返すこと."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="ちょっと見てるだけ",
            intent_type=IntentType.BROWSE,
            confidence_score=0.3,
        )

        assert intent.is_purchase_ready() is False

    def test_intent_should_show_offer(self) -> None:
        """オファー表示判定が正常に動作すること."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="比較検討中",
            intent_type=IntentType.COMPARE,
            confidence_score=0.6,
        )

        assert intent.should_show_offer() is True

    def test_intent_from_confidence_score(self) -> None:
        """確度スコアからレベル変換が正常に動作すること."""
        assert PurchaseIntent.from_confidence_score(0.95) == IntentConfidence.VERY_HIGH
        assert PurchaseIntent.from_confidence_score(0.8) == IntentConfidence.HIGH
        assert PurchaseIntent.from_confidence_score(0.5) == IntentConfidence.MEDIUM
        assert PurchaseIntent.from_confidence_score(0.2) == IntentConfidence.LOW
