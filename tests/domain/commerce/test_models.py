"""domain/commerce モデルのテスト.

contracts/protocol/commerce から re-export されたモデルの
基本的な作成・シリアライゼーションをテスト。
"""

from __future__ import annotations

import pytest

from domain.commerce.models import (
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

    def test_create_product(self) -> None:
        """基本的な商品作成."""
        product = Product(
            product_id="prod-001",
            name="テスト商品",
            price=1000.0,
            currency="JPY",
        )
        assert product.product_id == "prod-001"
        assert product.price == 1000.0
        assert product.category == ProductCategory.OTHER

    def test_product_to_schema_org(self) -> None:
        """Schema.org 形式への変換."""
        product = Product(
            product_id="prod-002",
            name="テスト",
            price=500.0,
            brand="TestBrand",
            category=ProductCategory.ELECTRONICS,
        )
        schema = product.to_schema_org()
        assert schema["@type"] == "Product"
        assert schema["name"] == "テスト"

    def test_product_to_ucp(self) -> None:
        """UCP 形式への変換."""
        product = Product(
            product_id="prod-003",
            name="UCP商品",
            price=2000.0,
        )
        ucp = product.to_ucp()
        assert ucp["id"] == "prod-003"
        assert ucp["price"]["amount"] == 2000.0


class TestCart:
    """Cart モデルのテスト."""

    def test_create_cart(self) -> None:
        """基本的なカート作成."""
        cart = Cart(cart_id="cart-001")
        assert cart.cart_id == "cart-001"
        assert cart.items == []


class TestPurchaseIntent:
    """PurchaseIntent モデルのテスト."""

    def test_create_intent(self) -> None:
        """購入意図の作成."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="スニーカーを探しています",
            intent_type=IntentType.BROWSE,
            confidence=IntentConfidence.MEDIUM,
            keywords=["スニーカー", "ランニング"],
        )
        assert intent.intent_type == IntentType.BROWSE
        assert intent.confidence == IntentConfidence.MEDIUM


class TestTransaction:
    """Transaction モデルのテスト."""

    def test_create_transaction(self) -> None:
        """取引の作成."""
        tx = Transaction(
            transaction_id="tx-001",
            cart_id="cart-001",
            subtotal=5000.0,
            total_amount=5000.0,
        )
        assert tx.transaction_id == "tx-001"
        assert tx.status == TransactionStatus.PENDING


class TestReexportIntegrity:
    """domain → contracts re-export の整合性テスト."""

    def test_interfaces_importable_from_domain(self) -> None:
        """domain.commerce からインターフェースをインポートできる."""
        from domain.commerce import IProduct, ICart, ITransaction

        assert IProduct is not None
        assert ICart is not None

    def test_interfaces_importable_from_contracts(self) -> None:
        """contracts.protocol.commerce からインターフェースをインポートできる."""
        from contracts.protocol.commerce import IProduct, ICart, ITransaction

        assert IProduct is not None

    def test_same_classes(self) -> None:
        """domain と contracts で同一クラスを参照."""
        from contracts.protocol.commerce.models import Product as CProduct
        from domain.commerce.models import Product as DProduct

        assert CProduct is DProduct
