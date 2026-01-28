# -*- coding: utf-8 -*-
"""コア抽象インターフェース.

商品、オファー、カート、取引、決済の抽象インターフェース。
UCP（Universal Commerce Protocol）互換設計。

設計原則:
- 非同期優先（async/await）
- 型安全（完全型アノテーション）
- 拡張可能（将来のUCP変更に対応）
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from agentflow.commerce.models import (
        Cart,
        CartItem,
        Offer,
        Product,
        Transaction,
    )


class IProduct(ABC):
    """商品プロバイダーインターフェース.

    商品の検索、取得、管理を担当。
    実装例: DatabaseProductProvider, APIProductProvider

    Example:
        >>> class MyProductProvider(IProduct):
        ...     async def get_product(self, product_id: str) -> Product | None:
        ...         return await self.db.get(product_id)
    """

    @abstractmethod
    async def get_product(self, product_id: str) -> Product | None:
        """商品を取得.

        Args:
            product_id: 商品ID

        Returns:
            商品、存在しない場合None
        """

    @abstractmethod
    async def search_products(
        self,
        query: str,
        *,
        category: str | None = None,
        price_min: float | None = None,
        price_max: float | None = None,
        limit: int = 20,
    ) -> list[Product]:
        """商品を検索.

        Args:
            query: 検索クエリ
            category: カテゴリフィルター
            price_min: 最低価格
            price_max: 最高価格
            limit: 最大件数

        Returns:
            商品リスト
        """

    @abstractmethod
    async def get_recommendations(
        self,
        user_id: str | None = None,
        context: dict[str, Any] | None = None,
        limit: int = 10,
    ) -> list[Product]:
        """推薦商品を取得.

        Args:
            user_id: ユーザーID
            context: コンテキスト情報
            limit: 最大件数

        Returns:
            推薦商品リスト
        """


class IOffer(ABC):
    """オファープロバイダーインターフェース.

    オファーの検索、取得、適用を担当。
    意図広告（Direct Offers）対応。
    """

    @abstractmethod
    async def get_offer(self, offer_id: str) -> Offer | None:
        """オファーを取得.

        Args:
            offer_id: オファーID

        Returns:
            オファー、存在しない場合None
        """

    @abstractmethod
    async def get_offers_for_product(
        self,
        product_id: str,
        *,
        user_id: str | None = None,
    ) -> list[Offer]:
        """商品に適用可能なオファーを取得.

        Args:
            product_id: 商品ID
            user_id: ユーザーID

        Returns:
            オファーリスト
        """

    @abstractmethod
    async def get_offers_for_intent(
        self,
        intent_keywords: list[str],
        *,
        user_context: dict[str, Any] | None = None,
    ) -> list[Offer]:
        """意図に基づくオファーを取得（意図広告用）.

        Args:
            intent_keywords: 意図キーワード
            user_context: ユーザーコンテキスト

        Returns:
            オファーリスト
        """

    @abstractmethod
    async def validate_offer(
        self,
        offer_id: str,
        cart: Cart,
    ) -> bool:
        """オファーの適用可否を検証.

        Args:
            offer_id: オファーID
            cart: カート

        Returns:
            適用可能な場合True
        """


class ICart(ABC):
    """カート管理インターフェース.

    カートの作成、更新、取得を担当。
    """

    @abstractmethod
    async def get_cart(self, cart_id: str) -> Cart | None:
        """カートを取得.

        Args:
            cart_id: カートID

        Returns:
            カート、存在しない場合None
        """

    @abstractmethod
    async def create_cart(
        self,
        user_id: str | None = None,
        session_id: str | None = None,
    ) -> Cart:
        """カートを作成.

        Args:
            user_id: ユーザーID
            session_id: セッションID

        Returns:
            新規カート
        """

    @abstractmethod
    async def add_item(self, cart_id: str, item: CartItem) -> Cart:
        """アイテムを追加.

        Args:
            cart_id: カートID
            item: 追加アイテム

        Returns:
            更新後のカート
        """

    @abstractmethod
    async def remove_item(self, cart_id: str, product_id: str) -> Cart:
        """アイテムを削除.

        Args:
            cart_id: カートID
            product_id: 商品ID

        Returns:
            更新後のカート
        """

    @abstractmethod
    async def apply_offer(self, cart_id: str, offer_id: str) -> Cart:
        """オファーを適用.

        Args:
            cart_id: カートID
            offer_id: オファーID

        Returns:
            更新後のカート
        """


class ITransaction(ABC):
    """取引管理インターフェース.

    取引の作成、更新、取得を担当。
    """

    @abstractmethod
    async def create_transaction(self, cart: Cart) -> Transaction:
        """取引を作成.

        Args:
            cart: カート

        Returns:
            新規取引
        """

    @abstractmethod
    async def get_transaction(self, transaction_id: str) -> Transaction | None:
        """取引を取得.

        Args:
            transaction_id: 取引ID

        Returns:
            取引、存在しない場合None
        """

    @abstractmethod
    async def update_status(
        self,
        transaction_id: str,
        status: str,
    ) -> Transaction:
        """取引ステータスを更新.

        Args:
            transaction_id: 取引ID
            status: 新ステータス

        Returns:
            更新後の取引
        """

    @abstractmethod
    async def complete_transaction(self, transaction_id: str) -> Transaction:
        """取引を完了.

        Args:
            transaction_id: 取引ID

        Returns:
            完了した取引
        """


class IPayment(ABC):
    """決済インターフェース.

    決済処理を担当。
    Stripe等の決済プロバイダーと連携。
    """

    @abstractmethod
    async def process_payment(
        self,
        transaction: Transaction,
        payment_method: str,
        payment_details: dict[str, Any],
    ) -> dict[str, Any]:
        """決済を処理.

        Args:
            transaction: 取引
            payment_method: 支払い方法
            payment_details: 支払い詳細

        Returns:
            決済結果
        """

    @abstractmethod
    async def refund(
        self,
        transaction_id: str,
        amount: float | None = None,
    ) -> dict[str, Any]:
        """返金処理.

        Args:
            transaction_id: 取引ID
            amount: 返金額（Noneの場合全額）

        Returns:
            返金結果
        """

    @abstractmethod
    async def get_payment_status(self, payment_id: str) -> dict[str, Any]:
        """決済ステータスを取得.

        Args:
            payment_id: 決済ID

        Returns:
            決済ステータス
        """

