# -*- coding: utf-8 -*-
"""AI商取引インターフェース.

LLM（Gemini等）を活用したAI商取引機能の抽象インターフェース。
意図分析、商品推薦、価格交渉、購入意思決定支援を提供。

設計原則:
- プロバイダー非依存: OpenAI, Anthropic, Google等に対応
- 疎結合: get_llm()パターンで実装を差し替え可能
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


class ICommerceAI(ABC):
    """AI商取引インターフェース.

    LLMを活用した商取引支援機能を提供。
    Google Gemini、OpenAI、Anthropic等のプロバイダーに対応。

    Example:
        >>> class GeminiCommerceAI(ICommerceAI):
        ...     def __init__(self):
        ...         self.llm = get_llm()  # Gemini自動選択
        ...
        ...     async def analyze_intent(self, user_input: str) -> PurchaseIntent:
        ...         response = await self.llm.generate(...)
        ...         return PurchaseIntent.model_validate(response)
    """

    # --- 意図分析 ---

    @abstractmethod
    async def analyze_intent(
        self,
        user_input: str,
        *,
        conversation_history: list[dict[str, str]] | None = None,
        user_context: dict[str, Any] | None = None,
    ) -> PurchaseIntent:
        """購買意図を分析.

        Args:
            user_input: ユーザー入力テキスト
            conversation_history: 会話履歴
            user_context: ユーザーコンテキスト（購買履歴等）

        Returns:
            購買意図
        """

    @abstractmethod
    async def detect_purchase_signal(
        self,
        user_input: str,
    ) -> tuple[bool, float]:
        """購買シグナルを検出.

        Args:
            user_input: ユーザー入力テキスト

        Returns:
            (購買意図あり, 確度スコア)
        """

    # --- 商品推薦 ---

    @abstractmethod
    async def recommend_products(
        self,
        intent: PurchaseIntent,
        *,
        available_products: list[Product] | None = None,
        limit: int = 5,
    ) -> list[Product]:
        """商品を推薦.

        Args:
            intent: 購買意図
            available_products: 候補商品リスト
            limit: 最大件数

        Returns:
            推薦商品リスト
        """

    @abstractmethod
    async def explain_product(
        self,
        product: Product,
        *,
        user_context: dict[str, Any] | None = None,
    ) -> str:
        """商品を説明.

        Args:
            product: 商品
            user_context: ユーザーコンテキスト

        Returns:
            商品説明テキスト
        """

    # --- オファー・取引 ---

    @abstractmethod
    async def generate_offer(
        self,
        product: Product,
        intent: PurchaseIntent,
        *,
        existing_offers: list[Offer] | None = None,
    ) -> DirectOffer:
        """オファーを生成.

        Args:
            product: 対象商品
            intent: 購買意図
            existing_offers: 既存オファー

        Returns:
            生成されたDirectOffer
        """

    @abstractmethod
    async def evaluate_deal(
        self,
        offer: DirectOffer,
        *,
        user_budget: float | None = None,
        alternatives: list[DirectOffer] | None = None,
    ) -> dict[str, Any]:
        """取引を評価.

        Args:
            offer: 評価対象オファー
            user_budget: ユーザー予算
            alternatives: 代替オファー

        Returns:
            評価結果（スコア、推薦度、理由等）
        """

    # --- 対話・交渉 ---

    @abstractmethod
    async def generate_response(
        self,
        user_input: str,
        *,
        context: dict[str, Any] | None = None,
        intent: PurchaseIntent | None = None,
    ) -> str:
        """応答を生成.

        Args:
            user_input: ユーザー入力
            context: コンテキスト
            intent: 購買意図

        Returns:
            応答テキスト
        """

    @abstractmethod
    async def negotiate_price(
        self,
        product: Product,
        user_offer: float,
        *,
        min_acceptable: float | None = None,
    ) -> dict[str, Any]:
        """価格交渉.

        Args:
            product: 対象商品
            user_offer: ユーザー提示価格
            min_acceptable: 最低許容価格

        Returns:
            交渉結果（受諾/拒否、カウンターオファー等）
        """

