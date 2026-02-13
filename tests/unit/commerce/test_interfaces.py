# -*- coding: utf-8 -*-
"""AI商取引インターフェースのテスト.

抽象インターフェースの契約テストとモック実装テスト。
"""

from typing import Any

import pytest

from agentflow.commerce.interfaces import (
    ICart,
    ICommerceAI,
    ICommerceFlow,
    ICommerceStep,
    IDealRecommender,
    IIntentAnalyzer,
    IOffer,
    IOfferProvider,
    IPayment,
    IProduct,
    ITransaction,
)
from agentflow.commerce.interfaces.flow import (
    FlowContext,
    FlowStatus,
    FlowStepType,
)
from agentflow.commerce.models import (
    Cart,
    CartItem,
    DirectOffer,
    IntentType,
    Offer,
    OfferType,
    Product,
    ProductCategory,
    PurchaseIntent,
    Transaction,
)


class TestFlowContext:
    """FlowContext のテスト."""

    def test_flow_context_creation(self) -> None:
        """フローコンテキストの作成が正常に動作すること."""
        context = FlowContext(
            flow_id="flow-001",
            session_id="session-001",
            user_id="user-001",
        )

        assert context.flow_id == "flow-001"
        assert context.status == FlowStatus.NOT_STARTED
        assert context.current_step is None

    def test_flow_context_with_data(self) -> None:
        """データ付きフローコンテキストが正常に動作すること."""
        intent = PurchaseIntent(
            intent_id="intent-001",
            user_input="テスト",
            intent_type=IntentType.PURCHASE,
        )

        context = FlowContext(
            flow_id="flow-001",
            intent=intent,
            status=FlowStatus.IN_PROGRESS,
            current_step=FlowStepType.INTENT_ANALYSIS,
        )

        assert context.intent is not None
        assert context.status == FlowStatus.IN_PROGRESS
        assert context.current_step == FlowStepType.INTENT_ANALYSIS


class TestFlowStepType:
    """FlowStepType のテスト."""

    def test_all_step_types_exist(self) -> None:
        """すべてのステップタイプが定義されていること."""
        expected_types = [
            "intent_analysis",
            "product_search",
            "price_compare",
            "offer_match",
            "recommendation",
            "cart_management",
            "checkout",
            "payment",
            "order_complete",
            "clarification",
        ]

        for step_type in expected_types:
            assert hasattr(FlowStepType, step_type.upper())


class TestFlowStatus:
    """FlowStatus のテスト."""

    def test_all_statuses_exist(self) -> None:
        """すべてのステータスが定義されていること."""
        expected_statuses = [
            "not_started",
            "in_progress",
            "waiting_input",
            "completed",
            "failed",
            "cancelled",
        ]

        for status in expected_statuses:
            assert hasattr(FlowStatus, status.upper())


class TestInterfaceContracts:
    """インターフェース契約のテスト.

    抽象インターフェースが正しく定義されていることを確認。
    """

    def test_iproduct_is_abstract(self) -> None:
        """IProduct が抽象クラスであること."""
        with pytest.raises(TypeError):
            IProduct()  # type: ignore[abstract]

    def test_ioffer_is_abstract(self) -> None:
        """IOffer が抽象クラスであること."""
        with pytest.raises(TypeError):
            IOffer()  # type: ignore[abstract]

    def test_icart_is_abstract(self) -> None:
        """ICart が抽象クラスであること."""
        with pytest.raises(TypeError):
            ICart()  # type: ignore[abstract]

    def test_itransaction_is_abstract(self) -> None:
        """ITransaction が抽象クラスであること."""
        with pytest.raises(TypeError):
            ITransaction()  # type: ignore[abstract]

    def test_ipayment_is_abstract(self) -> None:
        """IPayment が抽象クラスであること."""
        with pytest.raises(TypeError):
            IPayment()  # type: ignore[abstract]

    def test_iintent_analyzer_is_abstract(self) -> None:
        """IIntentAnalyzer が抽象クラスであること."""
        with pytest.raises(TypeError):
            IIntentAnalyzer()  # type: ignore[abstract]

    def test_ioffer_provider_is_abstract(self) -> None:
        """IOfferProvider が抽象クラスであること."""
        with pytest.raises(TypeError):
            IOfferProvider()  # type: ignore[abstract]

    def test_ideal_recommender_is_abstract(self) -> None:
        """IDealRecommender が抽象クラスであること."""
        with pytest.raises(TypeError):
            IDealRecommender()  # type: ignore[abstract]

    def test_icommerce_flow_is_abstract(self) -> None:
        """ICommerceFlow が抽象クラスであること."""
        with pytest.raises(TypeError):
            ICommerceFlow()  # type: ignore[abstract]

    def test_icommerce_step_is_abstract(self) -> None:
        """ICommerceStep が抽象クラスであること."""
        with pytest.raises(TypeError):
            ICommerceStep()  # type: ignore[abstract]

    def test_icommerce_ai_is_abstract(self) -> None:
        """ICommerceAI が抽象クラスであること."""
        with pytest.raises(TypeError):
            ICommerceAI()  # type: ignore[abstract]

