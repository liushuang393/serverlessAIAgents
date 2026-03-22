"""commerce flow 契約.

commerce のフロー境界は protocol 実装ではなく flow 契約として管理する。
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class FlowStepType(str, Enum):
    """commerce フローステップ種別."""

    INTENT_ANALYSIS = "intent_analysis"
    PRODUCT_SEARCH = "product_search"
    PRICE_COMPARE = "price_compare"
    OFFER_MATCH = "offer_match"
    RECOMMENDATION = "recommendation"
    CART_MANAGEMENT = "cart_management"
    CHECKOUT = "checkout"
    PAYMENT = "payment"
    ORDER_COMPLETE = "order_complete"
    CLARIFICATION = "clarification"


class CommerceFlowStatus(str, Enum):
    """commerce フロー実行状態."""

    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    WAITING_INPUT = "waiting_input"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class CommerceFlowContext:
    """commerce フローの共有状態."""

    flow_id: str
    session_id: str | None = None
    user_id: str | None = None
    status: CommerceFlowStatus = CommerceFlowStatus.NOT_STARTED
    current_step: FlowStepType | None = None
    intent: Any = None
    products: list[Any] = field(default_factory=list)
    offers: list[Any] = field(default_factory=list)
    recommendations: list[Any] = field(default_factory=list)
    cart: Any = None
    transaction: Any = None
    metadata: dict[str, Any] = field(default_factory=dict)
    errors: list[str] = field(default_factory=list)
    step_history: list[str] = field(default_factory=list)


class ICommerceStep(ABC):
    """commerce ステップの抽象契約."""

    @property
    @abstractmethod
    def step_type(self) -> FlowStepType:
        """ステップ種別を返す."""

    @abstractmethod
    async def execute(self, context: CommerceFlowContext) -> CommerceFlowContext:
        """ステップを実行する."""

    @abstractmethod
    async def validate(self, context: CommerceFlowContext) -> bool:
        """ステップ実行可否を判定する."""

    @abstractmethod
    async def rollback(self, context: CommerceFlowContext) -> CommerceFlowContext:
        """ステップをロールバックする."""


class ICommerceFlow(ABC):
    """commerce フローの抽象契約."""

    @abstractmethod
    async def initialize(
        self,
        user_input: str,
        *,
        user_id: str | None = None,
        session_id: str | None = None,
    ) -> CommerceFlowContext:
        """フローを初期化する."""

    @abstractmethod
    async def run(self, context: CommerceFlowContext) -> CommerceFlowContext:
        """フローを実行する."""

    @abstractmethod
    async def run_step(
        self,
        context: CommerceFlowContext,
        step_type: FlowStepType,
    ) -> CommerceFlowContext:
        """単一ステップを実行する."""

    @abstractmethod
    def add_step(self, step: ICommerceStep) -> ICommerceFlow:
        """ステップを追加する."""

    @abstractmethod
    async def cancel(self, context: CommerceFlowContext) -> CommerceFlowContext:
        """フローをキャンセルする."""

    @abstractmethod
    async def get_status(self, flow_id: str) -> CommerceFlowStatus:
        """現在のフロー状態を返す."""
