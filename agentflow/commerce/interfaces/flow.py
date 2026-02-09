"""フロー抽象インターフェース.

AI商取引フロー（Agentic Commerce Flow）の抽象インターフェース。
検索→発見→比較→カート→決済→完了の完全自動化フロー。

設計原則:
- パイプライン: 各ステップは独立して実行可能
- 状態管理: フローコンテキストで状態を共有
- 拡張可能: 新しいステップを追加可能
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class FlowStepType(str, Enum):
    """フローステップタイプ."""

    INTENT_ANALYSIS = "intent_analysis"     # 意図分析
    PRODUCT_SEARCH = "product_search"       # 商品検索
    PRICE_COMPARE = "price_compare"         # 価格比較
    OFFER_MATCH = "offer_match"             # オファー照合
    RECOMMENDATION = "recommendation"       # 推薦
    CART_MANAGEMENT = "cart_management"     # カート管理
    CHECKOUT = "checkout"                   # チェックアウト
    PAYMENT = "payment"                     # 決済
    ORDER_COMPLETE = "order_complete"       # 注文完了
    CLARIFICATION = "clarification"         # 確認対話


class FlowStatus(str, Enum):
    """フローステータス."""

    NOT_STARTED = "not_started"
    IN_PROGRESS = "in_progress"
    WAITING_INPUT = "waiting_input"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class FlowContext:
    """フローコンテキスト.

    フロー実行中の状態を管理。
    各ステップ間でデータを共有。

    Attributes:
        flow_id: フローID
        session_id: セッションID
        user_id: ユーザーID
        status: フローステータス
        current_step: 現在のステップ
        data: 共有データ
    """

    flow_id: str
    session_id: str | None = None
    user_id: str | None = None
    status: FlowStatus = FlowStatus.NOT_STARTED
    current_step: FlowStepType | None = None

    # 共有データ
    intent: Any = None  # PurchaseIntent
    products: list[Any] = field(default_factory=list)  # list[Product]
    offers: list[Any] = field(default_factory=list)  # list[Offer]
    recommendations: list[Any] = field(default_factory=list)  # list[DirectOffer]
    cart: Any = None  # Cart
    transaction: Any = None  # Transaction

    # メタデータ
    metadata: dict[str, Any] = field(default_factory=dict)
    errors: list[str] = field(default_factory=list)
    step_history: list[str] = field(default_factory=list)


class ICommerceStep(ABC):
    """商取引ステップインターフェース.

    フローの各ステップを表す抽象クラス。
    パイプラインパターンで連結可能。

    Example:
        >>> class IntentAnalysisStep(ICommerceStep):
        ...     async def execute(self, context: FlowContext) -> FlowContext:
        ...         intent = await self.analyzer.analyze(context.metadata["user_input"])
        ...         context.intent = intent
        ...         return context
    """

    @property
    @abstractmethod
    def step_type(self) -> FlowStepType:
        """ステップタイプを取得."""

    @abstractmethod
    async def execute(self, context: FlowContext) -> FlowContext:
        """ステップを実行.

        Args:
            context: フローコンテキスト

        Returns:
            更新後のコンテキスト
        """

    @abstractmethod
    async def validate(self, context: FlowContext) -> bool:
        """実行前検証.

        Args:
            context: フローコンテキスト

        Returns:
            実行可能な場合True
        """

    @abstractmethod
    async def rollback(self, context: FlowContext) -> FlowContext:
        """ロールバック.

        Args:
            context: フローコンテキスト

        Returns:
            ロールバック後のコンテキスト
        """


class ICommerceFlow(ABC):
    """商取引フローインターフェース.

    AI商取引の完全自動化フローを管理。
    FlowBuilderパターンで構築可能。

    Example:
        >>> class AgenticCommerceFlow(ICommerceFlow):
        ...     async def run(self, user_input: str) -> Transaction:
        ...         context = await self.initialize(user_input)
        ...         for step in self.steps:
        ...             context = await step.execute(context)
        ...         return context.transaction
    """

    @abstractmethod
    async def initialize(
        self,
        user_input: str,
        *,
        user_id: str | None = None,
        session_id: str | None = None,
    ) -> FlowContext:
        """フローを初期化.

        Args:
            user_input: ユーザー入力
            user_id: ユーザーID
            session_id: セッションID

        Returns:
            初期化されたコンテキスト
        """

    @abstractmethod
    async def run(self, context: FlowContext) -> FlowContext:
        """フローを実行.

        Args:
            context: フローコンテキスト

        Returns:
            完了後のコンテキスト
        """

    @abstractmethod
    async def run_step(
        self,
        context: FlowContext,
        step_type: FlowStepType,
    ) -> FlowContext:
        """特定ステップを実行.

        Args:
            context: フローコンテキスト
            step_type: ステップタイプ

        Returns:
            更新後のコンテキスト
        """

    @abstractmethod
    def add_step(self, step: ICommerceStep) -> ICommerceFlow:
        """ステップを追加.

        Args:
            step: 追加するステップ

        Returns:
            self（チェーン用）
        """

    @abstractmethod
    async def cancel(self, context: FlowContext) -> FlowContext:
        """フローをキャンセル.

        Args:
            context: フローコンテキスト

        Returns:
            キャンセル後のコンテキスト
        """

    @abstractmethod
    async def get_status(self, flow_id: str) -> FlowStatus:
        """フローステータスを取得.

        Args:
            flow_id: フローID

        Returns:
            フローステータス
        """

