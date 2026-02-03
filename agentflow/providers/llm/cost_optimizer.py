"""コスト最適化 - LLMコスト管理と予算制御.

予算内で最適なモデルを選択し、コスト追跡と最適化を提供。

使用例:
    >>> optimizer = CostOptimizer(budget=CostBudget(daily_limit=10.0))
    >>> model = optimizer.select_within_budget(["gpt-4o", "gpt-4o-mini"])
    >>> optimizer.record_usage("gpt-4o-mini", input_tokens=1000, output_tokens=500)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import UTC, datetime
from typing import Any

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class CostBudget(BaseModel):
    """コスト予算設定.

    Attributes:
        daily_limit: 日次コスト上限（USD）
        monthly_limit: 月次コスト上限（USD）
        per_request_limit: リクエストあたりの上限（USD）
        warning_threshold: 警告閾値（0.0-1.0、予算の何％で警告）
    """

    daily_limit: float = Field(default=10.0, description="日次コスト上限（USD）")
    monthly_limit: float = Field(default=300.0, description="月次コスト上限（USD）")
    per_request_limit: float = Field(default=1.0, description="リクエストあたりの上限（USD）")
    warning_threshold: float = Field(
        default=0.8, ge=0.0, le=1.0, description="警告閾値（予算の何％で警告）"
    )


@dataclass
class UsageRecord:
    """使用量記録.

    Attributes:
        model: モデル名
        timestamp: タイムスタンプ
        input_tokens: 入力トークン数
        output_tokens: 出力トークン数
        cost: コスト（USD）
    """

    model: str
    timestamp: datetime
    input_tokens: int
    output_tokens: int
    cost: float


@dataclass
class CostSummary:
    """コストサマリー.

    Attributes:
        daily_cost: 日次コスト
        monthly_cost: 月次コスト
        total_cost: 総コスト
        daily_remaining: 日次残予算
        monthly_remaining: 月次残予算
        is_over_budget: 予算超過フラグ
        is_warning: 警告フラグ
    """

    daily_cost: float = 0.0
    monthly_cost: float = 0.0
    total_cost: float = 0.0
    daily_remaining: float = 0.0
    monthly_remaining: float = 0.0
    is_over_budget: bool = False
    is_warning: bool = False


class CostOptimizer:
    """コスト最適化器.

    予算管理、コスト追跡、最適モデル選択を提供。

    使用例:
        >>> optimizer = CostOptimizer(budget=CostBudget(daily_limit=10.0))
        >>> # 予算内で最適なモデルを選択
        >>> model = optimizer.select_within_budget(["gpt-4o", "gpt-4o-mini"])
        >>> # 使用量を記録
        >>> optimizer.record_usage("gpt-4o-mini", input_tokens=1000, output_tokens=500)
        >>> # サマリー取得
        >>> summary = optimizer.get_summary()
    """

    def __init__(self, budget: CostBudget | None = None) -> None:
        """初期化.

        Args:
            budget: コスト予算設定
        """
        self._budget = budget or CostBudget()
        self._usage_records: list[UsageRecord] = []

        logger.info(
            f"CostOptimizer初期化: daily_limit=${self._budget.daily_limit}, "
            f"monthly_limit=${self._budget.monthly_limit}"
        )

    def select_within_budget(
        self,
        candidates: list[str],
        estimated_tokens: int = 1000,
    ) -> str | None:
        """予算内で最適なモデルを選択.

        Args:
            candidates: 候補モデルリスト
            estimated_tokens: 推定トークン数

        Returns:
            選択されたモデル名（予算超過時はNone）
        """
        from agentflow.llm.models import MODELS

        summary = self.get_summary()
        if summary.is_over_budget:
            logger.warning("予算超過のため、モデル選択を拒否")
            return None

        # コストでソート
        candidates_with_cost = []
        for model in candidates:
            model_info = MODELS.get(model)
            if not model_info:
                continue

            estimated_cost = (
                model_info.input_cost_per_1k * estimated_tokens / 1000
                + model_info.output_cost_per_1k * estimated_tokens / 1000
            )

            # リクエストあたりの上限チェック
            if estimated_cost > self._budget.per_request_limit:
                continue

            # 日次残予算チェック
            if estimated_cost > summary.daily_remaining:
                continue

            candidates_with_cost.append((model, estimated_cost))

        if not candidates_with_cost:
            logger.warning("予算内で利用可能なモデルがありません")
            return None

        # 最も安いモデルを選択
        selected = min(candidates_with_cost, key=lambda x: x[1])
        logger.info(f"予算内で選択: {selected[0]}（推定コスト: ${selected[1]:.4f}）")
        return selected[0]

    def record_usage(
        self,
        model: str,
        input_tokens: int,
        output_tokens: int,
    ) -> float:
        """使用量を記録.

        Args:
            model: モデル名
            input_tokens: 入力トークン数
            output_tokens: 出力トークン数

        Returns:
            計算されたコスト（USD）
        """
        from agentflow.llm.models import MODELS

        model_info = MODELS.get(model)
        if not model_info:
            logger.warning(f"モデル {model} の情報が見つかりません")
            cost = 0.0
        else:
            cost = (
                model_info.input_cost_per_1k * input_tokens / 1000
                + model_info.output_cost_per_1k * output_tokens / 1000
            )

        record = UsageRecord(
            model=model,
            timestamp=datetime.now(UTC),
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            cost=cost,
        )
        self._usage_records.append(record)

        logger.info(
            f"使用量記録: {model}, in={input_tokens}, out={output_tokens}, cost=${cost:.4f}"
        )

        # 警告チェック
        summary = self.get_summary()
        if summary.is_warning:
            logger.warning(
                f"予算警告: 日次{summary.daily_cost:.2f}/{self._budget.daily_limit:.2f}, "
                f"月次{summary.monthly_cost:.2f}/{self._budget.monthly_limit:.2f}"
            )

        return cost

    def get_summary(self) -> CostSummary:
        """コストサマリーを取得.

        Returns:
            コストサマリー
        """
        now = datetime.now(UTC)
        today_start = now.replace(hour=0, minute=0, second=0, microsecond=0)
        month_start = now.replace(day=1, hour=0, minute=0, second=0, microsecond=0)

        daily_cost = sum(r.cost for r in self._usage_records if r.timestamp >= today_start)
        monthly_cost = sum(r.cost for r in self._usage_records if r.timestamp >= month_start)
        total_cost = sum(r.cost for r in self._usage_records)

        daily_remaining = max(0, self._budget.daily_limit - daily_cost)
        monthly_remaining = max(0, self._budget.monthly_limit - monthly_cost)

        is_over_budget = (
            daily_cost >= self._budget.daily_limit or monthly_cost >= self._budget.monthly_limit
        )
        is_warning = (
            daily_cost >= self._budget.daily_limit * self._budget.warning_threshold
            or monthly_cost >= self._budget.monthly_limit * self._budget.warning_threshold
        )

        return CostSummary(
            daily_cost=daily_cost,
            monthly_cost=monthly_cost,
            total_cost=total_cost,
            daily_remaining=daily_remaining,
            monthly_remaining=monthly_remaining,
            is_over_budget=is_over_budget,
            is_warning=is_warning,
        )

    def get_usage_by_model(self) -> dict[str, dict[str, Any]]:
        """モデル別使用量を取得.

        Returns:
            モデル別使用量辞書
        """
        usage: dict[str, dict[str, Any]] = {}
        for record in self._usage_records:
            if record.model not in usage:
                usage[record.model] = {
                    "total_input_tokens": 0,
                    "total_output_tokens": 0,
                    "total_cost": 0.0,
                    "request_count": 0,
                }
            usage[record.model]["total_input_tokens"] += record.input_tokens
            usage[record.model]["total_output_tokens"] += record.output_tokens
            usage[record.model]["total_cost"] += record.cost
            usage[record.model]["request_count"] += 1

        return usage

    def reset_daily(self) -> None:
        """日次使用量をリセット（テスト用）."""
        now = datetime.now(UTC)
        today_start = now.replace(hour=0, minute=0, second=0, microsecond=0)
        self._usage_records = [r for r in self._usage_records if r.timestamp < today_start]
        logger.info("日次使用量をリセット")

    def estimate_cost(
        self,
        model: str,
        input_tokens: int,
        output_tokens: int,
    ) -> float:
        """コストを推定.

        Args:
            model: モデル名
            input_tokens: 入力トークン数
            output_tokens: 出力トークン数

        Returns:
            推定コスト（USD）
        """
        from agentflow.llm.models import MODELS

        model_info = MODELS.get(model)
        if not model_info:
            return 0.0

        return (
            model_info.input_cost_per_1k * input_tokens / 1000
            + model_info.output_cost_per_1k * output_tokens / 1000
        )
