"""ABTest - A/Bテスト（Phase 3.3: 2028）.

【機能】
- Agent/プロンプトのA/Bテスト
- 統計的有意性検定
- 自動勝者決定
- 段階的ロールアウト

使用例:
    >>> ab_test = ABTest("prompt_comparison")
    >>> ab_test.add_variant("control", agent_a)
    >>> ab_test.add_variant("treatment", agent_b)
    >>> result = await ab_test.run(test_cases)
"""

from __future__ import annotations

import logging
import random
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol


logger = logging.getLogger(__name__)


class VariantStatus(Enum):
    """バリアント状態."""

    ACTIVE = "active"
    WINNER = "winner"
    LOSER = "loser"
    STOPPED = "stopped"


@dataclass
class Variant:
    """A/Bテストバリアント."""

    variant_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    weight: float = 0.5  # トラフィック配分
    status: VariantStatus = VariantStatus.ACTIVE
    samples: int = 0
    successes: int = 0
    total_score: float = 0.0
    metrics: dict[str, list[float]] = field(default_factory=dict)

    @property
    def success_rate(self) -> float:
        """成功率."""
        if self.samples == 0:
            return 0.0
        return self.successes / self.samples

    @property
    def average_score(self) -> float:
        """平均スコア."""
        if self.samples == 0:
            return 0.0
        return self.total_score / self.samples


@dataclass
class ABTestConfig:
    """A/Bテスト設定."""

    min_samples_per_variant: int = 30
    confidence_level: float = 0.95
    max_duration_seconds: float = 3600.0
    early_stopping: bool = True
    early_stopping_threshold: float = 0.99  # 確実度閾値


@dataclass
class ABTestResult:
    """A/Bテスト結果."""

    test_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    variants: list[Variant] = field(default_factory=list)
    winner: Variant | None = None
    is_significant: bool = False
    p_value: float = 1.0
    confidence: float = 0.0
    duration_seconds: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "test_id": self.test_id,
            "name": self.name,
            "timestamp": self.timestamp.isoformat(),
            "variants": [
                {
                    "name": v.name,
                    "samples": v.samples,
                    "success_rate": v.success_rate,
                    "average_score": v.average_score,
                    "status": v.status.value,
                }
                for v in self.variants
            ],
            "winner": self.winner.name if self.winner else None,
            "is_significant": self.is_significant,
            "p_value": self.p_value,
            "confidence": self.confidence,
            "duration_seconds": self.duration_seconds,
        }


class VariantRunner(Protocol):
    """バリアントランナープロトコル."""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """バリアントを実行."""
        ...


class ABTest:
    """A/Bテスト実行器.

    Example:
        >>> ab_test = ABTest("new_prompt")
        >>> ab_test.add_variant("control", agent_a)
        >>> ab_test.add_variant("treatment", agent_b)
        >>> result = await ab_test.run(test_cases)
    """

    def __init__(
        self,
        name: str,
        config: ABTestConfig | None = None,
    ) -> None:
        """初期化."""
        self.name = name
        self._config = config or ABTestConfig()
        self._variants: dict[str, tuple[Variant, VariantRunner]] = {}

    def add_variant(
        self,
        name: str,
        runner: VariantRunner,
        weight: float = 0.5,
    ) -> Variant:
        """バリアントを追加."""
        variant = Variant(name=name, weight=weight)
        self._variants[name] = (variant, runner)
        return variant

    def select_variant(self) -> tuple[Variant, VariantRunner]:
        """バリアントを選択（重み付きランダム）."""
        variants = list(self._variants.values())
        weights = [v[0].weight for v in variants]
        total = sum(weights)
        r = random.random() * total

        cumulative = 0.0
        for variant, runner in variants:
            cumulative += variant.weight
            if r <= cumulative:
                return variant, runner

        return variants[-1]

    async def run(
        self,
        test_cases: list[dict[str, Any]],
        evaluator: Any = None,
    ) -> ABTestResult:
        """A/Bテストを実行.

        Args:
            test_cases: テストケース
            evaluator: 評価器（オプション）

        Returns:
            テスト結果
        """
        import time

        start_time = time.time()

        # 各テストケースでバリアントを実行
        for case in test_cases:
            for name, (variant, runner) in self._variants.items():
                try:
                    result = await runner.run(case)
                    variant.samples += 1

                    # 成功判定
                    if result.get("success", True):
                        variant.successes += 1

                    # スコア記録
                    score = result.get("score", 0.0)
                    variant.total_score += score

                    # メトリクス記録
                    for key, value in result.items():
                        if isinstance(value, int | float):
                            if key not in variant.metrics:
                                variant.metrics[key] = []
                            variant.metrics[key].append(float(value))

                except Exception as e:
                    logger.warning(f"Variant {name} failed: {e}")

            # 早期停止チェック
            if self._config.early_stopping and self._should_early_stop():
                logger.info("Early stopping triggered")
                break

        # 統計検定
        p_value, is_significant = self._perform_statistical_test()

        # 勝者決定
        winner = self._determine_winner() if is_significant else None

        # バリアント状態更新
        for name, (variant, _) in self._variants.items():
            if winner and variant.name == winner.name:
                variant.status = VariantStatus.WINNER
            elif winner:
                variant.status = VariantStatus.LOSER

        return ABTestResult(
            name=self.name,
            variants=[v for v, _ in self._variants.values()],
            winner=winner,
            is_significant=is_significant,
            p_value=p_value,
            confidence=1 - p_value if p_value < 1 else 0,
            duration_seconds=time.time() - start_time,
        )

    def _should_early_stop(self) -> bool:
        """早期停止すべきか判定."""
        variants = [v for v, _ in self._variants.values()]

        # 最小サンプル数チェック
        for v in variants:
            if v.samples < self._config.min_samples_per_variant:
                return False

        # 明確な勝者がいるか
        success_rates = [v.success_rate for v in variants]
        if len(success_rates) >= 2:
            max_rate = max(success_rates)
            second_max = sorted(success_rates)[-2]
            if max_rate - second_max > 0.1:  # 10%以上の差
                return True

        return False

    def _perform_statistical_test(self) -> tuple[float, bool]:
        """統計検定を実行.

        Returns:
            (p値, 有意か)
        """
        variants = [v for v, _ in self._variants.values()]
        if len(variants) < 2:
            return 1.0, False

        # 簡易的なカイ二乗検定
        v1, v2 = variants[0], variants[1]

        if v1.samples == 0 or v2.samples == 0:
            return 1.0, False

        # 期待値計算
        total_success = v1.successes + v2.successes
        total_samples = v1.samples + v2.samples
        expected_rate = total_success / total_samples

        expected1 = v1.samples * expected_rate
        expected2 = v2.samples * expected_rate

        if expected1 == 0 or expected2 == 0:
            return 1.0, False

        # カイ二乗値
        chi_sq = ((v1.successes - expected1) ** 2 / expected1) + (
            (v2.successes - expected2) ** 2 / expected2
        )

        # 簡易p値近似（自由度1）
        # chi_sq > 3.84 → p < 0.05
        # chi_sq > 6.64 → p < 0.01
        if chi_sq > 6.64:
            p_value = 0.01
        elif chi_sq > 3.84:
            p_value = 0.05
        else:
            p_value = 0.5

        is_significant = p_value < (1 - self._config.confidence_level)
        return p_value, is_significant

    def _determine_winner(self) -> Variant | None:
        """勝者を決定."""
        variants = [v for v, _ in self._variants.values()]
        if not variants:
            return None

        # 成功率が最も高いバリアント
        return max(variants, key=lambda v: v.success_rate)


__all__ = [
    "ABTest",
    "ABTestConfig",
    "ABTestResult",
    "Variant",
    "VariantRunner",
    "VariantStatus",
]
