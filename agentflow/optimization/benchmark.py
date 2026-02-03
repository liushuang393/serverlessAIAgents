"""Benchmark - 性能ベンチマーク（Phase 3.3: 2028）.

【機能】
- Agent性能測定
- 回帰検出
- 比較ベンチマーク
- レポート生成

使用例:
    >>> benchmark = Benchmark()
    >>> result = await benchmark.run(agent, test_suite)
"""

from __future__ import annotations

import logging
import statistics
import time
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol


logger = logging.getLogger(__name__)


class MetricType(Enum):
    """メトリックタイプ."""

    LATENCY = "latency"  # レイテンシ（ms）
    ACCURACY = "accuracy"  # 正確性（%）
    TOKEN_USAGE = "token_usage"  # トークン使用量
    COST = "cost"  # コスト
    THROUGHPUT = "throughput"  # スループット（req/s）


@dataclass
class TestCase:
    """テストケース."""

    case_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    input_data: dict[str, Any] = field(default_factory=dict)
    expected_output: dict[str, Any] | None = None
    tags: list[str] = field(default_factory=list)
    timeout_seconds: float = 30.0


@dataclass
class BenchmarkConfig:
    """ベンチマーク設定."""

    warmup_runs: int = 2
    benchmark_runs: int = 10
    parallel_workers: int = 1
    timeout_seconds: float = 300.0
    metrics: list[MetricType] = field(
        default_factory=lambda: [MetricType.LATENCY, MetricType.ACCURACY]
    )


@dataclass
class BenchmarkResult:
    """ベンチマーク結果."""

    benchmark_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    total_cases: int = 0
    passed_cases: int = 0
    failed_cases: int = 0
    metrics: dict[str, dict[str, float]] = field(default_factory=dict)
    detailed_results: list[dict[str, Any]] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def pass_rate(self) -> float:
        """合格率."""
        if self.total_cases == 0:
            return 0.0
        return self.passed_cases / self.total_cases * 100

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "benchmark_id": self.benchmark_id,
            "name": self.name,
            "timestamp": self.timestamp.isoformat(),
            "total_cases": self.total_cases,
            "passed_cases": self.passed_cases,
            "failed_cases": self.failed_cases,
            "pass_rate": self.pass_rate,
            "metrics": self.metrics,
            "metadata": self.metadata,
        }


class BenchmarkSuite:
    """ベンチマークスイート."""

    def __init__(self, name: str = "") -> None:
        """初期化."""
        self.name = name
        self.test_cases: list[TestCase] = []

    def add_case(self, case: TestCase) -> None:
        """テストケースを追加."""
        self.test_cases.append(case)

    def add_cases(self, cases: list[TestCase]) -> None:
        """複数テストケースを追加."""
        self.test_cases.extend(cases)


class AgentRunner(Protocol):
    """Agentランナープロトコル."""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agentを実行."""
        ...


class Benchmark:
    """ベンチマーク実行器.

    Example:
        >>> benchmark = Benchmark()
        >>> suite = BenchmarkSuite("performance_test")
        >>> suite.add_case(TestCase(input_data={"query": "test"}))
        >>> result = await benchmark.run(agent, suite)
    """

    def __init__(self, config: BenchmarkConfig | None = None) -> None:
        """初期化."""
        self._config = config or BenchmarkConfig()
        self._baseline: BenchmarkResult | None = None

    async def run(
        self,
        agent: AgentRunner,
        suite: BenchmarkSuite,
        name: str = "",
    ) -> BenchmarkResult:
        """ベンチマークを実行."""
        result = BenchmarkResult(name=name or suite.name)
        latencies: list[float] = []
        accuracies: list[float] = []

        # ウォームアップ
        for _ in range(self._config.warmup_runs):
            if suite.test_cases:
                await agent.run(suite.test_cases[0].input_data)

        # ベンチマーク実行
        for case in suite.test_cases:
            result.total_cases += 1
            case_latencies: list[float] = []

            for _ in range(self._config.benchmark_runs):
                start = time.perf_counter()
                try:
                    output = await agent.run(case.input_data)
                    elapsed = (time.perf_counter() - start) * 1000  # ms
                    case_latencies.append(elapsed)

                    # 正確性評価
                    if case.expected_output:
                        accuracy = self._calculate_accuracy(output, case.expected_output)
                        accuracies.append(accuracy)

                except Exception as e:
                    logger.warning(f"Test case {case.case_id} failed: {e}")
                    result.failed_cases += 1
                    continue

            if case_latencies:
                result.passed_cases += 1
                latencies.extend(case_latencies)

                result.detailed_results.append(
                    {
                        "case_id": case.case_id,
                        "name": case.name,
                        "avg_latency_ms": statistics.mean(case_latencies),
                        "p95_latency_ms": self._percentile(case_latencies, 95),
                        "runs": len(case_latencies),
                    }
                )

        # メトリクス集計
        if latencies:
            result.metrics["latency"] = {
                "mean": statistics.mean(latencies),
                "median": statistics.median(latencies),
                "stdev": statistics.stdev(latencies) if len(latencies) > 1 else 0,
                "p95": self._percentile(latencies, 95),
                "p99": self._percentile(latencies, 99),
                "min": min(latencies),
                "max": max(latencies),
            }

        if accuracies:
            result.metrics["accuracy"] = {
                "mean": statistics.mean(accuracies),
                "median": statistics.median(accuracies),
                "min": min(accuracies),
                "max": max(accuracies),
            }

        return result

    def set_baseline(self, baseline: BenchmarkResult) -> None:
        """ベースラインを設定."""
        self._baseline = baseline

    def detect_regression(self, result: BenchmarkResult) -> dict[str, Any]:
        """回帰を検出."""
        if not self._baseline:
            return {"has_regression": False, "message": "No baseline set"}

        regressions: list[dict[str, Any]] = []

        # レイテンシ回帰チェック
        if "latency" in result.metrics and "latency" in self._baseline.metrics:
            current_p95 = result.metrics["latency"]["p95"]
            baseline_p95 = self._baseline.metrics["latency"]["p95"]
            if current_p95 > baseline_p95 * 1.2:  # 20%以上悪化
                regressions.append(
                    {
                        "metric": "latency_p95",
                        "baseline": baseline_p95,
                        "current": current_p95,
                        "degradation": (current_p95 - baseline_p95) / baseline_p95 * 100,
                    }
                )

        # 正確性回帰チェック
        if "accuracy" in result.metrics and "accuracy" in self._baseline.metrics:
            current_acc = result.metrics["accuracy"]["mean"]
            baseline_acc = self._baseline.metrics["accuracy"]["mean"]
            if current_acc < baseline_acc * 0.95:  # 5%以上悪化
                regressions.append(
                    {
                        "metric": "accuracy",
                        "baseline": baseline_acc,
                        "current": current_acc,
                        "degradation": (baseline_acc - current_acc) / baseline_acc * 100,
                    }
                )

        return {
            "has_regression": len(regressions) > 0,
            "regressions": regressions,
            "baseline_id": self._baseline.benchmark_id,
            "current_id": result.benchmark_id,
        }

    def _calculate_accuracy(
        self,
        output: dict[str, Any],
        expected: dict[str, Any],
    ) -> float:
        """正確性を計算."""
        if not expected:
            return 100.0

        matches = 0
        total = len(expected)

        for key, expected_value in expected.items():
            if key in output and output[key] == expected_value:
                matches += 1

        return (matches / total * 100) if total > 0 else 100.0

    def _percentile(self, data: list[float], p: int) -> float:
        """パーセンタイルを計算."""
        if not data:
            return 0.0
        sorted_data = sorted(data)
        k = (len(sorted_data) - 1) * p / 100
        f = int(k)
        c = f + 1 if f + 1 < len(sorted_data) else f
        return sorted_data[f] + (k - f) * (sorted_data[c] - sorted_data[f])


__all__ = [
    "AgentRunner",
    "Benchmark",
    "BenchmarkConfig",
    "BenchmarkResult",
    "BenchmarkSuite",
    "MetricType",
    "TestCase",
]
