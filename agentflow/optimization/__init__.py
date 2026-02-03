"""AgentFlow 最適化モジュール（Phase 3.3: 2028）.

【機能】
- 自動プロンプト最適化
- 性能ベンチマーク
- A/Bテスト
- 回帰検出

使用例:
    >>> from agentflow.optimization import AutoOptimizer, Benchmark
    >>> optimizer = AutoOptimizer()
    >>> result = await optimizer.optimize_prompt(agent, test_cases)
"""

from agentflow.optimization.ab_testing import (
    ABTest,
    ABTestConfig,
    ABTestResult,
    Variant,
)
from agentflow.optimization.auto_optimizer import (
    AutoOptimizer,
    OptimizationConfig,
    OptimizationResult,
    PromptCandidate,
)
from agentflow.optimization.benchmark import (
    Benchmark,
    BenchmarkConfig,
    BenchmarkResult,
    BenchmarkSuite,
    TestCase,
)


__all__ = [
    # 自動最適化
    "AutoOptimizer",
    "OptimizationConfig",
    "OptimizationResult",
    "PromptCandidate",
    # ベンチマーク
    "Benchmark",
    "BenchmarkConfig",
    "BenchmarkResult",
    "BenchmarkSuite",
    "TestCase",
    # A/Bテスト
    "ABTest",
    "ABTestConfig",
    "ABTestResult",
    "Variant",
]
