"""AutoOptimizer - 自動プロンプト最適化（Phase 3.3: 2028）.

【機能】
- プロンプトの自動最適化
- 多様な候補生成
- 評価と選択
- 反復改善

使用例:
    >>> optimizer = AutoOptimizer()
    >>> result = await optimizer.optimize(agent, test_cases)
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol


logger = logging.getLogger(__name__)


class OptimizationStrategy(Enum):
    """最適化戦略."""

    RANDOM_MUTATION = "random_mutation"  # ランダム変異
    GRADIENT_FREE = "gradient_free"  # 勾配フリー最適化
    EVOLUTIONARY = "evolutionary"  # 進化的最適化
    LLM_GUIDED = "llm_guided"  # LLMガイド最適化


@dataclass
class PromptCandidate:
    """プロンプト候補."""

    candidate_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    prompt: str = ""
    score: float = 0.0
    metrics: dict[str, float] = field(default_factory=dict)
    generation: int = 0
    parent_id: str | None = None
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))


@dataclass
class OptimizationConfig:
    """最適化設定."""

    strategy: OptimizationStrategy = OptimizationStrategy.EVOLUTIONARY
    max_iterations: int = 10
    population_size: int = 5
    mutation_rate: float = 0.3
    elite_ratio: float = 0.2
    target_score: float = 90.0
    timeout_seconds: float = 300.0


@dataclass
class OptimizationResult:
    """最適化結果."""

    best_candidate: PromptCandidate
    all_candidates: list[PromptCandidate]
    iterations: int
    improvement: float
    duration_seconds: float
    metadata: dict[str, Any] = field(default_factory=dict)


class Evaluator(Protocol):
    """評価プロトコル."""

    async def evaluate(self, prompt: str, test_cases: list[dict[str, Any]]) -> float:
        """プロンプトを評価."""
        ...


class AutoOptimizer:
    """自動プロンプト最適化器.

    進化的アルゴリズムを使用してプロンプトを最適化。

    Example:
        >>> optimizer = AutoOptimizer()
        >>> result = await optimizer.optimize(base_prompt, test_cases, evaluator)
    """

    def __init__(
        self,
        config: OptimizationConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初期化.

        Args:
            config: 最適化設定
            llm_client: LLMクライアント（LLMガイド最適化用）
        """
        self._config = config or OptimizationConfig()
        self._llm_client = llm_client

    async def optimize(
        self,
        base_prompt: str,
        test_cases: list[dict[str, Any]],
        evaluator: Evaluator,
    ) -> OptimizationResult:
        """プロンプトを最適化.

        Args:
            base_prompt: 基本プロンプト
            test_cases: テストケース
            evaluator: 評価器

        Returns:
            最適化結果
        """
        import time

        start_time = time.time()

        # 初期集団生成
        population = self._generate_initial_population(base_prompt)

        # 評価
        for candidate in population:
            candidate.score = await evaluator.evaluate(candidate.prompt, test_cases)

        best_candidate = max(population, key=lambda x: x.score)
        initial_score = best_candidate.score
        all_candidates: list[PromptCandidate] = list(population)

        # 反復最適化
        for iteration in range(self._config.max_iterations):
            # 目標スコア達成チェック
            if best_candidate.score >= self._config.target_score:
                logger.info(f"Target score reached at iteration {iteration}")
                break

            # タイムアウトチェック
            if time.time() - start_time > self._config.timeout_seconds:
                logger.warning("Optimization timeout")
                break

            # 次世代生成
            population = self._evolve_population(population, iteration + 1)

            # 評価
            for candidate in population:
                if candidate.score == 0.0:  # 未評価のみ
                    candidate.score = await evaluator.evaluate(candidate.prompt, test_cases)

            all_candidates.extend(population)

            # 最良候補更新
            current_best = max(population, key=lambda x: x.score)
            if current_best.score > best_candidate.score:
                best_candidate = current_best
                logger.info(f"Iteration {iteration}: New best score {best_candidate.score:.2f}")

        return OptimizationResult(
            best_candidate=best_candidate,
            all_candidates=all_candidates,
            iterations=iteration + 1,
            improvement=best_candidate.score - initial_score,
            duration_seconds=time.time() - start_time,
        )

    def _generate_initial_population(self, base_prompt: str) -> list[PromptCandidate]:
        """初期集団を生成."""
        population = [PromptCandidate(prompt=base_prompt, generation=0)]

        # 変異による多様化
        for _ in range(self._config.population_size - 1):
            mutated = self._mutate_prompt(base_prompt)
            population.append(PromptCandidate(prompt=mutated, generation=0))

        return population

    def _evolve_population(
        self,
        population: list[PromptCandidate],
        generation: int,
    ) -> list[PromptCandidate]:
        """集団を進化."""
        # スコア順でソート
        sorted_pop = sorted(population, key=lambda x: x.score, reverse=True)

        # エリート選択
        elite_count = max(1, int(len(sorted_pop) * self._config.elite_ratio))
        elites = sorted_pop[:elite_count]

        new_population: list[PromptCandidate] = []

        # エリートを保持
        for elite in elites:
            new_population.append(
                PromptCandidate(
                    prompt=elite.prompt,
                    score=elite.score,
                    generation=generation,
                    parent_id=elite.candidate_id,
                )
            )

        # 変異で残りを生成
        while len(new_population) < self._config.population_size:
            import random

            parent = random.choice(elites)
            mutated = self._mutate_prompt(parent.prompt)
            new_population.append(
                PromptCandidate(
                    prompt=mutated,
                    generation=generation,
                    parent_id=parent.candidate_id,
                )
            )

        return new_population

    def _mutate_prompt(self, prompt: str) -> str:
        """プロンプトを変異."""
        import random

        mutations = [
            self._add_clarification,
            self._rephrase_section,
            self._add_example,
            self._add_constraint,
            self._simplify,
        ]

        if random.random() < self._config.mutation_rate:
            mutation_fn = random.choice(mutations)
            return mutation_fn(prompt)

        return prompt

    def _add_clarification(self, prompt: str) -> str:
        """明確化を追加."""
        clarifications = [
            "\n\n重要: 回答は具体的かつ簡潔に。",
            "\n\n注意: ステップバイステップで考えてください。",
            "\n\n補足: 不明な点があれば質問してください。",
        ]
        import random

        return prompt + random.choice(clarifications)

    def _rephrase_section(self, prompt: str) -> str:
        """セクションを言い換え."""
        # 簡易実装: 一部の単語を類義語に置換
        replacements = {
            "行う": "実施する",
            "作成する": "生成する",
            "確認する": "検証する",
            "分析する": "調査する",
        }
        result = prompt
        for old, new in replacements.items():
            if old in result:
                result = result.replace(old, new, 1)
                break
        return result

    def _add_example(self, prompt: str) -> str:
        """例を追加."""
        return prompt + "\n\n例: 入力「X」に対して「Y」と回答。"

    def _add_constraint(self, prompt: str) -> str:
        """制約を追加."""
        constraints = [
            "\n\n制約: 200文字以内で回答。",
            "\n\n制約: 専門用語は避ける。",
            "\n\n制約: 箇条書きで回答。",
        ]
        import random

        return prompt + random.choice(constraints)

    def _simplify(self, prompt: str) -> str:
        """簡略化."""
        # 長い文を短くする（簡易実装）
        lines = prompt.split("\n")
        if len(lines) > 5:
            return "\n".join(lines[:5])
        return prompt


__all__ = [
    "AutoOptimizer",
    "Evaluator",
    "OptimizationConfig",
    "OptimizationResult",
    "OptimizationStrategy",
    "PromptCandidate",
]
