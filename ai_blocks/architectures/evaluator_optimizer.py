"""
Evaluator-Optimizer アーキテクチャパターン

このモジュールは、生成された出力を自己評価し、
品質が不十分な場合は改善指示に基づいて再生成するパターンを実装します。
"""

import asyncio
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional

from ..core import EvaluatorInterface
from ..core.models import EvaluationResult, Message, MessageRole
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class OptimizationStrategy(str, Enum):
    """最適化戦略"""

    ITERATIVE = "iterative"  # 反復改善
    MULTI_CANDIDATE = "multi_candidate"  # 複数候補生成
    HYBRID = "hybrid"  # ハイブリッド


@dataclass
class GenerationAttempt:
    """生成試行のデータクラス"""

    attempt_number: int
    content: str
    evaluation: Optional[EvaluationResult] = None
    generation_time: float = 0.0
    evaluation_time: float = 0.0
    improvement_prompt: Optional[str] = None


class EvaluatorOptimizer(Agent):
    """自己評価による出力最適化エージェント"""

    def __init__(
        self,
        name: str = "EvaluatorOptimizer",
        llm_provider: Any = None,
        evaluator: Optional[EvaluatorInterface] = None,
        strategy: OptimizationStrategy = OptimizationStrategy.ITERATIVE,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Evaluator-Optimizerを初期化する

        Args:
            name: エージェント名
            llm_provider: LLMプロバイダー
            evaluator: 評価器
            strategy: 最適化戦略
            config: 設定辞書
        """
        super().__init__(name, config)

        self.llm = llm_provider
        self.evaluator = evaluator
        self.strategy = strategy

        # 設定値
        self.max_iterations = self.config.get("max_iterations", 3)
        self.min_score_threshold = self.config.get("min_score_threshold", 0.7)
        self.max_candidates = self.config.get("max_candidates", 3)
        self.evaluation_criteria = self.config.get(
            "evaluation_criteria", ["accuracy", "clarity", "completeness", "relevance"]
        )

        if not self.llm:
            raise ValueError("LLMプロバイダーが必要です")
        if not self.evaluator:
            raise ValueError("評価器が必要です")

        logger.info(f"EvaluatorOptimizer '{self.name}' を初期化しました (strategy: {strategy})")

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理して最適化された応答を生成する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 最適化された処理結果
        """
        context = context or {}
        start_time = time.time()

        try:
            if self.strategy == OptimizationStrategy.ITERATIVE:
                result = await self._iterative_optimization(input_text, context)
            elif self.strategy == OptimizationStrategy.MULTI_CANDIDATE:
                result = await self._multi_candidate_optimization(input_text, context)
            elif self.strategy == OptimizationStrategy.HYBRID:
                result = await self._hybrid_optimization(input_text, context)
            else:
                raise ValueError(f"サポートされていない最適化戦略: {self.strategy}")

            total_time = time.time() - start_time
            logger.info(f"最適化完了 (戦略: {self.strategy}, 実行時間: {total_time:.2f}秒)")

            return result

        except Exception as e:
            logger.error(f"最適化処理エラー: {e}")
            raise

    async def _iterative_optimization(
        self, input_text: str, context: Dict[str, Any]
    ) -> str:
        """
        反復改善による最適化

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            str: 最適化された結果
        """
        attempts: List[GenerationAttempt] = []
        current_prompt = input_text

        for iteration in range(self.max_iterations):
            logger.info(f"反復 {iteration + 1}/{self.max_iterations} を開始")

            # 生成
            attempt = await self._generate_content(
                current_prompt, context, iteration + 1
            )

            # 評価
            await self._evaluate_content(attempt, input_text, context)
            attempts.append(attempt)

            # 品質チェック
            if (
                attempt.evaluation is not None
                and attempt.evaluation.passed
                and attempt.evaluation.score >= self.min_score_threshold
            ):
                logger.info(f"品質基準を満たしました (スコア: {attempt.evaluation.score:.3f})")
                return attempt.content

            # 改善プロンプトを生成
            if iteration < self.max_iterations - 1:
                current_prompt = await self._generate_improvement_prompt(
                    input_text, attempt, context
                )

        # 最高スコアの結果を返す
        best_attempt = max(
            attempts, key=lambda a: a.evaluation.score if a.evaluation else 0.0
        )
        logger.info(
            f"最高スコアの結果を返します (スコア: {best_attempt.evaluation.score if best_attempt.evaluation else 0.0:.3f})"
        )

        return best_attempt.content

    async def _multi_candidate_optimization(
        self, input_text: str, context: Dict[str, Any]
    ) -> str:
        """
        複数候補生成による最適化

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            str: 最適化された結果
        """
        logger.info(f"{self.max_candidates} 個の候補を生成します")

        # 複数の候補を並列生成
        generation_tasks = [
            self._generate_content(input_text, context, i + 1)
            for i in range(self.max_candidates)
        ]

        candidates = await asyncio.gather(*generation_tasks)

        # 各候補を評価
        evaluation_tasks = [
            self._evaluate_content(candidate, input_text, context)
            for candidate in candidates
        ]

        await asyncio.gather(*evaluation_tasks)

        # 最高スコアの候補を選択
        best_candidate = max(
            candidates, key=lambda c: c.evaluation.score if c.evaluation else 0.0
        )

        logger.info(
            f"最高スコアの候補を選択 (スコア:{best_candidate.evaluation.score if best_candidate.evaluation else 0.0:.3f})"
        )

        return str(best_candidate.content)

    async def _hybrid_optimization(
        self, input_text: str, context: Dict[str, Any]
    ) -> str:
        """
        ハイブリッド最適化（複数候補 + 反復改善）

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            str: 最適化された結果
        """
        # まず複数候補を生成
        initial_candidates = min(self.max_candidates, 2)
        logger.info(f"初期候補 {initial_candidates} 個を生成します")

        generation_tasks = [
            self._generate_content(input_text, context, i + 1)
            for i in range(initial_candidates)
        ]

        candidates = await asyncio.gather(*generation_tasks)

        # 各候補を評価
        evaluation_tasks = [
            self._evaluate_content(candidate, input_text, context)
            for candidate in candidates
        ]

        await asyncio.gather(*evaluation_tasks)

        # 最高スコアの候補を選択
        best_candidate = max(
            candidates, key=lambda c: c.evaluation.score if c.evaluation else 0.0
        )

        # 品質基準を満たしていれば終了
        if (
            best_candidate.evaluation is not None
            and best_candidate.evaluation.passed
            and best_candidate.evaluation.score >= self.min_score_threshold
        ):
            logger.info(
                f"初期候補が品質基準を満たしました (スコア: {best_candidate.evaluation.score:.3f})"
            )
            return str(best_candidate.content)

        # 反復改善を実行
        logger.info("反復改善を開始します")
        current_prompt = await self._generate_improvement_prompt(
            input_text, best_candidate, context
        )

        for iteration in range(self.max_iterations - 1):
            logger.info(f"改善反復 {iteration + 1}/{self.max_iterations - 1} を開始")

            # 改善版を生成
            improved_attempt = await self._generate_content(
                current_prompt, context, initial_candidates + iteration + 1
            )

            # 評価
            await self._evaluate_content(improved_attempt, input_text, context)

            # より良い結果なら更新
            if (
                improved_attempt.evaluation is not None
                and best_candidate.evaluation is not None
                and improved_attempt.evaluation.score > best_candidate.evaluation.score
            ):
                best_candidate = improved_attempt
                logger.info(
                    f"改善 (スコア: {best_candidate.evaluation.score if best_candidate.evaluation else 0.0:.3f})"
                )

            # 品質基準を満たしたら終了
            if (
                best_candidate.evaluation is not None
                and best_candidate.evaluation.passed
                and best_candidate.evaluation.score >= self.min_score_threshold
            ):
                break

            # 次の改善プロンプトを生成
            if iteration < self.max_iterations - 2:
                current_prompt = await self._generate_improvement_prompt(
                    input_text, best_candidate, context
                )

        return str(best_candidate.content)

    async def _generate_content(
        self, prompt: str, context: Dict[str, Any], attempt_number: int
    ) -> GenerationAttempt:
        """
        コンテンツを生成する

        Args:
            prompt: 生成プロンプト
            context: コンテキスト
            attempt_number: 試行番号

        Returns:
            GenerationAttempt: 生成試行結果
        """
        start_time = time.time()

        # システムメッセージにコンテキスト情報を含める
        system_content = "あなたは高品質な回答を生成する専門アシスタントです。"
        if context:
            system_content += f"\n\n追加コンテキスト: {context}"

        messages = [
            Message(role=MessageRole.SYSTEM, content=system_content),
            Message(role=MessageRole.USER, content=prompt),
        ]

        if self.llm is None:
            raise ValueError("LLMプロバイダーが設定されていません")
        response = await self.llm.generate(messages)
        generation_time = time.time() - start_time

        return GenerationAttempt(
            attempt_number=attempt_number,
            content=str(response.content),
            generation_time=generation_time,
        )

    async def _evaluate_content(
        self, attempt: GenerationAttempt, original_input: str, context: Dict[str, Any]
    ) -> None:
        """
        生成されたコンテンツを評価する

        Args:
            attempt: 評価する生成試行
            original_input: 元の入力
            context: コンテキスト
        """
        start_time = time.time()

        # 元の入力をコンテキストに追加
        evaluation_context = context.copy() if context else {}
        evaluation_context["original_input"] = original_input

        if self.evaluator is None:
            raise ValueError("評価器が設定されていません")

        evaluation = await self.evaluator.evaluate(
            output=attempt.content,
            criteria=self.evaluation_criteria,
            context=evaluation_context,
        )

        attempt.evaluation = evaluation
        attempt.evaluation_time = time.time() - start_time

        logger.debug(
            f"評価完了 (試行 {attempt.attempt_number}): "
            f"スコア {evaluation.score:.3f}, 合格: {evaluation.passed}"
        )

    async def _generate_improvement_prompt(
        self, original_input: str, attempt: GenerationAttempt, context: Dict[str, Any]
    ) -> str:
        """
        改善プロンプトを生成する

        Args:
            original_input: 元の入力
            attempt: 改善対象の試行
            context: コンテキスト

        Returns:
            str: 改善プロンプト
        """
        if attempt.evaluation is None:
            feedback = "評価が実行されていません"
            suggestions = "- 評価を実行してください"
        else:
            feedback = attempt.evaluation.feedback
            suggestions = "\n".join(
                f"- {suggestion}" for suggestion in attempt.evaluation.suggestions
            )

        # コンテキスト情報を追加
        context_info = ""
        if context:
            context_info = f"\n【追加コンテキスト】\n{context}\n"

        improvement_prompt = f"""以下の回答を改善してください：

【元の質問】
{original_input}

【現在の回答】
{attempt.content}

【評価フィードバック】
{feedback}

【改善提案】
{suggestions}{context_info}

【改善指示】
上記のフィードバックと提案を参考に、より高品質な回答を生成してください。
特に評価の低かった点を重点的に改善し、より正確で分かりやすい回答を心がけてください。
"""

        attempt.improvement_prompt = improvement_prompt
        return improvement_prompt
