# -*- coding: utf-8 -*-
"""結果検証 - 実行結果の検証と自己修正判断.

実行結果を検証し、期待と比較して自己修正の必要性を判断する。

設計原則:
- 複数の検証戦略をサポート
- 定量的・定性的評価
- 自己修正のための具体的フィードバック

使用例:
    >>> verifier = ResultVerifier(llm_client=my_llm)
    >>> result = await verifier.verify(
    ...     goal=my_goal,
    ...     result=execution_result,
    ...     expected={"quality": 0.8},
    ... )
    >>> if not result.is_acceptable:
    ...     print(f"修正が必要: {result.feedback}")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class VerificationStrategy(str, Enum):
    """検証戦略."""

    EXACT_MATCH = "exact_match"         # 完全一致
    THRESHOLD = "threshold"             # 閾値ベース
    SEMANTIC = "semantic"               # 意味的類似度
    CONSTRAINT = "constraint"           # 制約ベース
    LLM_JUDGE = "llm_judge"             # LLM判定
    COMPOSITE = "composite"             # 複合


class VerificationResult(BaseModel):
    """検証結果.

    Attributes:
        is_acceptable: 合格判定
        score: スコア（0.0-1.0）
        strategy_used: 使用した検証戦略
        feedback: フィードバック
        suggestions: 改善提案
        details: 詳細情報
        should_replan: 再計画が必要か
    """

    is_acceptable: bool = Field(default=True, description="合格判定")
    score: float = Field(default=1.0, ge=0.0, le=1.0, description="スコア")
    strategy_used: VerificationStrategy = Field(
        default=VerificationStrategy.THRESHOLD
    )
    feedback: str = Field(default="", description="フィードバック")
    suggestions: list[str] = Field(default_factory=list, description="改善提案")
    details: dict[str, Any] = Field(default_factory=dict, description="詳細")
    should_replan: bool = Field(default=False, description="再計画が必要か")


@dataclass
class ResultVerifier:
    """結果検証器.

    実行結果を検証し、期待との比較と自己修正の判断を行う。

    主な機能:
    - 複数の検証戦略
    - LLMによる意味的検証
    - 自己修正のための具体的フィードバック
    """

    llm_client: Any = None
    default_strategy: VerificationStrategy = VerificationStrategy.THRESHOLD
    acceptance_threshold: float = 0.7
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.pev.verifier")
    )

    async def verify(
        self,
        goal: str,
        result: dict[str, Any],
        expected: dict[str, Any] | None = None,
        strategy: VerificationStrategy | None = None,
    ) -> VerificationResult:
        """結果を検証.

        Args:
            goal: 目標
            result: 実行結果
            expected: 期待される結果
            strategy: 検証戦略

        Returns:
            VerificationResult
        """
        strategy = strategy or self.default_strategy
        expected = expected or {}

        self._logger.info(f"検証開始: {goal} (strategy={strategy.value})")

        if strategy == VerificationStrategy.EXACT_MATCH:
            verification = self._verify_exact_match(result, expected)
        elif strategy == VerificationStrategy.THRESHOLD:
            verification = self._verify_threshold(result, expected)
        elif strategy == VerificationStrategy.SEMANTIC:
            verification = await self._verify_semantic(goal, result, expected)
        elif strategy == VerificationStrategy.CONSTRAINT:
            verification = self._verify_constraint(result, expected)
        elif strategy == VerificationStrategy.LLM_JUDGE:
            verification = await self._verify_llm_judge(goal, result, expected)
        elif strategy == VerificationStrategy.COMPOSITE:
            verification = await self._verify_composite(goal, result, expected)
        else:
            verification = self._verify_threshold(result, expected)

        verification.strategy_used = strategy

        self._logger.info(
            f"検証完了: 合格={verification.is_acceptable}, "
            f"スコア={verification.score:.2f}"
        )

        return verification

    def _verify_exact_match(
        self,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """完全一致検証."""
        matches = 0
        total = len(expected) if expected else 1
        mismatches = []

        for key, expected_value in expected.items():
            actual_value = result.get(key)
            if actual_value == expected_value:
                matches += 1
            else:
                mismatches.append(f"{key}: 期待={expected_value}, 実際={actual_value}")

        score = matches / total if total > 0 else 1.0

        return VerificationResult(
            is_acceptable=score >= 1.0,
            score=score,
            feedback="; ".join(mismatches) if mismatches else "完全一致",
            suggestions=[f"修正: {m}" for m in mismatches],
        )

    def _verify_threshold(
        self,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """閾値ベース検証."""
        # 結果にスコアが含まれている場合
        score = result.get("score", result.get("quality", result.get("confidence", 1.0)))

        if isinstance(score, (int, float)):
            score = float(score)
            # 0-100スケールを0-1に正規化
            if score > 1.0:
                score = score / 100.0
        else:
            score = 1.0

        is_acceptable = score >= self.acceptance_threshold

        return VerificationResult(
            is_acceptable=is_acceptable,
            score=score,
            feedback=f"スコア {score:.2f} (閾値: {self.acceptance_threshold})",
            suggestions=[] if is_acceptable else ["品質を向上させてください"],
            should_replan=not is_acceptable,
        )

    async def _verify_semantic(
        self,
        goal: str,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """意味的類似度検証."""
        if not self.llm_client:
            # LLMなしの場合はthreshold検証にフォールバック
            return self._verify_threshold(result, expected)

        prompt = f"""以下の目標と結果を比較し、意味的に目標を達成しているか評価してください。

目標: {goal}

期待される結果: {expected}

実際の結果: {result}

JSON形式で回答:
{{
    "score": 0.0-1.0の数値,
    "is_acceptable": true/false,
    "feedback": "評価コメント",
    "suggestions": ["改善提案1", "改善提案2"]
}}"""

        try:
            response = await self.llm_client.generate(prompt)
            content = response.get("content", str(response))

            import json
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)

            return VerificationResult(
                is_acceptable=data.get("is_acceptable", False),
                score=float(data.get("score", 0.0)),
                feedback=data.get("feedback", ""),
                suggestions=data.get("suggestions", []),
                should_replan=not data.get("is_acceptable", False),
            )

        except Exception as e:
            self._logger.warning(f"意味的検証失敗: {e}")
            return self._verify_threshold(result, expected)

    def _verify_constraint(
        self,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """制約ベース検証."""
        constraints = expected.get("constraints", [])
        if not constraints:
            return self._verify_threshold(result, expected)

        violations = []
        satisfied = 0

        for constraint in constraints:
            constraint_type = constraint.get("type", "")
            field = constraint.get("field", "")
            value = result.get(field)

            if constraint_type == "min":
                threshold = constraint.get("value", 0)
                if value is not None and value >= threshold:
                    satisfied += 1
                else:
                    violations.append(f"{field} >= {threshold} (実際: {value})")

            elif constraint_type == "max":
                threshold = constraint.get("value", float("inf"))
                if value is not None and value <= threshold:
                    satisfied += 1
                else:
                    violations.append(f"{field} <= {threshold} (実際: {value})")

            elif constraint_type == "required":
                if value is not None:
                    satisfied += 1
                else:
                    violations.append(f"{field} は必須です")

            elif constraint_type == "in":
                allowed = constraint.get("values", [])
                if value in allowed:
                    satisfied += 1
                else:
                    violations.append(f"{field} は {allowed} のいずれかである必要があります")

        total = len(constraints) if constraints else 1
        score = satisfied / total

        return VerificationResult(
            is_acceptable=len(violations) == 0,
            score=score,
            feedback="; ".join(violations) if violations else "全制約を満たしています",
            suggestions=[f"修正: {v}" for v in violations],
            should_replan=len(violations) > 0,
            details={"satisfied": satisfied, "total": total, "violations": violations},
        )

    async def _verify_llm_judge(
        self,
        goal: str,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """LLM判定検証."""
        if not self.llm_client:
            return self._verify_threshold(result, expected)

        prompt = f"""あなたは厳格な品質審査官です。以下の目標と結果を評価してください。

## 目標
{goal}

## 期待される品質基準
{expected}

## 実際の結果
{result}

## 評価基準
1. 目標達成度 (0-100)
2. 品質 (0-100)
3. 完全性 (0-100)

JSON形式で回答:
{{
    "goal_achievement": 0-100,
    "quality": 0-100,
    "completeness": 0-100,
    "overall_score": 0-100,
    "is_acceptable": true/false,
    "feedback": "詳細な評価コメント",
    "suggestions": ["具体的な改善提案1", "具体的な改善提案2"],
    "should_replan": true/false,
    "replan_reason": "再計画が必要な場合の理由"
}}"""

        try:
            response = await self.llm_client.generate(prompt)
            content = response.get("content", str(response))

            import json
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            data = json.loads(content)

            overall_score = float(data.get("overall_score", 0)) / 100.0

            return VerificationResult(
                is_acceptable=data.get("is_acceptable", False),
                score=overall_score,
                feedback=data.get("feedback", ""),
                suggestions=data.get("suggestions", []),
                should_replan=data.get("should_replan", False),
                details={
                    "goal_achievement": data.get("goal_achievement", 0),
                    "quality": data.get("quality", 0),
                    "completeness": data.get("completeness", 0),
                    "replan_reason": data.get("replan_reason", ""),
                },
            )

        except Exception as e:
            self._logger.warning(f"LLM判定失敗: {e}")
            return self._verify_threshold(result, expected)

    async def _verify_composite(
        self,
        goal: str,
        result: dict[str, Any],
        expected: dict[str, Any],
    ) -> VerificationResult:
        """複合検証 - 複数の検証戦略を組み合わせる."""
        strategies = expected.get("strategies", [
            VerificationStrategy.THRESHOLD,
            VerificationStrategy.CONSTRAINT,
        ])

        weights = expected.get("weights", {})
        default_weight = 1.0 / len(strategies) if strategies else 1.0

        results = []
        total_weight = 0.0
        weighted_score = 0.0

        for strategy in strategies:
            if isinstance(strategy, str):
                strategy = VerificationStrategy(strategy)

            weight = weights.get(strategy.value, default_weight)

            if strategy == VerificationStrategy.EXACT_MATCH:
                ver_result = self._verify_exact_match(result, expected)
            elif strategy == VerificationStrategy.THRESHOLD:
                ver_result = self._verify_threshold(result, expected)
            elif strategy == VerificationStrategy.SEMANTIC:
                ver_result = await self._verify_semantic(goal, result, expected)
            elif strategy == VerificationStrategy.CONSTRAINT:
                ver_result = self._verify_constraint(result, expected)
            elif strategy == VerificationStrategy.LLM_JUDGE:
                ver_result = await self._verify_llm_judge(goal, result, expected)
            else:
                continue

            results.append({
                "strategy": strategy.value,
                "result": ver_result,
                "weight": weight,
            })

            weighted_score += ver_result.score * weight
            total_weight += weight

        # 加重平均スコア
        final_score = weighted_score / total_weight if total_weight > 0 else 0.0

        # 全てのフィードバックと提案を集約
        all_feedback = []
        all_suggestions = []
        should_replan = False

        for r in results:
            ver = r["result"]
            if ver.feedback:
                all_feedback.append(f"[{r['strategy']}] {ver.feedback}")
            all_suggestions.extend(ver.suggestions)
            if ver.should_replan:
                should_replan = True

        return VerificationResult(
            is_acceptable=final_score >= self.acceptance_threshold,
            score=final_score,
            feedback="\n".join(all_feedback),
            suggestions=list(set(all_suggestions)),
            should_replan=should_replan,
            details={
                "strategy_results": [
                    {"strategy": r["strategy"], "score": r["result"].score, "weight": r["weight"]}
                    for r in results
                ]
            },
        )


__all__ = [
    "VerificationStrategy",
    "VerificationResult",
    "ResultVerifier",
]

