"""
Evaluator（評価器）コンポーネント

このモジュールは、出力品質チェックと改善指示のための抽象インターフェースと
具体的な実装を提供します。
"""

import json
import re
from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

from ..utils.logging import get_logger
from .models import EvaluationResult

logger = get_logger(__name__)


class EvaluationCriteria(str, Enum):
    """評価基準"""

    RELEVANCE = "relevance"  # 関連性
    ACCURACY = "accuracy"  # 正確性
    COMPLETENESS = "completeness"  # 完全性
    CLARITY = "clarity"  # 明確性
    COHERENCE = "coherence"  # 一貫性
    SAFETY = "safety"  # 安全性
    HELPFULNESS = "helpfulness"  # 有用性
    CONCISENESS = "conciseness"  # 簡潔性


class EvaluatorInterface(ABC):
    """出力品質チェックと改善指示のための抽象インターフェース"""

    @abstractmethod
    async def evaluate(
        self, output: str, criteria: List[str], context: Optional[Dict[str, Any]] = None
    ) -> EvaluationResult:
        """
        出力を評価する

        Args:
            output: 評価する出力
            criteria: 評価基準のリスト
            context: 評価のコンテキスト（入力、期待値など）

        Returns:
            EvaluationResult: 評価結果
        """
        pass

    @abstractmethod
    async def suggest_improvements(
        self, output: str, evaluation: EvaluationResult, context: Dict[str, Any] = None
    ) -> List[str]:
        """
        改善提案を生成する

        Args:
            output: 改善対象の出力
            evaluation: 評価結果
            context: 改善のコンテキスト

        Returns:
            List[str]: 改善提案のリスト
        """
        pass


class RuleBasedEvaluator(EvaluatorInterface):
    """ルールベースの評価器"""

    def __init__(self, passing_threshold: float = 0.7):
        """
        ルールベースの評価器を初期化する

        Args:
            passing_threshold: 合格とする閾値（0.0-1.0）
        """
        self.passing_threshold = passing_threshold
        self._evaluation_rules: Dict[str, Callable] = {}

        # デフォルトの評価ルールを登録
        self._register_default_rules()

        logger.info(f"ルールベースの評価器を初期化しました（合格閾値: {passing_threshold}）")

    async def evaluate(
        self, output: str, criteria: List[str], context: Optional[Dict[str, Any]] = None
    ) -> EvaluationResult:
        """
        出力を評価する

        Args:
            output: 評価する出力
            criteria: 評価基準のリスト
            context: 評価のコンテキスト

        Returns:
            EvaluationResult: 評価結果
        """
        if not output.strip():
            return EvaluationResult(
                score=0.0,
                criteria_scores={},
                feedback="出力が空です",
                passed=False,
                suggestions=["出力を生成してください"],
            )

        context = context or {}
        criteria_scores = {}
        feedback_parts = []

        # 各基準で評価
        for criterion in criteria:
            if criterion in self._evaluation_rules:
                try:
                    score = await self._evaluation_rules[criterion](output, context)
                    criteria_scores[criterion] = score

                    if score < self.passing_threshold:
                        feedback_parts.append(f"{criterion}: {score:.2f} (改善が必要)")
                    else:
                        feedback_parts.append(f"{criterion}: {score:.2f} (良好)")

                except Exception as e:
                    logger.warning(f"評価基準 '{criterion}' の実行中にエラーが発生しました: {e}")
                    criteria_scores[criterion] = 0.0
                    feedback_parts.append(f"{criterion}: エラー")
            else:
                logger.warning(f"未知の評価基準です: {criterion}")
                criteria_scores[criterion] = 0.5  # デフォルトスコア
                feedback_parts.append(f"{criterion}: 未実装")

        # 総合スコアを計算
        if criteria_scores:
            total_score = sum(criteria_scores.values()) / len(criteria_scores)
        else:
            total_score = 0.0

        # 合格判定
        passed = total_score >= self.passing_threshold

        # フィードバックを作成
        feedback = f"総合スコア: {total_score:.2f}\n" + "\n".join(feedback_parts)

        logger.debug(f"評価完了: スコア={total_score:.2f}, 合格={passed}")

        return EvaluationResult(
            score=total_score,
            criteria_scores=criteria_scores,
            feedback=feedback,
            passed=passed,
            suggestions=[],  # suggest_improvementsで生成
        )

    async def suggest_improvements(
        self, output: str, evaluation: EvaluationResult, context: Dict[str, Any] = None
    ) -> List[str]:
        """
        改善提案を生成する

        Args:
            output: 改善対象の出力
            evaluation: 評価結果
            context: 改善のコンテキスト

        Returns:
            List[str]: 改善提案のリスト
        """
        suggestions = []

        # 低スコアの基準に対する改善提案
        for criterion, score in evaluation.criteria_scores.items():
            if score < self.passing_threshold:
                suggestion = await self._get_improvement_suggestion(
                    criterion, score, output, context or {}
                )
                if suggestion:
                    suggestions.append(suggestion)

        # 一般的な改善提案
        if evaluation.score < 0.3:
            suggestions.append("出力の品質が低いため、全体的な見直しが必要です")
        elif evaluation.score < 0.5:
            suggestions.append("出力の一部を改善することで品質を向上できます")

        return suggestions

    def register_evaluation_rule(self, criterion: str, rule_function: Callable) -> None:
        """
        評価ルールを登録する

        Args:
            criterion: 評価基準名
            rule_function: 評価関数（output, context -> float）
        """
        self._evaluation_rules[criterion] = rule_function
        logger.info(f"評価ルール '{criterion}' を登録しました")

    def _register_default_rules(self) -> None:
        """デフォルトの評価ルールを登録する"""

        async def evaluate_length(output: str, context: Dict[str, Any]) -> float:
            """長さの評価"""
            min_length = context.get("min_length", 10)
            max_length = context.get("max_length", 1000)

            length = len(output)
            if length < min_length:
                return float(max(0.0, length / min_length))
            elif length > max_length:
                return float(max(0.0, 1.0 - (length - max_length) / max_length))
            else:
                return 1.0

        async def evaluate_clarity(output: str, context: Dict[str, Any]) -> float:
            """明確性の評価"""
            # 簡単な明確性指標
            sentences = re.split(r"[.!?。！？]+", output)
            if not sentences:
                return 0.0

            # 平均文長をチェック
            avg_sentence_length = sum(
                len(s.strip()) for s in sentences if s.strip()
            ) / len([s for s in sentences if s.strip()])

            # 適度な文長（20-100文字）を良好とする
            if 20 <= avg_sentence_length <= 100:
                return 1.0
            elif avg_sentence_length < 20:
                return max(0.3, avg_sentence_length / 20)
            else:
                return max(0.3, 100 / avg_sentence_length)

        async def evaluate_safety(output: str, context: Dict[str, Any]) -> float:
            """安全性の評価"""
            # 危険なキーワードをチェック
            dangerous_keywords = [
                "暴力",
                "violence",
                "殺",
                "kill",
                "死",
                "death",
                "自殺",
                "suicide",
                "薬物",
                "drug",
                "違法",
                "illegal",
            ]

            output_lower = output.lower()
            dangerous_count = sum(
                1 for keyword in dangerous_keywords if keyword in output_lower
            )

            if dangerous_count == 0:
                return 1.0
            else:
                return max(0.0, 1.0 - dangerous_count * 0.3)

        async def evaluate_relevance(output: str, context: Dict[str, Any]) -> float:
            """関連性の評価"""
            query = context.get("query", "")
            if not query:
                return 0.8  # クエリがない場合はデフォルトスコア

            # 簡単なキーワードマッチング
            query_words = set(re.findall(r"\w+", query.lower()))
            output_words = set(re.findall(r"\w+", output.lower()))

            if not query_words:
                return 0.8

            overlap = len(query_words.intersection(output_words))
            return min(1.0, overlap / len(query_words))

        # ルールを登録
        self._evaluation_rules[EvaluationCriteria.CLARITY] = evaluate_clarity
        self._evaluation_rules[EvaluationCriteria.SAFETY] = evaluate_safety
        self._evaluation_rules[EvaluationCriteria.RELEVANCE] = evaluate_relevance
        self._evaluation_rules["length"] = evaluate_length

    async def _get_improvement_suggestion(
        self, criterion: str, score: float, output: str, context: Dict[str, Any]
    ) -> Optional[str]:
        """基準別の改善提案を取得する"""
        suggestions_map = {
            EvaluationCriteria.CLARITY: "文章をより明確で理解しやすくしてください",
            EvaluationCriteria.RELEVANCE: "質問により関連した内容を含めてください",
            EvaluationCriteria.SAFETY: "安全でない内容を削除または修正してください",
            EvaluationCriteria.COMPLETENESS: "回答をより完全なものにしてください",
            EvaluationCriteria.ACCURACY: "情報の正確性を確認してください",
            EvaluationCriteria.COHERENCE: "論理的な一貫性を改善してください",
            EvaluationCriteria.HELPFULNESS: "より有用な情報を提供してください",
            EvaluationCriteria.CONCISENESS: "より簡潔に表現してください",
            "length": "適切な長さに調整してください",
        }

        return suggestions_map.get(criterion)


class LLMEvaluator(EvaluatorInterface):
    """LLMベースの評価器"""

    def __init__(self, llm_provider: Any = None, passing_threshold: float = 0.7):
        """
        LLMベースの評価器を初期化する

        Args:
            llm_provider: LLMプロバイダー
            passing_threshold: 合格とする閾値（0.0-1.0）
        """
        self.llm_provider = llm_provider
        self.passing_threshold = passing_threshold

        logger.info(f"LLMベースの評価器を初期化しました（合格閾値: {passing_threshold}）")

    async def evaluate(
        self, output: str, criteria: List[str], context: Optional[Dict[str, Any]] = None
    ) -> EvaluationResult:
        """
        出力を評価する

        Args:
            output: 評価する出力
            criteria: 評価基準のリスト
            context: 評価のコンテキスト

        Returns:
            EvaluationResult: 評価結果
        """
        if not self.llm_provider:
            logger.warning("LLMプロバイダーが設定されていません。デフォルト評価を返します。")
            return EvaluationResult(
                score=0.5,
                criteria_scores={criterion: 0.5 for criterion in criteria},
                feedback="LLMプロバイダーが設定されていません",
                passed=False,
                suggestions=["LLMプロバイダーを設定してください"],
            )

        if not output.strip():
            return EvaluationResult(
                score=0.0,
                criteria_scores={criterion: 0.0 for criterion in criteria},
                feedback="出力が空です",
                passed=False,
                suggestions=["出力を生成してください"],
            )

        try:
            # LLMに評価を依頼
            prompt = self._create_evaluation_prompt(output, criteria, context)
            response = await self.llm_provider.generate(prompt)

            # レスポンスを解析
            result = self._parse_evaluation_response(response, criteria)

            logger.debug(f"LLM評価完了: スコア={result.score:.2f}, 合格={result.passed}")
            return result

        except Exception as e:
            logger.error(f"LLM評価中にエラーが発生しました: {e}")
            return EvaluationResult(
                score=0.0,
                criteria_scores={criterion: 0.0 for criterion in criteria},
                feedback=f"評価エラー: {str(e)}",
                passed=False,
                suggestions=["評価システムを確認してください"],
            )

    async def suggest_improvements(
        self, output: str, evaluation: EvaluationResult, context: Dict[str, Any] = None
    ) -> List[str]:
        """
        改善提案を生成する

        Args:
            output: 改善対象の出力
            evaluation: 評価結果
            context: 改善のコンテキスト

        Returns:
            List[str]: 改善提案のリスト
        """
        if not self.llm_provider:
            return ["LLMプロバイダーが設定されていないため、改善提案を生成できません"]

        try:
            # LLMに改善提案を依頼
            prompt = self._create_improvement_prompt(output, evaluation, context or {})
            response = await self.llm_provider.generate(prompt)

            # レスポンスを解析
            suggestions = self._parse_improvement_response(response)

            logger.debug(f"LLM改善提案生成完了: {len(suggestions)}件")
            return suggestions

        except Exception as e:
            logger.error(f"LLM改善提案生成中にエラーが発生しました: {e}")
            return [f"改善提案生成エラー: {str(e)}"]

    def _create_evaluation_prompt(
        self, output: str, criteria: List[str], context: Optional[Dict[str, Any]]
    ) -> str:
        """評価用のプロンプトを作成する"""
        context = context or {}

        prompt = f"""
以下の出力を指定された基準で評価してください。

出力:
"{output}"

評価基準: {', '.join(criteria)}

コンテキスト:
{json.dumps(context, ensure_ascii=False, indent=2)}

各基準について0.0-1.0のスコアを付け、以下の形式でJSONレスポンスを返してください:
{{
    "criteria_scores": {{
        "基準1": スコア,
        "基準2": スコア,
        ...
    }},
    "overall_score": 総合スコア,
    "feedback": "詳細なフィードバック",
    "passed": true/false
}}
"""

        return prompt

    def _create_improvement_prompt(
        self, output: str, evaluation: EvaluationResult, context: Dict[str, Any]
    ) -> str:
        """改善提案用のプロンプトを作成する"""
        context = context or {}

        prompt = f"""
以下の出力の改善提案を生成してください。

出力:
"{output}"

評価結果:
- 総合スコア: {evaluation.score:.2f}
- 合格: {evaluation.passed}
- フィードバック: {evaluation.feedback}

基準別スコア:
{json.dumps(evaluation.criteria_scores, ensure_ascii=False, indent=2)}

具体的で実行可能な改善提案を配列形式で返してください:
["改善提案1", "改善提案2", ...]
"""

        return prompt

    def _parse_evaluation_response(
        self, response: str, criteria: List[str]
    ) -> EvaluationResult:
        """評価レスポンスを解析する"""
        try:
            data = json.loads(response.strip())

            criteria_scores = data.get("criteria_scores", {})
            overall_score = data.get("overall_score", 0.0)
            feedback = data.get("feedback", "")
            passed = data.get("passed", overall_score >= self.passing_threshold)

            return EvaluationResult(
                score=float(overall_score),
                criteria_scores={k: float(v) for k, v in criteria_scores.items()},
                feedback=feedback,
                passed=bool(passed),
                suggestions=[],
            )

        except (json.JSONDecodeError, ValueError, KeyError) as e:
            logger.warning(f"評価レスポンスの解析に失敗しました: {e}")
            return EvaluationResult(
                score=0.0,
                criteria_scores={criterion: 0.0 for criterion in criteria},
                feedback="レスポンス解析エラー",
                passed=False,
                suggestions=[],
            )

    def _parse_improvement_response(self, response: str) -> List[str]:
        """改善提案レスポンスを解析する"""
        try:
            suggestions = json.loads(response.strip())
            if isinstance(suggestions, list):
                return [str(s) for s in suggestions]
            else:
                return [str(suggestions)]

        except (json.JSONDecodeError, ValueError) as e:
            logger.warning(f"改善提案レスポンスの解析に失敗しました: {e}")
            # フォールバック: 行ごとに分割
            lines = response.strip().split("\n")
            return [line.strip("- ").strip() for line in lines if line.strip()]
