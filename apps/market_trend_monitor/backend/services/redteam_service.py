"""Red Team Challenge サービス.

反証分析の管理を行うビジネスロジック層。
全ての結論に対して反証を試み、確信度を調整します。
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Challenge,
    ChallengeResult,
    ChallengeType,
    Claim,
)


class RedTeamService:
    """反証分析サービス.

    全ての結論に対して反証を試み、確信度を調整します。

    チャレンジタイプ:
    - COUNTER_EVIDENCE: 反証証拠の提示
    - ALTERNATIVE_EXPLANATION: 代替説明の提示
    - BIAS_CHECK: バイアスチェック
    - ASSUMPTION_CHALLENGE: 前提条件への挑戦
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        # インメモリストレージ（将来的にDB化）
        self._challenges: dict[str, Challenge] = {}
        self._results: dict[str, ChallengeResult] = {}

    def create_challenge(
        self,
        claim: Claim,
        challenge_type: ChallengeType,
        argument: str,
        counter_evidence_ids: list[str] | None = None,
    ) -> Challenge:
        """チャレンジを作成.

        Args:
            claim: 対象主張
            challenge_type: チャレンジタイプ
            argument: 反論内容
            counter_evidence_ids: 反証証拠ID

        Returns:
            作成されたチャレンジ
        """
        challenge = Challenge(
            id=str(uuid.uuid4()),
            claim_id=claim.id,
            challenge_type=challenge_type,
            argument=argument,
            counter_evidence_ids=counter_evidence_ids or [],
            created_at=datetime.now(),
        )

        self._challenges[challenge.id] = challenge
        self._logger.info(
            f"Challenge created: {challenge.id}, "
            f"type={challenge_type.value}"
        )
        return challenge

    def evaluate_challenge(
        self,
        challenge: Challenge,
        is_valid: bool,
        impact_score: float,
        evaluator_notes: str = "",
    ) -> ChallengeResult:
        """チャレンジを評価.

        Args:
            challenge: 評価対象チャレンジ
            is_valid: チャレンジが有効か
            impact_score: 影響度スコア（0.0-1.0）
            evaluator_notes: 評価者メモ

        Returns:
            評価結果
        """
        # 信頼度調整を計算
        confidence_adjustment = 0.0
        if is_valid:
            # 有効なチャレンジは信頼度を下げる
            confidence_adjustment = -impact_score * 0.3

        result = ChallengeResult(
            id=str(uuid.uuid4()),
            challenge_id=challenge.id,
            is_valid=is_valid,
            impact_score=impact_score,
            confidence_adjustment=confidence_adjustment,
            evaluator_notes=evaluator_notes,
            evaluated_at=datetime.now(),
        )

        self._results[result.id] = result
        self._logger.info(
            f"Challenge evaluated: {result.id}, "
            f"valid={is_valid}, adjustment={confidence_adjustment:.2f}"
        )
        return result

    def get_challenge(self, challenge_id: str) -> Challenge | None:
        """チャレンジを取得."""
        return self._challenges.get(challenge_id)

    def get_result(self, result_id: str) -> ChallengeResult | None:
        """評価結果を取得."""
        return self._results.get(result_id)

    def list_challenges_for_claim(self, claim_id: str) -> list[Challenge]:
        """主張に対するチャレンジ一覧を取得."""
        return [
            c for c in self._challenges.values()
            if c.claim_id == claim_id
        ]

    def get_adjusted_confidence(
        self,
        claim: Claim,
    ) -> float:
        """チャレンジ結果を反映した調整済み信頼度を取得.

        Args:
            claim: 対象主張

        Returns:
            調整済み信頼度
        """
        challenges = self.list_challenges_for_claim(claim.id)

        total_adjustment = 0.0
        for challenge in challenges:
            # チャレンジに対する結果を検索
            for result in self._results.values():
                if result.challenge_id == challenge.id:
                    total_adjustment += result.confidence_adjustment

        adjusted = claim.confidence + total_adjustment
        return max(0.0, min(1.0, adjusted))

    def generate_challenge_prompts(
        self,
        claim: Claim,
    ) -> list[dict[str, Any]]:
        """主張に対するチャレンジプロンプトを生成.

        LLM に渡すためのプロンプトを生成します。

        Args:
            claim: 対象主張

        Returns:
            チャレンジプロンプトリスト
        """
        prompts = []

        # 反証証拠チャレンジ
        prompts.append({
            "type": ChallengeType.COUNTER_EVIDENCE.value,
            "prompt": (
                f"以下の主張に対する反証を探してください:\n"
                f"主張: {claim.statement}\n"
                f"信頼度: {claim.confidence:.2f}\n"
                f"この主張が誤りである可能性を示す証拠を提示してください。"
            ),
        })

        # 代替説明チャレンジ
        prompts.append({
            "type": ChallengeType.ALTERNATIVE_EXPLANATION.value,
            "prompt": (
                f"以下の主張に対する代替説明を提示してください:\n"
                f"主張: {claim.statement}\n"
                f"同じ証拠から導ける別の結論は何ですか？"
            ),
        })

        # バイアスチェック
        prompts.append({
            "type": ChallengeType.BIAS_CHECK.value,
            "prompt": (
                f"以下の主張に含まれる可能性のあるバイアスを特定してください:\n"
                f"主張: {claim.statement}\n"
                f"確証バイアス、選択バイアス、生存者バイアスなどを検討してください。"
            ),
        })

        # 前提条件チャレンジ
        prompts.append({
            "type": ChallengeType.ASSUMPTION_CHALLENGE.value,
            "prompt": (
                f"以下の主張の前提条件を検証してください:\n"
                f"主張: {claim.statement}\n"
                f"この主張が成り立つために必要な前提条件は何ですか？"
                f"それらの前提条件は妥当ですか？"
            ),
        })

        return prompts

    def get_redteam_stats(self) -> dict[str, Any]:
        """Red Team 統計を取得."""
        challenges = list(self._challenges.values())
        results = list(self._results.values())

        valid_count = sum(1 for r in results if r.is_valid)

        type_counts = {}
        for c in challenges:
            t = c.challenge_type.value
            type_counts[t] = type_counts.get(t, 0) + 1

        avg_impact = 0.0
        if results:
            avg_impact = sum(r.impact_score for r in results) / len(results)

        return {
            "total_challenges": len(challenges),
            "total_results": len(results),
            "valid_challenges": valid_count,
            "challenge_type_distribution": type_counts,
            "average_impact_score": avg_impact,
        }
