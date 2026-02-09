"""Red Team エージェント.

全ての結論に対して反証を試み、確信度を調整します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    ChallengeResultSchema,
    ChallengeSchema,
    ChallengeType,
    Claim,
    ClaimLevel,
    ClaimSchema,
)
from apps.market_trend_monitor.backend.services.redteam_service import (
    RedTeamService,
)
from pydantic import BaseModel, Field

from agentflow import ResilientAgent


# ============================================================
# Agent I/O スキーマ
# ============================================================


class RedTeamInput(BaseModel):
    """RedTeamAgent 入力スキーマ."""

    claims: list[ClaimSchema] = Field(default_factory=list)
    challenge_types: list[str] = Field(
        default_factory=lambda: [
            ChallengeType.COUNTER_EVIDENCE.value,
            ChallengeType.ALTERNATIVE_EXPLANATION.value,
        ],
        description="実行するチャレンジタイプ",
    )


class RedTeamOutput(BaseModel):
    """RedTeamAgent 出力スキーマ."""

    challenges: list[ChallengeSchema] = Field(default_factory=list)
    results: list[ChallengeResultSchema] = Field(default_factory=list)
    adjusted_claims: list[ClaimSchema] = Field(default_factory=list)
    total_challenges: int = 0
    valid_challenges: int = 0
    stats: dict = Field(default_factory=dict)


# ============================================================
# Agent 実装
# ============================================================


class RedTeamAgent(ResilientAgent[RedTeamInput, RedTeamOutput]):
    """Red Team エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 主張に対する反証分析
    - チャレンジの生成と評価
    - 信頼度の調整

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "RedTeamAgent"
    temperature = 0.7  # 創造的な反証には高めの温度

    def __init__(
        self,
        llm_client: Any = None,
        redteam_service: RedTeamService | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント（None の場合は自動取得）
            redteam_service: Red Team サービス（None の場合は新規作成）
        """
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._redteam_service = redteam_service or RedTeamService()

    def _parse_input(self, input_data: dict[str, Any]) -> RedTeamInput:
        """入力データを Pydantic モデルに変換."""
        return RedTeamInput(**input_data)

    async def process(self, input_data: RedTeamInput) -> RedTeamOutput:
        """反証分析を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き分析結果
        """
        claims = input_data.claims
        challenge_types = [
            ChallengeType(t) for t in input_data.challenge_types
        ]

        self._logger.info(
            f"Red Team analysis for {len(claims)} claims, "
            f"{len(challenge_types)} challenge types"
        )

        all_challenges: list[ChallengeSchema] = []
        all_results: list[ChallengeResultSchema] = []
        adjusted_claims: list[ClaimSchema] = []
        valid_count = 0

        for claim_schema in claims:
            # ClaimSchema から Claim に変換
            claim = self._schema_to_claim(claim_schema)

            # 各チャレンジタイプで分析
            for challenge_type in challenge_types:
                # チャレンジを生成
                challenge = self._redteam_service.create_challenge(
                    claim=claim,
                    challenge_type=challenge_type,
                    argument=f"Auto-generated challenge for {claim.statement}",
                )

                # 簡易評価（実際はLLMで評価）
                is_valid = claim.confidence < 0.8
                impact = 0.3 if is_valid else 0.1

                result = self._redteam_service.evaluate_challenge(
                    challenge=challenge,
                    is_valid=is_valid,
                    impact_score=impact,
                    evaluator_notes="Auto-evaluated",
                )

                if is_valid:
                    valid_count += 1

                # スキーマに変換
                all_challenges.append(self._challenge_to_schema(challenge))
                all_results.append(self._result_to_schema(result))

            # 調整済み信頼度を取得
            adjusted_conf = self._redteam_service.get_adjusted_confidence(claim)
            adjusted_claim = claim_schema.model_copy()
            adjusted_claim.confidence = adjusted_conf
            adjusted_claims.append(adjusted_claim)

        stats = self._redteam_service.get_redteam_stats()

        self._logger.info(
            f"Red Team completed: {len(all_challenges)} challenges, "
            f"{valid_count} valid"
        )

        return RedTeamOutput(
            challenges=all_challenges,
            results=all_results,
            adjusted_claims=adjusted_claims,
            total_challenges=len(all_challenges),
            valid_challenges=valid_count,
            stats=stats,
        )

    def _schema_to_claim(self, schema: ClaimSchema) -> Claim:
        """ClaimSchema を Claim に変換."""
        return Claim(
            id=schema.id,
            statement=schema.statement,
            evidence_ids=schema.evidence_ids,
            confidence=schema.confidence,
            level=ClaimLevel(schema.level),
            created_at=datetime.fromisoformat(schema.created_at),
            updated_at=datetime.fromisoformat(schema.updated_at),
        )

    def _challenge_to_schema(
        self, challenge: Any
    ) -> ChallengeSchema:
        """Challenge を ChallengeSchema に変換."""
        return ChallengeSchema(
            id=challenge.id,
            claim_id=challenge.claim_id,
            challenge_type=challenge.challenge_type.value,
            argument=challenge.argument,
            counter_evidence_ids=challenge.counter_evidence_ids,
            created_at=challenge.created_at.isoformat(),
        )

    def _result_to_schema(
        self, result: Any
    ) -> ChallengeResultSchema:
        """ChallengeResult を ChallengeResultSchema に変換."""
        return ChallengeResultSchema(
            id=result.id,
            challenge_id=result.challenge_id,
            is_valid=result.is_valid,
            impact_score=result.impact_score,
            confidence_adjustment=result.confidence_adjustment,
            evaluator_notes=result.evaluator_notes,
            evaluated_at=result.evaluated_at.isoformat(),
        )
