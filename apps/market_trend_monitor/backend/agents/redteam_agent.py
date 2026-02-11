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

            # Phase 10: LLMベースのチャレンジ生成・評価
            prompts = self._redteam_service.generate_challenge_prompts(claim)

            for challenge_type in challenge_types:
                # 該当タイプのプロンプトを取得
                prompt_entry = next(
                    (p for p in prompts if p["type"] == challenge_type.value),
                    None,
                )
                if not prompt_entry:
                    continue

                # LLMで反論を生成
                argument = await self._generate_challenge_argument(
                    prompt_entry["prompt"],
                    claim.statement,
                    challenge_type.value,
                )

                challenge = self._redteam_service.create_challenge(
                    claim=claim,
                    challenge_type=challenge_type,
                    argument=argument,
                )

                # LLMで有効性を評価
                is_valid, impact = await self._evaluate_challenge_with_llm(
                    claim.statement, argument, challenge_type.value,
                )

                result = self._redteam_service.evaluate_challenge(
                    challenge=challenge,
                    is_valid=is_valid,
                    impact_score=impact,
                    evaluator_notes="LLM-evaluated",
                )

                if is_valid:
                    valid_count += 1

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

    async def _generate_challenge_argument(
        self,
        prompt: str,
        statement: str,
        challenge_type: str,
    ) -> str:
        """LLMで反論を生成.

        Args:
            prompt: チャレンジプロンプト
            statement: 主張文
            challenge_type: チャレンジタイプ

        Returns:
            生成された反論
        """
        try:
            response = await self._call_llm(prompt)
            if response and response.strip():
                return response.strip()[:500]
        except Exception as e:
            self._logger.warning("LLMチャレンジ生成失敗: %s", e)
        return f"[{challenge_type}] {statement} に対する検証が必要"

    async def _evaluate_challenge_with_llm(
        self,
        statement: str,
        argument: str,
        challenge_type: str,
    ) -> tuple[bool, float]:
        """LLMでチャレンジの有効性を評価.

        Returns:
            (is_valid, impact_score) のタプル
        """
        prompt = (
            f"以下の主張に対する反論の有効性を評価してください。\n\n"
            f"主張: {statement}\n"
            f"反論タイプ: {challenge_type}\n"
            f"反論: {argument}\n\n"
            f"JSON形式で回答: "
            f'{{"is_valid": true/false, "impact": 0.0-1.0, "reason": "..."}}'
        )
        try:
            response = await self._call_llm(prompt)
            if response:
                import json
                start = response.find("{")
                end = response.rfind("}")
                if start != -1 and end > start:
                    data = json.loads(response[start:end + 1])
                    is_valid = bool(data.get("is_valid", False))
                    impact = min(max(float(data.get("impact", 0.3)), 0.0), 1.0)
                    return is_valid, impact
        except Exception as e:
            self._logger.warning("LLMチャレンジ評価失敗: %s", e)
        return False, 0.1

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
