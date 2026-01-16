# -*- coding: utf-8 -*-
"""予測復盤エージェント.

過去の予測と実際の結果を比較し、精度を評価します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

from agentflow import ResilientAgent

from apps.market_trend_monitor.backend.models import (
    PredictionOutcome,
    PredictionReviewSchema,
    PredictionSchema,
)
from apps.market_trend_monitor.backend.services.prediction_service import (
    PredictionService,
)


# ============================================================
# Agent I/O スキーマ
# ============================================================


class PredictionReviewInput(BaseModel):
    """PredictionReviewAgent 入力スキーマ."""

    predictions: list[PredictionSchema] = Field(default_factory=list)
    actual_outcomes: dict[str, str] = Field(
        default_factory=dict,
        description="prediction_id -> 実際の結果のマッピング",
    )


class PredictionReviewOutput(BaseModel):
    """PredictionReviewAgent 出力スキーマ."""

    reviews: list[PredictionReviewSchema] = Field(default_factory=list)
    total_reviewed: int = 0
    accuracy_stats: dict = Field(default_factory=dict)


# ============================================================
# Agent 実装
# ============================================================


class PredictionReviewAgent(
    ResilientAgent[PredictionReviewInput, PredictionReviewOutput]
):
    """予測復盤エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 過去の予測と実際の結果を比較
    - 精度スコアの計算
    - 復盤統計の生成

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "PredictionReviewAgent"
    temperature = 0.3  # 評価タスクは低温度

    def __init__(
        self,
        llm_client: Any = None,
        prediction_service: PredictionService | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント（None の場合は自動取得）
            prediction_service: 予測サービス（None の場合は新規作成）
        """
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._prediction_service = prediction_service or PredictionService()

    def _parse_input(
        self, input_data: dict[str, Any]
    ) -> PredictionReviewInput:
        """入力データを Pydantic モデルに変換."""
        return PredictionReviewInput(**input_data)

    async def process(
        self, input_data: PredictionReviewInput
    ) -> PredictionReviewOutput:
        """予測復盤を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き復盤結果
        """
        predictions = input_data.predictions
        actual_outcomes = input_data.actual_outcomes

        self._logger.info(f"Reviewing {len(predictions)} predictions")

        reviews: list[PredictionReviewSchema] = []

        for pred_schema in predictions:
            # 予測を登録（まだ存在しない場合）
            prediction = self._prediction_service.get_prediction(pred_schema.id)
            if not prediction:
                from datetime import date as date_type
                target = date_type.fromisoformat(pred_schema.target_date)
                prediction = self._prediction_service.create_prediction(
                    claim_id=pred_schema.claim_id or "",
                    statement=pred_schema.statement,
                    confidence=pred_schema.confidence,
                    target_date=target,
                    metadata=pred_schema.metadata,
                )

            # 実際の結果がある場合は復盤
            actual = actual_outcomes.get(pred_schema.id)
            if actual:
                # 結果判定（簡易版）
                outcome = self._determine_outcome(
                    predicted=pred_schema.statement,
                    actual=actual,
                )

                review = self._prediction_service.review_prediction(
                    prediction_id=prediction.id,
                    actual_outcome=actual,
                    outcome=outcome,
                    notes="Auto-reviewed",
                )

                if review:
                    reviews.append(
                        PredictionReviewSchema(
                            id=review.id,
                            prediction_id=review.prediction_id,
                            actual_outcome=review.actual_outcome,
                            outcome=review.outcome.value,
                            accuracy_score=review.accuracy_score,
                            reviewed_at=review.reviewed_at.isoformat(),
                            notes=review.notes,
                        )
                    )

        accuracy_stats = self._prediction_service.get_accuracy_stats()

        self._logger.info(f"Reviewed {len(reviews)} predictions")

        return PredictionReviewOutput(
            reviews=reviews,
            total_reviewed=len(reviews),
            accuracy_stats=accuracy_stats,
        )

    def _determine_outcome(
        self, predicted: str, actual: str
    ) -> PredictionOutcome:
        """結果判定.

        簡易版：文字列の類似度で判定。
        将来的にはLLMを使用した高度な判定を実装。
        """
        predicted_lower = predicted.lower().strip()
        actual_lower = actual.lower().strip()

        if predicted_lower == actual_lower:
            return PredictionOutcome.CORRECT

        # 部分一致チェック
        if predicted_lower in actual_lower or actual_lower in predicted_lower:
            return PredictionOutcome.PARTIAL

        # キーワードベースの判定
        predicted_words = set(predicted_lower.split())
        actual_words = set(actual_lower.split())
        overlap = predicted_words & actual_words

        if len(overlap) >= len(predicted_words) * 0.5:
            return PredictionOutcome.PARTIAL

        return PredictionOutcome.INCORRECT

