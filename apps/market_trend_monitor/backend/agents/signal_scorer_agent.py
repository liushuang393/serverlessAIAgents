"""信号評価エージェント.

5軸評価体系に基づく統一的な信号強度評価を提供します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    SentimentType,
    SignalSchema,
    SignalScoreSchema,
    Trend,
    TrendSchema,
)
from apps.market_trend_monitor.backend.services.signal_service import (
    SignalService,
)
from pydantic import BaseModel, Field

from agentflow import ResilientAgent


# ============================================================
# Agent I/O スキーマ
# ============================================================


class SignalScorerInput(BaseModel):
    """SignalScorerAgent 入力スキーマ."""

    trends: list[TrendSchema] = Field(default_factory=list)
    evidence_counts: dict[str, int] = Field(
        default_factory=dict,
        description="trend_id -> 証拠数のマッピング",
    )
    source_types: dict[str, list[str]] = Field(
        default_factory=dict,
        description="trend_id -> ソースタイプリストのマッピング",
    )


class SignalScorerOutput(BaseModel):
    """SignalScorerAgent 出力スキーマ."""

    signals: list[SignalSchema] = Field(default_factory=list)
    total_evaluated: int = 0
    strong_signals_count: int = 0
    dashboard_stats: dict = Field(default_factory=dict)


# ============================================================
# Agent 実装
# ============================================================


class SignalScorerAgent(ResilientAgent[SignalScorerInput, SignalScorerOutput]):
    """信号評価エージェント（ResilientAgent 継承・型安全）.

    役割:
    - トレンドに対する5軸信号評価
    - 信号グレード判定（A/B/C/D）
    - ダッシュボード統計の生成

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "SignalScorerAgent"
    temperature = 0.2  # 評価タスクは低温度

    def __init__(
        self,
        llm_client: Any = None,
        signal_service: SignalService | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント（None の場合は自動取得）
            signal_service: 信号サービス（None の場合は新規作成）
        """
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._signal_service = signal_service or SignalService()

    def _parse_input(self, input_data: dict[str, Any]) -> SignalScorerInput:
        """入力データを Pydantic モデルに変換."""
        return SignalScorerInput(**input_data)

    async def process(
        self, input_data: SignalScorerInput
    ) -> SignalScorerOutput:
        """信号評価を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き評価結果
        """
        trends = input_data.trends
        evidence_counts = input_data.evidence_counts
        source_types = input_data.source_types

        self._logger.info(f"Evaluating {len(trends)} trends")

        signals: list[SignalSchema] = []
        strong_count = 0

        for trend_schema in trends:
            # TrendSchema から Trend に変換
            trend = self._schema_to_trend(trend_schema)

            # 証拠数とソースタイプを取得
            ev_count = evidence_counts.get(trend.id, 0)
            src_types = source_types.get(trend.id, [])

            # 信号評価を実行
            signal = self._signal_service.evaluate_trend(
                trend=trend,
                evidence_count=ev_count,
                source_types=src_types,
            )

            # SignalSchema に変換
            signal_schema = SignalSchema(
                id=signal.id,
                trend_id=signal.trend_id,
                score=SignalScoreSchema(
                    reliability=signal.score.reliability,
                    leading=signal.score.leading,
                    relevance=signal.score.relevance,
                    actionability=signal.score.actionability,
                    convergence=signal.score.convergence,
                    total=signal.score.total,
                    grade=signal.grade.value,
                ),
                grade=signal.grade.value,
                evaluated_at=signal.evaluated_at.isoformat(),
                metadata=signal.metadata,
            )
            signals.append(signal_schema)

            if signal.grade.value in ["A", "B"]:
                strong_count += 1

        dashboard_stats = self._signal_service.get_dashboard_stats()

        self._logger.info(
            f"Evaluated {len(signals)} signals, "
            f"{strong_count} strong signals (A/B)"
        )

        return SignalScorerOutput(
            signals=signals,
            total_evaluated=len(signals),
            strong_signals_count=strong_count,
            dashboard_stats=dashboard_stats,
        )

    def _schema_to_trend(self, schema: TrendSchema) -> Trend:
        """TrendSchema を Trend に変換."""
        first_seen = schema.first_seen or datetime.now().isoformat()
        last_seen = schema.last_seen or datetime.now().isoformat()
        return Trend(
            id=schema.id,
            topic=schema.topic,
            score=schema.score,
            articles_count=schema.articles_count,
            keywords=schema.keywords,
            sentiment=SentimentType(schema.sentiment),
            growth_rate=schema.growth_rate,
            first_seen=datetime.fromisoformat(first_seen),
            last_seen=datetime.fromisoformat(last_seen),
            article_count=schema.article_count,
            metadata=schema.metadata,
        )
