"""異常検知サービス.

時系列データの急増・急減検知を提供します。
Z-scoreベースの異常検出とトレンドブレイクポイント検出を行います。
"""

from __future__ import annotations

import logging
import math
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from apps.market_trend_monitor.backend.models import Evidence, Trend


@dataclass
class Anomaly:
    """異常データモデル."""

    id: str
    metric_name: str
    timestamp: datetime
    expected_value: float
    actual_value: float
    z_score: float
    severity: str  # "low", "medium", "high"
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "metric_name": self.metric_name,
            "timestamp": self.timestamp.isoformat(),
            "expected_value": self.expected_value,
            "actual_value": self.actual_value,
            "z_score": self.z_score,
            "severity": self.severity,
            "metadata": self.metadata,
        }


class AnomalyDetectionService:
    """異常検知サービス.

    - 時系列データの急増・急減検知（Z-scoreベース）
    - トレンドブレイクポイント検出
    - 異常スコアの計算
    """

    # Z-score 閾値
    LOW_THRESHOLD: float = 1.5
    MEDIUM_THRESHOLD: float = 2.0
    HIGH_THRESHOLD: float = 3.0

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)

    async def detect_volume_anomalies(
        self,
        evidences: list[Evidence],
        window_days: int = 30,
        bucket_days: int = 1,
    ) -> list[Anomaly]:
        """証拠量の異常を検知.

        Args:
            evidences: 証拠リスト
            window_days: 分析ウィンドウ日数
            bucket_days: 集計バケット日数

        Returns:
            検知された異常リスト
        """
        if not evidences:
            return []

        buckets = self._bucket_by_period(evidences, bucket_days)
        # Phase 10: 最小ベースラインを5に引き上げ（小サンプルでの偽陽性防止）
        if len(buckets) < 5:
            return []

        values = [float(count) for count in buckets.values()]
        anomalies: list[Anomaly] = []

        mean_val = sum(values) / len(values)
        std_val = self._compute_std(values)

        if std_val == 0:
            return []

        for date_key, count in buckets.items():
            z = self.calculate_z_score(values, float(count))
            if abs(z) >= self.LOW_THRESHOLD:
                severity = self._classify_severity(abs(z))
                anomalies.append(
                    Anomaly(
                        id=str(uuid.uuid4()),
                        metric_name="evidence_volume",
                        timestamp=datetime.fromisoformat(date_key),
                        expected_value=mean_val,
                        actual_value=float(count),
                        z_score=z,
                        severity=severity,
                        metadata={"window_days": window_days, "bucket_days": bucket_days},
                    )
                )

        return sorted(anomalies, key=lambda a: abs(a.z_score), reverse=True)

    async def detect_sentiment_shifts(
        self,
        trends: list[Trend],
    ) -> list[Anomaly]:
        """センチメント変化の異常を検知.

        Args:
            trends: トレンドリスト

        Returns:
            検知された異常リスト
        """
        if not trends:
            return []

        sentiment_scores = []
        for trend in trends:
            score = {"positive": 1.0, "neutral": 0.0, "negative": -1.0}.get(
                trend.sentiment.value,
                0.0,
            )
            sentiment_scores.append((trend, score))

        if len(sentiment_scores) < 5:
            return []

        values = [s for _, s in sentiment_scores]
        anomalies: list[Anomaly] = []

        for trend, score in sentiment_scores:
            z = self.calculate_z_score(values, score)
            if abs(z) >= self.LOW_THRESHOLD:
                severity = self._classify_severity(abs(z))
                anomalies.append(
                    Anomaly(
                        id=str(uuid.uuid4()),
                        metric_name="sentiment_shift",
                        timestamp=trend.created_at,
                        expected_value=sum(values) / len(values),
                        actual_value=score,
                        z_score=z,
                        severity=severity,
                        metadata={
                            "trend_topic": trend.topic,
                            "sentiment": trend.sentiment.value,
                        },
                    )
                )

        return sorted(anomalies, key=lambda a: abs(a.z_score), reverse=True)

    async def detect_growth_rate_anomalies(
        self,
        trends: list[Trend],
    ) -> list[Anomaly]:
        """成長率の異常を検知.

        Args:
            trends: トレンドリスト

        Returns:
            検知された異常リスト
        """
        if not trends or len(trends) < 5:
            return []

        growth_rates = [t.growth_rate for t in trends]
        anomalies: list[Anomaly] = []

        for trend in trends:
            z = self.calculate_z_score(growth_rates, trend.growth_rate)
            if abs(z) >= self.LOW_THRESHOLD:
                severity = self._classify_severity(abs(z))
                anomalies.append(
                    Anomaly(
                        id=str(uuid.uuid4()),
                        metric_name="growth_rate_anomaly",
                        timestamp=trend.created_at,
                        expected_value=sum(growth_rates) / len(growth_rates),
                        actual_value=trend.growth_rate,
                        z_score=z,
                        severity=severity,
                        metadata={"trend_topic": trend.topic},
                    )
                )

        return sorted(anomalies, key=lambda a: abs(a.z_score), reverse=True)

    def calculate_z_score(self, values: list[float], current: float) -> float:
        """Z-scoreを計算.

        Args:
            values: ベースライン値リスト
            current: 現在値

        Returns:
            Z-score
        """
        if len(values) < 2:
            return 0.0

        mean_val = sum(values) / len(values)
        std_val = self._compute_std(values)

        if std_val == 0:
            return 0.0

        return (current - mean_val) / std_val

    def _classify_severity(self, abs_z: float) -> str:
        """Z-scoreの絶対値から異常の深刻度を分類."""
        if abs_z >= self.HIGH_THRESHOLD:
            return "high"
        if abs_z >= self.MEDIUM_THRESHOLD:
            return "medium"
        return "low"

    @staticmethod
    def _compute_std(values: list[float]) -> float:
        """標本標準偏差を計算（ベッセル補正適用）."""
        if len(values) < 2:
            return 0.0
        mean_val = sum(values) / len(values)
        # Phase 10: ベッセル補正 (n-1) で標本標準偏差を使用
        variance = sum((x - mean_val) ** 2 for x in values) / (len(values) - 1)
        return math.sqrt(variance)

    @staticmethod
    def _bucket_by_period(
        evidences: list[Evidence],
        bucket_days: int = 1,
    ) -> dict[str, int]:
        """証拠を期間バケットに分類.

        Args:
            evidences: 証拠リスト
            bucket_days: バケット日数

        Returns:
            日付文字列 -> 件数のマッピング
        """
        buckets: dict[str, int] = {}
        for evidence in evidences:
            ts = evidence.collected_at
            bucket_date = ts.replace(
                hour=0,
                minute=0,
                second=0,
                microsecond=0,
            )
            if bucket_days > 1:
                days_offset = (bucket_date - datetime(2020, 1, 1)).days
                bucket_offset = (days_offset // bucket_days) * bucket_days
                bucket_date = datetime(2020, 1, 1) + timedelta(days=bucket_offset)

            key = bucket_date.isoformat()
            buckets[key] = buckets.get(key, 0) + 1

        return dict(sorted(buckets.items()))
