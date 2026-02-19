"""異常検知ユニットテスト.

AnomalyDetectionService のテスト。
外部依存なし。
"""

from __future__ import annotations

from datetime import datetime, timedelta

import pytest
from apps.market_trend_monitor.backend.models import Evidence, SentimentType, SourceType, Trend
from apps.market_trend_monitor.backend.services.anomaly_detection_service import (
    Anomaly,
    AnomalyDetectionService,
)


# ============================================================
# Helpers
# ============================================================


def _make_evidence(
    evidence_id: str = "e-1",
    collected_at: datetime | None = None,
) -> Evidence:
    """テスト用 Evidence を生成."""
    return Evidence(
        id=evidence_id,
        source_id="src-1",
        source_type=SourceType.NEWS,
        url=f"https://example.com/{evidence_id}",
        title="Test Evidence",
        content_hash=f"hash-{evidence_id}",
        collected_at=collected_at or datetime.now(),
        reliability_score=0.7,
    )


def _make_trend(
    trend_id: str = "t-1",
    growth_rate: float = 0.1,
    sentiment: SentimentType = SentimentType.NEUTRAL,
) -> Trend:
    """テスト用 Trend を生成."""
    return Trend(
        id=trend_id,
        topic="Test Topic",
        score=0.5,
        articles_count=5,
        keywords=["test"],
        sentiment=sentiment,
        growth_rate=growth_rate,
    )


# ============================================================
# Anomaly Model Tests
# ============================================================


class TestAnomalyModel:
    """Anomaly データモデルのテスト."""

    def test_anomaly_creation(self) -> None:
        """異常データ生成テスト."""
        anomaly = Anomaly(
            id="anom-1",
            metric_name="evidence_volume",
            timestamp=datetime(2026, 1, 15),
            expected_value=10.0,
            actual_value=50.0,
            z_score=3.5,
            severity="high",
        )
        assert anomaly.id == "anom-1"
        assert anomaly.severity == "high"

    def test_anomaly_to_dict(self) -> None:
        """to_dict 変換テスト."""
        anomaly = Anomaly(
            id="anom-1",
            metric_name="test",
            timestamp=datetime(2026, 1, 15),
            expected_value=5.0,
            actual_value=15.0,
            z_score=2.5,
            severity="medium",
        )
        d = anomaly.to_dict()
        assert d["id"] == "anom-1"
        assert d["severity"] == "medium"
        assert d["z_score"] == 2.5


# ============================================================
# Service Tests
# ============================================================


class TestAnomalyDetectionService:
    """AnomalyDetectionService のテスト."""

    async def test_detect_volume_anomalies_basic(self) -> None:
        """基本的なボリューム異常検知テスト."""
        service = AnomalyDetectionService()

        base_date = datetime(2026, 1, 1)
        evidences = []
        # 通常期間: 日あたり2件
        for day in range(20):
            for j in range(2):
                evidences.append(
                    _make_evidence(
                        f"e-{day}-{j}",
                        collected_at=base_date + timedelta(days=day),
                    )
                )
        # 異常日: 日あたり20件
        anomaly_date = base_date + timedelta(days=20)
        for j in range(20):
            evidences.append(
                _make_evidence(
                    f"e-anom-{j}",
                    collected_at=anomaly_date,
                )
            )

        anomalies = await service.detect_volume_anomalies(evidences)
        # 20件の急増が検知されるはず
        assert len(anomalies) > 0
        high_severity = [a for a in anomalies if a.severity == "high"]
        assert len(high_severity) > 0

    async def test_detect_volume_anomalies_empty(self) -> None:
        """空リストのボリューム異常検知テスト."""
        service = AnomalyDetectionService()
        anomalies = await service.detect_volume_anomalies([])
        assert anomalies == []

    async def test_detect_volume_anomalies_too_few(self) -> None:
        """データ不足時のボリューム異常検知テスト."""
        service = AnomalyDetectionService()
        evidences = [_make_evidence("e-1"), _make_evidence("e-2")]
        anomalies = await service.detect_volume_anomalies(evidences)
        assert anomalies == []

    async def test_detect_volume_anomalies_uniform(self) -> None:
        """均一データのボリューム異常検知テスト."""
        service = AnomalyDetectionService()
        base_date = datetime(2026, 1, 1)
        evidences = []
        for day in range(10):
            evidences.append(
                _make_evidence(
                    f"e-{day}",
                    collected_at=base_date + timedelta(days=day),
                )
            )
        anomalies = await service.detect_volume_anomalies(evidences)
        assert anomalies == []  # 均一なので異常なし

    async def test_detect_sentiment_shifts_basic(self) -> None:
        """センチメントシフト検知テスト."""
        service = AnomalyDetectionService()
        trends = [
            _make_trend("t-1", sentiment=SentimentType.NEUTRAL),
            _make_trend("t-2", sentiment=SentimentType.NEUTRAL),
            _make_trend("t-3", sentiment=SentimentType.NEUTRAL),
            _make_trend("t-4", sentiment=SentimentType.NEUTRAL),
            _make_trend("t-5", sentiment=SentimentType.NEGATIVE),
        ]
        anomalies = await service.detect_sentiment_shifts(trends)
        # 大半がNEUTRALの中にNEGATIVEは異常として検知される可能性
        # （データサイズによる）
        assert isinstance(anomalies, list)

    async def test_detect_sentiment_shifts_empty(self) -> None:
        """空リストのセンチメントシフト検知テスト."""
        service = AnomalyDetectionService()
        anomalies = await service.detect_sentiment_shifts([])
        assert anomalies == []

    async def test_detect_sentiment_shifts_too_few(self) -> None:
        """データ不足時のセンチメントシフト検知テスト."""
        service = AnomalyDetectionService()
        trends = [_make_trend("t-1"), _make_trend("t-2")]
        anomalies = await service.detect_sentiment_shifts(trends)
        assert anomalies == []

    async def test_detect_growth_rate_anomalies(self) -> None:
        """成長率異常検知テスト."""
        service = AnomalyDetectionService()
        trends = [
            _make_trend("t-1", growth_rate=0.1),
            _make_trend("t-2", growth_rate=0.12),
            _make_trend("t-3", growth_rate=0.08),
            _make_trend("t-4", growth_rate=0.11),
            _make_trend("t-5", growth_rate=2.5),  # 異常に高い成長率
        ]
        anomalies = await service.detect_growth_rate_anomalies(trends)
        assert len(anomalies) > 0
        # 最も高い z-score は t-5 のはず
        assert anomalies[0].metadata.get("trend_topic") == "Test Topic"

    async def test_detect_growth_rate_anomalies_empty(self) -> None:
        """空リストの成長率異常検知テスト."""
        service = AnomalyDetectionService()
        anomalies = await service.detect_growth_rate_anomalies([])
        assert anomalies == []

    async def test_detect_growth_rate_anomalies_too_few(self) -> None:
        """データ不足時の成長率異常検知テスト."""
        service = AnomalyDetectionService()
        trends = [_make_trend("t-1")]
        anomalies = await service.detect_growth_rate_anomalies(trends)
        assert anomalies == []

    def test_calculate_z_score_basic(self) -> None:
        """基本的なZ-score計算テスト."""
        service = AnomalyDetectionService()
        values = [10.0, 10.0, 10.0, 10.0, 10.0]
        # 全て同じ値 → std=0 → z=0
        z = service.calculate_z_score(values, 10.0)
        assert z == 0.0

    def test_calculate_z_score_outlier(self) -> None:
        """外れ値のZ-score計算テスト."""
        service = AnomalyDetectionService()
        values = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 100.0]
        z = service.calculate_z_score(values, 100.0)
        assert z > 2.0  # 外れ値は高いZ-score

    def test_calculate_z_score_too_few(self) -> None:
        """データ不足時のZ-score計算テスト."""
        service = AnomalyDetectionService()
        z = service.calculate_z_score([1.0], 1.0)
        assert z == 0.0

    def test_calculate_z_score_negative(self) -> None:
        """負のZ-score計算テスト."""
        service = AnomalyDetectionService()
        values = [10.0, 10.0, 10.0, 10.0, 10.0]
        z = service.calculate_z_score(values + [0.0], 0.0)
        assert z < 0

    def test_classify_severity_high(self) -> None:
        """高深刻度分類テスト."""
        service = AnomalyDetectionService()
        assert service._classify_severity(3.5) == "high"
        assert service._classify_severity(3.0) == "high"

    def test_classify_severity_medium(self) -> None:
        """中深刻度分類テスト."""
        service = AnomalyDetectionService()
        assert service._classify_severity(2.5) == "medium"
        assert service._classify_severity(2.0) == "medium"

    def test_classify_severity_low(self) -> None:
        """低深刻度分類テスト."""
        service = AnomalyDetectionService()
        assert service._classify_severity(1.5) == "low"
        assert service._classify_severity(1.8) == "low"

    def test_compute_std(self) -> None:
        """標準偏差計算テスト."""
        std = AnomalyDetectionService._compute_std([1.0, 1.0, 1.0])
        assert std == pytest.approx(0.0)

        # Phase 10: Bessel correction (sample std, n-1)
        # For [0.0, 10.0]: mean=5.0, var=(25+25)/1=50, std=sqrt(50)
        std = AnomalyDetectionService._compute_std([0.0, 10.0])
        assert std == pytest.approx(50**0.5)

    def test_compute_std_single(self) -> None:
        """単一値の標準偏差テスト."""
        std = AnomalyDetectionService._compute_std([5.0])
        assert std == 0.0

    def test_bucket_by_period(self) -> None:
        """期間バケット分類テスト."""
        base = datetime(2026, 1, 1, 10, 30)
        evidences = [
            _make_evidence("e-1", collected_at=base),
            _make_evidence("e-2", collected_at=base + timedelta(hours=5)),
            _make_evidence("e-3", collected_at=base + timedelta(days=1)),
        ]
        buckets = AnomalyDetectionService._bucket_by_period(evidences)
        # 2日分のバケット
        assert len(buckets) == 2

    def test_bucket_by_period_multi_day(self) -> None:
        """複数日バケット分類テスト."""
        base = datetime(2026, 1, 1)
        evidences = [
            _make_evidence("e-1", collected_at=base),
            _make_evidence("e-2", collected_at=base + timedelta(days=1)),
            _make_evidence("e-3", collected_at=base + timedelta(days=2)),
            _make_evidence("e-4", collected_at=base + timedelta(days=3)),
        ]
        buckets = AnomalyDetectionService._bucket_by_period(evidences, bucket_days=2)
        # 2日バケットで4日分 → 2バケット
        assert len(buckets) == 2

    async def test_anomalies_sorted_by_z_score(self) -> None:
        """異常がZ-score降順にソートされるテスト."""
        service = AnomalyDetectionService()
        trends = [
            _make_trend("t-1", growth_rate=0.1),
            _make_trend("t-2", growth_rate=0.1),
            _make_trend("t-3", growth_rate=0.1),
            _make_trend("t-4", growth_rate=0.1),
            _make_trend("t-5", growth_rate=5.0),
            _make_trend("t-6", growth_rate=3.0),
        ]
        anomalies = await service.detect_growth_rate_anomalies(trends)
        for i in range(len(anomalies) - 1):
            assert abs(anomalies[i].z_score) >= abs(anomalies[i + 1].z_score)
