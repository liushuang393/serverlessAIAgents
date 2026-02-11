"""情報源信頼度動的追跡サービス.

Phase 12: API成功率、コンテンツ品質スコア、鮮度をEWMAで追跡。
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class SourceMetrics:
    """情報源メトリクス."""

    source_type: str
    success_rate: float = 1.0       # API成功率 (EWMA)
    quality_score: float = 0.5      # コンテンツ品質 (EWMA)
    freshness_score: float = 1.0    # 鮮度 (EWMA)
    total_requests: int = 0
    total_successes: int = 0
    last_updated: datetime = field(default_factory=datetime.now)

    @property
    def reliability_score(self) -> float:
        """総合信頼度スコア."""
        return (
            self.success_rate * 0.4
            + self.quality_score * 0.4
            + self.freshness_score * 0.2
        )

    def to_dict(self) -> dict[str, Any]:
        return {
            "source_type": self.source_type,
            "success_rate": self.success_rate,
            "quality_score": self.quality_score,
            "freshness_score": self.freshness_score,
            "reliability_score": self.reliability_score,
            "total_requests": self.total_requests,
            "total_successes": self.total_successes,
            "last_updated": self.last_updated.isoformat(),
        }


class SourceReliabilityTracker:
    """情報源信頼度動的追跡.

    EWMA（指数加重移動平均）で各メトリクスを更新。
    alpha=0.1 で過去の実績を重視しつつ最新結果を反映。
    """

    EWMA_ALPHA: float = 0.1  # 平滑化係数
    DEFAULT_SCORES: dict[str, float] = {
        "news": 0.6,
        "github": 0.8,
        "arxiv": 0.9,
        "rss": 0.5,
        "stackoverflow": 0.7,
        "devto": 0.5,
    }

    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._metrics: dict[str, SourceMetrics] = {}

    def _get_or_create(self, source_type: str) -> SourceMetrics:
        if source_type not in self._metrics:
            default = self.DEFAULT_SCORES.get(source_type, 0.5)
            self._metrics[source_type] = SourceMetrics(
                source_type=source_type,
                success_rate=default,
                quality_score=default,
            )
        return self._metrics[source_type]

    def report_request(
        self,
        source_type: str,
        success: bool,
        article_count: int = 0,
        avg_content_length: float = 0.0,
    ) -> float:
        """収集結果を報告してメトリクスを更新.

        Args:
            source_type: 情報源タイプ
            success: API呼び出し成功フラグ
            article_count: 取得記事数
            avg_content_length: 平均コンテンツ長

        Returns:
            更新後の信頼度スコア
        """
        m = self._get_or_create(source_type)
        alpha = self.EWMA_ALPHA

        m.total_requests += 1
        if success:
            m.total_successes += 1

        # 成功率のEWMA更新
        success_val = 1.0 if success else 0.0
        m.success_rate = alpha * success_val + (1 - alpha) * m.success_rate

        # 品質スコアのEWMA更新（コンテンツ長に基づく）
        if success and article_count > 0:
            # 200文字以上で品質1.0、それ以下は比例
            quality = min(avg_content_length / 200.0, 1.0)
            m.quality_score = alpha * quality + (1 - alpha) * m.quality_score

        # 鮮度（成功時はリフレッシュ）
        if success:
            m.freshness_score = 1.0
        else:
            # 失敗のたびに減衰
            m.freshness_score = max(0.1, m.freshness_score * 0.95)

        m.last_updated = datetime.now()
        return m.reliability_score

    def get_reliability(self, source_type: str) -> float:
        """情報源の現在の信頼度を取得."""
        m = self._metrics.get(source_type)
        if not m:
            return self.DEFAULT_SCORES.get(source_type, 0.5)
        return m.reliability_score

    def get_all_metrics(self) -> dict[str, dict[str, Any]]:
        """全情報源のメトリクスを取得."""
        return {k: v.to_dict() for k, v in self._metrics.items()}
