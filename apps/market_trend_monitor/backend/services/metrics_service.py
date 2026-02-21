"""メトリクスサービス.

Phase 13: パイプライン実行時間、エラー数、証拠登録数などのメトリクスを収集。
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass
from datetime import datetime
from typing import Any


@dataclass
class PipelineMetrics:
    """パイプライン実行メトリクス."""

    total_runs: int = 0
    successful_runs: int = 0
    failed_runs: int = 0
    total_duration_seconds: float = 0.0
    last_run_at: datetime | None = None
    last_error: str | None = None
    articles_collected: int = 0
    evidences_registered: int = 0
    signals_evaluated: int = 0
    predictions_created: int = 0

    @property
    def avg_duration(self) -> float:
        if self.total_runs == 0:
            return 0.0
        return self.total_duration_seconds / self.total_runs

    @property
    def success_rate(self) -> float:
        if self.total_runs == 0:
            return 0.0
        return self.successful_runs / self.total_runs

    def to_dict(self) -> dict[str, Any]:
        return {
            "total_runs": self.total_runs,
            "successful_runs": self.successful_runs,
            "failed_runs": self.failed_runs,
            "avg_duration_seconds": round(self.avg_duration, 2),
            "total_duration_seconds": round(self.total_duration_seconds, 2),
            "success_rate": round(self.success_rate, 4),
            "last_run_at": self.last_run_at.isoformat() if self.last_run_at else None,
            "last_error": self.last_error,
            "articles_collected": self.articles_collected,
            "evidences_registered": self.evidences_registered,
            "signals_evaluated": self.signals_evaluated,
            "predictions_created": self.predictions_created,
        }


class MetricsService:
    """メトリクス収集サービス."""

    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._metrics = PipelineMetrics()
        self._start_time = time.monotonic()
        self._run_start: float | None = None

    def start_run(self) -> None:
        """パイプライン実行開始を記録."""
        self._run_start = time.monotonic()

    def end_run(self, success: bool, error: str | None = None) -> None:
        """パイプライン実行終了を記録."""
        self._metrics.total_runs += 1
        self._metrics.last_run_at = datetime.now()

        if success:
            self._metrics.successful_runs += 1
        else:
            self._metrics.failed_runs += 1
            self._metrics.last_error = error

        if self._run_start is not None:
            duration = time.monotonic() - self._run_start
            self._metrics.total_duration_seconds += duration
            self._run_start = None

    def record_articles(self, count: int) -> None:
        self._metrics.articles_collected += count

    def record_evidences(self, count: int) -> None:
        self._metrics.evidences_registered += count

    def record_signals(self, count: int) -> None:
        self._metrics.signals_evaluated += count

    def record_predictions(self, count: int) -> None:
        self._metrics.predictions_created += count

    def get_metrics(self) -> dict[str, Any]:
        return self._metrics.to_dict()

    def get_health(self) -> dict[str, Any]:
        uptime = time.monotonic() - self._start_time
        return {
            "status": "healthy",
            "uptime_seconds": round(uptime, 2),
            "last_run": self._metrics.last_run_at.isoformat() if self._metrics.last_run_at else None,
            "total_runs": self._metrics.total_runs,
        }

    def is_ready(self) -> bool:
        """サービスがリクエスト受付可能か."""
        return True


# シングルトン
metrics_service = MetricsService()
