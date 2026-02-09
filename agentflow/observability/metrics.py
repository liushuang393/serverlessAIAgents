"""メトリクス収集モジュール.

アプリケーションメトリクスの収集と公開を提供します。

特徴:
- Prometheus 互換メトリクス
- カウンター、ゲージ、ヒストグラム
- ラベルサポート
- HTTP エンドポイント公開
"""

from __future__ import annotations

import threading
import time
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Iterator


# グローバルメトリクスコレクター
_metrics_instance: MetricsCollector | None = None


@dataclass
class MetricValue:
    """メトリクス値.

    Attributes:
        value: 現在の値
        labels: ラベル
        timestamp: 更新タイムスタンプ
    """

    value: float
    labels: dict[str, str] = field(default_factory=dict)
    timestamp: float = field(default_factory=time.time)


class Counter:
    """カウンターメトリクス.

    単調増加する値（リクエスト数、エラー数など）。

    Example:
        >>> counter = Counter("requests_total", "Total requests")
        >>> counter.inc()
        >>> counter.inc(5)
        >>> counter.inc(labels={"method": "POST"})
    """

    def __init__(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
    ) -> None:
        """初期化.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト
        """
        self._name = name
        self._description = description
        self._label_names = label_names or []
        self._values: dict[tuple[str, ...], float] = defaultdict(float)
        self._lock = threading.Lock()

    def inc(
        self,
        value: float = 1,
        labels: dict[str, str] | None = None,
    ) -> None:
        """カウンターを増加.

        Args:
            value: 増加値（正の数のみ）
            labels: ラベル
        """
        if value < 0:
            msg = "Counter can only increase"
            raise ValueError(msg)

        label_key = self._get_label_key(labels)
        with self._lock:
            self._values[label_key] += value

    def get(self, labels: dict[str, str] | None = None) -> float:
        """現在の値を取得.

        Args:
            labels: ラベル

        Returns:
            現在の値
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            return self._values[label_key]

    def _get_label_key(self, labels: dict[str, str] | None) -> tuple[str, ...]:
        """ラベルをキーに変換."""
        if not labels:
            return ()
        return tuple(labels.get(name, "") for name in self._label_names)

    @property
    def name(self) -> str:
        """メトリクス名."""
        return self._name

    @property
    def description(self) -> str:
        """説明."""
        return self._description


class Gauge:
    """ゲージメトリクス.

    増減する値（メモリ使用量、アクティブ接続数など）。

    Example:
        >>> gauge = Gauge("active_connections", "Active connections")
        >>> gauge.set(10)
        >>> gauge.inc()
        >>> gauge.dec()
    """

    def __init__(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
    ) -> None:
        """初期化.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト
        """
        self._name = name
        self._description = description
        self._label_names = label_names or []
        self._values: dict[tuple[str, ...], float] = defaultdict(float)
        self._lock = threading.Lock()

    def set(
        self,
        value: float,
        labels: dict[str, str] | None = None,
    ) -> None:
        """値を設定.

        Args:
            value: 設定値
            labels: ラベル
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            self._values[label_key] = value

    def inc(
        self,
        value: float = 1,
        labels: dict[str, str] | None = None,
    ) -> None:
        """値を増加.

        Args:
            value: 増加値
            labels: ラベル
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            self._values[label_key] += value

    def dec(
        self,
        value: float = 1,
        labels: dict[str, str] | None = None,
    ) -> None:
        """値を減少.

        Args:
            value: 減少値
            labels: ラベル
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            self._values[label_key] -= value

    def get(self, labels: dict[str, str] | None = None) -> float:
        """現在の値を取得.

        Args:
            labels: ラベル

        Returns:
            現在の値
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            return self._values[label_key]

    def _get_label_key(self, labels: dict[str, str] | None) -> tuple[str, ...]:
        """ラベルをキーに変換."""
        if not labels:
            return ()
        return tuple(labels.get(name, "") for name in self._label_names)

    @property
    def name(self) -> str:
        """メトリクス名."""
        return self._name

    @property
    def description(self) -> str:
        """説明."""
        return self._description


class Histogram:
    """ヒストグラムメトリクス.

    値の分布を計測（レイテンシ、リクエストサイズなど）。

    Example:
        >>> histogram = Histogram("request_duration", "Request duration")
        >>> histogram.observe(0.5)
        >>> with histogram.time():
        ...     # 処理
        ...     pass
    """

    DEFAULT_BUCKETS = (0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10)

    def __init__(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
        buckets: tuple[float, ...] | None = None,
    ) -> None:
        """初期化.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト
            buckets: バケット境界値
        """
        self._name = name
        self._description = description
        self._label_names = label_names or []
        self._buckets = buckets or self.DEFAULT_BUCKETS
        self._observations: dict[tuple[str, ...], list[float]] = defaultdict(list)
        self._lock = threading.Lock()

    def observe(
        self,
        value: float,
        labels: dict[str, str] | None = None,
    ) -> None:
        """値を観測.

        Args:
            value: 観測値
            labels: ラベル
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            self._observations[label_key].append(value)

    @contextmanager
    def time(
        self,
        labels: dict[str, str] | None = None,
    ) -> Iterator[None]:
        """実行時間を計測.

        Example:
            >>> with histogram.time():
            ...     # 処理
            ...     pass
        """
        start = time.time()
        try:
            yield
        finally:
            duration = time.time() - start
            self.observe(duration, labels)

    def get_stats(
        self,
        labels: dict[str, str] | None = None,
    ) -> dict[str, Any]:
        """統計情報を取得.

        Args:
            labels: ラベル

        Returns:
            統計情報
        """
        label_key = self._get_label_key(labels)
        with self._lock:
            observations = self._observations[label_key].copy()

        if not observations:
            return {
                "count": 0,
                "sum": 0,
                "avg": 0,
                "min": 0,
                "max": 0,
                "buckets": {},
            }

        # バケット計算
        bucket_counts: dict[float, int] = {}
        for bucket in self._buckets:
            bucket_counts[bucket] = sum(1 for v in observations if v <= bucket)

        return {
            "count": len(observations),
            "sum": sum(observations),
            "avg": sum(observations) / len(observations),
            "min": min(observations),
            "max": max(observations),
            "buckets": bucket_counts,
        }

    def _get_label_key(self, labels: dict[str, str] | None) -> tuple[str, ...]:
        """ラベルをキーに変換."""
        if not labels:
            return ()
        return tuple(labels.get(name, "") for name in self._label_names)

    @property
    def name(self) -> str:
        """メトリクス名."""
        return self._name

    @property
    def description(self) -> str:
        """説明."""
        return self._description


class MetricsCollector:
    """メトリクスコレクター.

    メトリクスの登録と収集を管理します。

    Example:
        >>> collector = MetricsCollector()
        >>> requests = collector.counter("requests_total")
        >>> duration = collector.histogram("request_duration")
        >>> requests.inc()
    """

    def __init__(self, prefix: str = "agentflow") -> None:
        """初期化.

        Args:
            prefix: メトリクス名プレフィックス
        """
        self._prefix = prefix
        self._counters: dict[str, Counter] = {}
        self._gauges: dict[str, Gauge] = {}
        self._histograms: dict[str, Histogram] = {}
        self._lock = threading.Lock()

    def counter(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
    ) -> Counter:
        """カウンターを取得または作成.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト

        Returns:
            Counter インスタンス
        """
        full_name = f"{self._prefix}_{name}"
        with self._lock:
            if full_name not in self._counters:
                self._counters[full_name] = Counter(full_name, description, label_names)
            return self._counters[full_name]

    def gauge(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
    ) -> Gauge:
        """ゲージを取得または作成.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト

        Returns:
            Gauge インスタンス
        """
        full_name = f"{self._prefix}_{name}"
        with self._lock:
            if full_name not in self._gauges:
                self._gauges[full_name] = Gauge(full_name, description, label_names)
            return self._gauges[full_name]

    def histogram(
        self,
        name: str,
        description: str = "",
        label_names: list[str] | None = None,
        buckets: tuple[float, ...] | None = None,
    ) -> Histogram:
        """ヒストグラムを取得または作成.

        Args:
            name: メトリクス名
            description: 説明
            label_names: ラベル名リスト
            buckets: バケット境界値

        Returns:
            Histogram インスタンス
        """
        full_name = f"{self._prefix}_{name}"
        with self._lock:
            if full_name not in self._histograms:
                self._histograms[full_name] = Histogram(
                    full_name, description, label_names, buckets
                )
            return self._histograms[full_name]

    def collect(self) -> dict[str, Any]:
        """全メトリクスを収集.

        Returns:
            メトリクス情報
        """
        with self._lock:
            counters = {
                name: {
                    "type": "counter",
                    "description": c.description,
                    "value": c.get(),
                }
                for name, c in self._counters.items()
            }
            gauges = {
                name: {
                    "type": "gauge",
                    "description": g.description,
                    "value": g.get(),
                }
                for name, g in self._gauges.items()
            }
            histograms = {
                name: {
                    "type": "histogram",
                    "description": h.description,
                    "stats": h.get_stats(),
                }
                for name, h in self._histograms.items()
            }

        return {
            "counters": counters,
            "gauges": gauges,
            "histograms": histograms,
        }

    def to_prometheus(self) -> str:
        """Prometheus 形式で出力.

        Returns:
            Prometheus 形式の文字列
        """
        lines = []

        with self._lock:
            # Counters
            for name, counter in self._counters.items():
                if counter.description:
                    lines.append(f"# HELP {name} {counter.description}")
                lines.append(f"# TYPE {name} counter")
                lines.append(f"{name} {counter.get()}")

            # Gauges
            for name, gauge in self._gauges.items():
                if gauge.description:
                    lines.append(f"# HELP {name} {gauge.description}")
                lines.append(f"# TYPE {name} gauge")
                lines.append(f"{name} {gauge.get()}")

            # Histograms
            for name, histogram in self._histograms.items():
                if histogram.description:
                    lines.append(f"# HELP {name} {histogram.description}")
                lines.append(f"# TYPE {name} histogram")
                stats = histogram.get_stats()
                for bucket, count in stats.get("buckets", {}).items():
                    lines.append(f'{name}_bucket{{le="{bucket}"}} {count}')
                lines.append(f'{name}_bucket{{le="+Inf"}} {stats["count"]}')
                lines.append(f'{name}_sum {stats["sum"]}')
                lines.append(f'{name}_count {stats["count"]}')

        return "\n".join(lines)


def setup_metrics(prefix: str = "agentflow") -> MetricsCollector:
    """メトリクスコレクターをセットアップ.

    Args:
        prefix: メトリクス名プレフィックス

    Returns:
        MetricsCollector インスタンス
    """
    global _metrics_instance
    _metrics_instance = MetricsCollector(prefix)
    return _metrics_instance


def get_metrics() -> MetricsCollector:
    """メトリクスコレクターを取得.

    Returns:
        MetricsCollector インスタンス
    """
    global _metrics_instance
    if _metrics_instance is None:
        _metrics_instance = MetricsCollector()
    return _metrics_instance

