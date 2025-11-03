"""
高度な観測可能性システム

このモジュールは、分散トレーシング、リアルタイムメトリクス、パフォーマンス監視機能を提供します。
OpenTelemetry準拠のトレーシング、Prometheus形式のメトリクス、構造化ログを含みます。
"""

import asyncio
import threading
import time
import uuid
from collections import defaultdict, deque
from contextlib import asynccontextmanager, contextmanager
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field

from .logging import get_logger

logger = get_logger(__name__)


class MetricType(str, Enum):
    """メトリクスタイプ"""

    COUNTER = "counter"
    GAUGE = "gauge"
    HISTOGRAM = "histogram"
    SUMMARY = "summary"


class SpanStatus(str, Enum):
    """スパンステータス"""

    OK = "ok"
    ERROR = "error"
    TIMEOUT = "timeout"


@dataclass
class Span:
    """分散トレーシングのスパン"""

    span_id: str
    trace_id: str
    parent_span_id: Optional[str]
    operation_name: str
    start_time: float
    end_time: Optional[float] = None
    duration: Optional[float] = None
    status: SpanStatus = SpanStatus.OK
    tags: Dict[str, Any] = field(default_factory=dict)
    logs: List[Dict[str, Any]] = field(default_factory=list)

    def finish(self, status: SpanStatus = SpanStatus.OK) -> None:
        """スパンを終了する"""
        self.end_time = time.time()
        self.duration = self.end_time - self.start_time
        self.status = status

    def add_tag(self, key: str, value: Any) -> None:
        """タグを追加する"""
        self.tags[key] = value

    def add_log(self, message: str, level: str = "info", **kwargs) -> None:
        """ログを追加する"""
        log_entry = {
            "timestamp": time.time(),
            "level": level,
            "message": message,
            **kwargs,
        }
        self.logs.append(log_entry)


@dataclass
class Trace:
    """分散トレーシングのトレース"""

    trace_id: str
    spans: Dict[str, Span] = field(default_factory=dict)
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None
    duration: Optional[float] = None

    def add_span(self, span: Span) -> None:
        """スパンを追加する"""
        self.spans[span.span_id] = span

    def finish(self) -> None:
        """トレースを終了する"""
        self.end_time = time.time()
        self.duration = self.end_time - self.start_time


class Metric(BaseModel):
    """メトリクス"""

    name: str = Field(..., description="メトリクス名")
    type: MetricType = Field(..., description="メトリクスタイプ")
    value: float = Field(..., description="値")
    labels: Dict[str, str] = Field(default_factory=dict, description="ラベル")
    timestamp: float = Field(default_factory=time.time, description="タイムスタンプ")
    help_text: Optional[str] = Field(None, description="ヘルプテキスト")


class PerformanceMetrics(BaseModel):
    """パフォーマンスメトリクス"""

    cpu_usage: float = Field(..., description="CPU使用率")
    memory_usage: float = Field(..., description="メモリ使用率")
    request_count: int = Field(..., description="リクエスト数")
    error_count: int = Field(..., description="エラー数")
    avg_response_time: float = Field(..., description="平均応答時間")
    active_connections: int = Field(..., description="アクティブ接続数")
    timestamp: float = Field(default_factory=time.time, description="タイムスタンプ")


class TraceContext:
    """トレースコンテキスト（スレッドローカル）"""

    def __init__(self):
        self._local = threading.local()

    @property
    def current_trace(self) -> Optional[Trace]:
        """現在のトレースを取得する"""
        return getattr(self._local, "trace", None)

    @current_trace.setter
    def current_trace(self, trace: Optional[Trace]) -> None:
        """現在のトレースを設定する"""
        self._local.trace = trace

    @property
    def current_span(self) -> Optional[Span]:
        """現在のスパンを取得する"""
        return getattr(self._local, "span", None)

    @current_span.setter
    def current_span(self, span: Optional[Span]) -> None:
        """現在のスパンを設定する"""
        self._local.span = span


class MetricsCollector:
    """メトリクス収集器"""

    def __init__(self):
        self._metrics: Dict[str, Metric] = {}
        self._counters: Dict[str, float] = defaultdict(float)
        self._gauges: Dict[str, float] = {}
        self._histograms: Dict[str, List[float]] = defaultdict(list)
        self._lock = threading.Lock()

    def counter(
        self, name: str, value: float = 1.0, labels: Dict[str, str] = None
    ) -> None:
        """カウンターメトリクスを記録する"""
        with self._lock:
            key = self._make_key(name, labels or {})
            self._counters[key] += value

            metric = Metric(
                name=name,
                type=MetricType.COUNTER,
                value=self._counters[key],
                labels=labels or {},
                help_text=f"Counter metric for {name}",
            )
            self._metrics[key] = metric

    def gauge(self, name: str, value: float, labels: Dict[str, str] = None) -> None:
        """ゲージメトリクスを記録する"""
        with self._lock:
            key = self._make_key(name, labels or {})
            self._gauges[key] = value

            metric = Metric(
                name=name,
                type=MetricType.GAUGE,
                value=value,
                labels=labels or {},
                help_text=f"Gauge metric for {name}",
            )
            self._metrics[key] = metric

    def histogram(self, name: str, value: float, labels: Dict[str, str] = None) -> None:
        """ヒストグラムメトリクスを記録する"""
        with self._lock:
            key = self._make_key(name, labels or {})
            self._histograms[key].append(value)

            # 基本統計を計算
            values = self._histograms[key]
            avg_value = sum(values) / len(values)

            metric = Metric(
                name=name,
                type=MetricType.HISTOGRAM,
                value=avg_value,
                labels=labels or {},
                help_text=f"Histogram metric for {name}",
            )
            self._metrics[key] = metric

    def _make_key(self, name: str, labels: Dict[str, str]) -> str:
        """メトリクスキーを生成する"""
        if not labels:
            return name

        label_str = ",".join(f"{k}={v}" for k, v in sorted(labels.items()))
        return f"{name}{{{label_str}}}"

    def get_metrics(self) -> List[Metric]:
        """全メトリクスを取得する"""
        with self._lock:
            return list(self._metrics.values())

    def get_prometheus_format(self) -> str:
        """Prometheus形式でメトリクスを出力する"""
        lines = []

        with self._lock:
            for metric in self._metrics.values():
                # ヘルプテキスト
                if metric.help_text:
                    lines.append(f"# HELP {metric.name} {metric.help_text}")

                # タイプ
                lines.append(f"# TYPE {metric.name} {metric.type}")

                # 値
                if metric.labels:
                    label_str = ",".join(f'{k}="{v}"' for k, v in metric.labels.items())
                    lines.append(f"{metric.name}{{{label_str}}} {metric.value}")
                else:
                    lines.append(f"{metric.name} {metric.value}")

        return "\n".join(lines)


class Tracer:
    """分散トレーサー"""

    def __init__(self, service_name: str = "ai-blocks"):
        self.service_name = service_name
        self._traces: Dict[str, Trace] = {}
        self._context = TraceContext()
        self._lock = threading.Lock()

    def start_trace(self, operation_name: str) -> Trace:
        """新しいトレースを開始する"""
        trace_id = str(uuid.uuid4())
        trace = Trace(trace_id=trace_id)

        with self._lock:
            self._traces[trace_id] = trace

        self._context.current_trace = trace

        # ルートスパンを作成
        root_span = self.start_span(operation_name)
        root_span.add_tag("service.name", self.service_name)

        logger.debug(f"トレースを開始しました: {trace_id}")
        return trace

    def start_span(
        self,
        operation_name: str,
        parent_span: Optional[Span] = None,
        tags: Dict[str, Any] = None,
    ) -> Span:
        """新しいスパンを開始する"""
        current_trace = self._context.current_trace
        if not current_trace:
            # トレースが存在しない場合は新しく作成
            current_trace = self.start_trace(operation_name)

        span_id = str(uuid.uuid4())
        parent_span_id = None

        if parent_span:
            parent_span_id = parent_span.span_id
        elif self._context.current_span:
            parent_span_id = self._context.current_span.span_id

        span = Span(
            span_id=span_id,
            trace_id=current_trace.trace_id,
            parent_span_id=parent_span_id,
            operation_name=operation_name,
            start_time=time.time(),
        )

        if tags:
            for key, value in tags.items():
                span.add_tag(key, value)

        current_trace.add_span(span)
        self._context.current_span = span

        logger.debug(f"スパンを開始しました: {operation_name} ({span_id})")
        return span

    def finish_span(self, span: Span, status: SpanStatus = SpanStatus.OK) -> None:
        """スパンを終了する"""
        span.finish(status)
        logger.debug(f"スパンを終了しました: {span.operation_name} ({span.duration:.3f}秒)")

    def finish_trace(self, trace: Trace) -> None:
        """トレースを終了する"""
        trace.finish()
        logger.debug(f"トレースを終了しました: {trace.trace_id} ({trace.duration:.3f}秒)")

    def get_trace(self, trace_id: str) -> Optional[Trace]:
        """トレースを取得する"""
        with self._lock:
            return self._traces.get(trace_id)

    def get_all_traces(self) -> List[Trace]:
        """全トレースを取得する"""
        with self._lock:
            return list(self._traces.values())


class PerformanceMonitor:
    """パフォーマンス監視"""

    def __init__(self, collection_interval: float = 60.0):
        self.collection_interval = collection_interval
        self._metrics_history: deque[PerformanceMetrics] = deque(
            maxlen=1000
        )  # 最大1000件の履歴
        self._monitoring_task: Optional[asyncio.Task] = None
        self._running = False

    async def start_monitoring(self) -> None:
        """監視を開始する"""
        if self._running:
            return

        self._running = True
        self._monitoring_task = asyncio.create_task(self._monitoring_loop())
        logger.info("パフォーマンス監視を開始しました")

    async def stop_monitoring(self) -> None:
        """監視を停止する"""
        self._running = False

        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass

        logger.info("パフォーマンス監視を停止しました")

    async def _monitoring_loop(self) -> None:
        """監視ループ"""
        while self._running:
            try:
                metrics = await self._collect_metrics()
                self._metrics_history.append(metrics)

                # 異常検知
                await self._detect_anomalies(metrics)

                await asyncio.sleep(self.collection_interval)

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"パフォーマンス監視中にエラーが発生しました: {e}")
                await asyncio.sleep(5)  # エラー時は短い間隔で再試行

    async def _collect_metrics(self) -> PerformanceMetrics:
        """メトリクスを収集する"""
        try:
            import psutil

            # システムメトリクス
            cpu_usage = psutil.cpu_percent()
            memory = psutil.virtual_memory()
            memory_usage = memory.percent
        except ImportError:
            # psutilが利用できない場合のフォールバック
            cpu_usage = 0.0
            memory_usage = 0.0

        # アプリケーションメトリクス（模擬）
        request_count = 0  # 実際の実装では適切なカウンターから取得
        error_count = 0
        avg_response_time = 0.0
        active_connections = 0

        return PerformanceMetrics(
            cpu_usage=cpu_usage,
            memory_usage=memory_usage,
            request_count=request_count,
            error_count=error_count,
            avg_response_time=avg_response_time,
            active_connections=active_connections,
        )

    async def _detect_anomalies(self, current_metrics: PerformanceMetrics) -> None:
        """異常検知"""
        # 簡単な閾値ベースの異常検知
        if current_metrics.cpu_usage > 80:
            logger.warning(f"CPU使用率が高いです: {current_metrics.cpu_usage:.1f}%")

        if current_metrics.memory_usage > 85:
            logger.warning(f"メモリ使用率が高いです: {current_metrics.memory_usage:.1f}%")

        if current_metrics.avg_response_time > 5.0:
            logger.warning(f"応答時間が遅いです: {current_metrics.avg_response_time:.2f}秒")

    def get_metrics_history(
        self, limit: Optional[int] = None
    ) -> List[PerformanceMetrics]:
        """メトリクス履歴を取得する"""
        if limit:
            return list(self._metrics_history)[-limit:]
        return list(self._metrics_history)

    def get_current_metrics(self) -> Optional[PerformanceMetrics]:
        """現在のメトリクスを取得する"""
        if self._metrics_history:
            return self._metrics_history[-1]
        return None


class ObservabilityManager:
    """観測可能性管理クラス"""

    def __init__(self, service_name: str = "ai-blocks"):
        self.service_name = service_name
        self.tracer = Tracer(service_name)
        self.metrics_collector = MetricsCollector()
        self.performance_monitor = PerformanceMonitor()
        self._initialized = False

    async def initialize(self) -> None:
        """観測可能性システムを初期化する"""
        if self._initialized:
            return

        await self.performance_monitor.start_monitoring()
        self._initialized = True

        logger.info("観測可能性システムを初期化しました")

    async def cleanup(self) -> None:
        """クリーンアップ処理"""
        await self.performance_monitor.stop_monitoring()
        logger.info("観測可能性システムをクリーンアップしました")

    @contextmanager
    def trace(self, operation_name: str, tags: Dict[str, Any] = None):
        """トレーシングコンテキストマネージャー"""
        span = self.tracer.start_span(operation_name, tags=tags)

        try:
            yield span
        except Exception as e:
            span.add_tag("error", True)
            span.add_tag("error.message", str(e))
            span.add_log(f"エラーが発生しました: {e}", level="error")
            self.tracer.finish_span(span, SpanStatus.ERROR)
            raise
        else:
            self.tracer.finish_span(span, SpanStatus.OK)

    @asynccontextmanager
    async def async_trace(self, operation_name: str, tags: Dict[str, Any] = None):
        """非同期トレーシングコンテキストマネージャー"""
        span = self.tracer.start_span(operation_name, tags=tags)

        try:
            yield span
        except Exception as e:
            span.add_tag("error", True)
            span.add_tag("error.message", str(e))
            span.add_log(f"エラーが発生しました: {e}", level="error")
            self.tracer.finish_span(span, SpanStatus.ERROR)
            raise
        else:
            self.tracer.finish_span(span, SpanStatus.OK)

    def record_metric(
        self,
        name: str,
        value: float,
        metric_type: MetricType,
        labels: Dict[str, str] = None,
    ) -> None:
        """メトリクスを記録する"""
        if metric_type == MetricType.COUNTER:
            self.metrics_collector.counter(name, value, labels)
        elif metric_type == MetricType.GAUGE:
            self.metrics_collector.gauge(name, value, labels)
        elif metric_type == MetricType.HISTOGRAM:
            self.metrics_collector.histogram(name, value, labels)

    def get_observability_summary(self) -> Dict[str, Any]:
        """観測可能性の概要を取得する"""
        traces = self.tracer.get_all_traces()
        metrics = self.metrics_collector.get_metrics()
        current_performance = self.performance_monitor.get_current_metrics()

        return {
            "service_name": self.service_name,
            "traces": {
                "total_count": len(traces),
                "active_count": len([t for t in traces if t.end_time is None]),
            },
            "metrics": {
                "total_count": len(metrics),
                "types": {
                    metric_type.value: len(
                        [m for m in metrics if m.type == metric_type]
                    )
                    for metric_type in MetricType
                },
            },
            "performance": current_performance.dict() if current_performance else None,
            "timestamp": time.time(),
        }


# グローバル観測可能性管理インスタンス
_observability_manager: Optional[ObservabilityManager] = None


async def get_observability_manager() -> ObservabilityManager:
    """観測可能性管理インスタンスを取得する（シングルトン）"""
    global _observability_manager
    if _observability_manager is None:
        _observability_manager = ObservabilityManager()
        await _observability_manager.initialize()
    return _observability_manager


# 便利関数とデコレータ
def trace(operation_name: str, tags: Dict[str, Any] = None):
    """トレーシングデコレータ"""

    def decorator(func):
        if asyncio.iscoroutinefunction(func):

            async def async_wrapper(*args, **kwargs):
                manager = await get_observability_manager()
                async with manager.async_trace(operation_name, tags):
                    return await func(*args, **kwargs)

            return async_wrapper
        else:

            def sync_wrapper(*args, **kwargs):
                import asyncio

                manager = asyncio.run(get_observability_manager())
                with manager.trace(operation_name, tags):
                    return func(*args, **kwargs)

            return sync_wrapper

    return decorator


async def record_counter(
    name: str, value: float = 1.0, labels: Dict[str, str] = None
) -> None:
    """カウンターメトリクスを記録する便利関数"""
    manager = await get_observability_manager()
    manager.record_metric(name, value, MetricType.COUNTER, labels)


async def record_gauge(name: str, value: float, labels: Dict[str, str] = None) -> None:
    """ゲージメトリクスを記録する便利関数"""
    manager = await get_observability_manager()
    manager.record_metric(name, value, MetricType.GAUGE, labels)


async def record_histogram(
    name: str, value: float, labels: Dict[str, str] = None
) -> None:
    """ヒストグラムメトリクスを記録する便利関数"""
    manager = await get_observability_manager()
    manager.record_metric(name, value, MetricType.HISTOGRAM, labels)
