"""分散トレーシングモジュール.

リクエストの追跡とスパン管理を提供します。

特徴:
- 分散トレーシング（Trace ID、Span ID）
- コンテキスト伝播
- OpenTelemetry 互換
- デコレータサポート
"""

from __future__ import annotations

import functools
import threading
import time
import uuid
from collections.abc import Callable, Iterator
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import Any, TypeVar


# グローバルトレーサー
_tracer_instance: Tracer | None = None
_current_span: threading.local = threading.local()

F = TypeVar("F", bound=Callable[..., Any])


@dataclass
class SpanContext:
    """スパンコンテキスト.

    Attributes:
        trace_id: トレース ID
        span_id: スパン ID
        parent_span_id: 親スパン ID
    """

    trace_id: str
    span_id: str
    parent_span_id: str | None = None

    @classmethod
    def generate(cls, parent: SpanContext | None = None) -> SpanContext:
        """新しいスパンコンテキストを生成.

        Args:
            parent: 親コンテキスト

        Returns:
            SpanContext インスタンス
        """
        if parent:
            return cls(
                trace_id=parent.trace_id,
                span_id=uuid.uuid4().hex[:16],
                parent_span_id=parent.span_id,
            )
        return cls(
            trace_id=uuid.uuid4().hex,
            span_id=uuid.uuid4().hex[:16],
            parent_span_id=None,
        )


@dataclass
class Span:
    """トレーシングスパン.

    Attributes:
        name: スパン名
        context: スパンコンテキスト
        start_time: 開始時間
        end_time: 終了時間
        status: ステータス
        attributes: 属性
        events: イベント
    """

    name: str
    context: SpanContext
    start_time: float = field(default_factory=time.time)
    end_time: float | None = None
    status: str = "ok"
    attributes: dict[str, Any] = field(default_factory=dict)
    events: list[dict[str, Any]] = field(default_factory=list)

    @property
    def duration_ms(self) -> float:
        """実行時間（ミリ秒）."""
        if self.end_time is None:
            return (time.time() - self.start_time) * 1000
        return (self.end_time - self.start_time) * 1000

    @property
    def trace_id(self) -> str:
        """トレース ID."""
        return self.context.trace_id

    @property
    def span_id(self) -> str:
        """スパン ID."""
        return self.context.span_id

    def set_attribute(self, key: str, value: Any) -> None:
        """属性を設定.

        Args:
            key: キー
            value: 値
        """
        self.attributes[key] = value

    def add_event(self, name: str, attributes: dict[str, Any] | None = None) -> None:
        """イベントを追加.

        Args:
            name: イベント名
            attributes: 属性
        """
        self.events.append(
            {
                "name": name,
                "timestamp": datetime.now(UTC).isoformat(),
                "attributes": attributes or {},
            }
        )

    def set_status(self, status: str, message: str | None = None) -> None:
        """ステータスを設定.

        Args:
            status: ステータス（ok / error）
            message: メッセージ
        """
        self.status = status
        if message:
            self.attributes["status_message"] = message

    def end(self) -> None:
        """スパンを終了."""
        self.end_time = time.time()

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換.

        Returns:
            辞書
        """
        return {
            "name": self.name,
            "trace_id": self.trace_id,
            "span_id": self.span_id,
            "parent_span_id": self.context.parent_span_id,
            "start_time": datetime.fromtimestamp(
                self.start_time, tz=UTC
            ).isoformat(),
            "end_time": (
                datetime.fromtimestamp(self.end_time, tz=UTC).isoformat()
                if self.end_time
                else None
            ),
            "duration_ms": self.duration_ms,
            "status": self.status,
            "attributes": self.attributes,
            "events": self.events,
        }


class SpanExporter:
    """スパンエクスポーター基底クラス."""

    def export(self, span: Span) -> None:
        """スパンをエクスポート.

        Args:
            span: スパン
        """
        raise NotImplementedError


class ConsoleExporter(SpanExporter):
    """コンソールエクスポーター."""

    def export(self, span: Span) -> None:
        """スパンをコンソールに出力."""
        import json

        print(json.dumps(span.to_dict(), ensure_ascii=False, default=str))


class InMemoryExporter(SpanExporter):
    """メモリ内エクスポーター（テスト用）."""

    def __init__(self) -> None:
        """初期化."""
        self._spans: list[Span] = []
        self._lock = threading.Lock()

    def export(self, span: Span) -> None:
        """スパンをメモリに保存."""
        with self._lock:
            self._spans.append(span)

    def get_spans(self) -> list[Span]:
        """保存されたスパンを取得.

        Returns:
            スパンリスト
        """
        with self._lock:
            return self._spans.copy()

    def clear(self) -> None:
        """スパンをクリア."""
        with self._lock:
            self._spans.clear()


class Tracer:
    """トレーサー.

    分散トレーシングを管理します。

    Example:
        >>> tracer = Tracer("my-service")
        >>> with tracer.span("process-request") as span:
        ...     span.set_attribute("user_id", "123")
        ...     # 処理
        ...     pass
    """

    def __init__(
        self,
        service_name: str = "agentflow",
        exporter: SpanExporter | None = None,
    ) -> None:
        """初期化.

        Args:
            service_name: サービス名
            exporter: スパンエクスポーター
        """
        self._service_name = service_name
        self._exporter = exporter
        self._spans: list[Span] = []
        self._lock = threading.Lock()

    @contextmanager
    def span(
        self,
        name: str,
        attributes: dict[str, Any] | None = None,
    ) -> Iterator[Span]:
        """スパンを作成.

        Args:
            name: スパン名
            attributes: 属性

        Yields:
            Span インスタンス
        """
        # 親スパンを取得
        parent_span = getattr(_current_span, "span", None)
        parent_context = parent_span.context if parent_span else None

        # 新しいスパンを作成
        span = Span(
            name=name,
            context=SpanContext.generate(parent_context),
            attributes={"service": self._service_name, **(attributes or {})},
        )

        # 現在のスパンを設定
        old_span = getattr(_current_span, "span", None)
        _current_span.span = span

        try:
            yield span
            span.set_status("ok")
        except Exception as e:
            span.set_status("error", str(e))
            span.add_event("exception", {"type": type(e).__name__, "message": str(e)})
            raise
        finally:
            span.end()
            _current_span.span = old_span

            # エクスポート
            if self._exporter:
                self._exporter.export(span)

            # 保存
            with self._lock:
                self._spans.append(span)

    def get_current_span(self) -> Span | None:
        """現在のスパンを取得.

        Returns:
            現在のスパン、または None
        """
        return getattr(_current_span, "span", None)

    def get_current_trace_id(self) -> str | None:
        """現在のトレース ID を取得.

        Returns:
            トレース ID、または None
        """
        span = self.get_current_span()
        return span.trace_id if span else None

    def trace(self, name: str | None = None) -> Callable[[F], F]:
        """トレーシングデコレータ.

        Example:
            >>> @tracer.trace()
            ... async def process(data):
            ...     pass

        Args:
            name: スパン名（省略時は関数名）

        Returns:
            デコレータ
        """

        def decorator(func: F) -> F:
            span_name = name or func.__name__

            if asyncio_is_coroutine_function(func):

                @functools.wraps(func)
                async def async_wrapper(*args: Any, **kwargs: Any) -> Any:
                    with self.span(span_name):
                        return await func(*args, **kwargs)

                return async_wrapper  # type: ignore

            @functools.wraps(func)
            def sync_wrapper(*args: Any, **kwargs: Any) -> Any:
                with self.span(span_name):
                    return func(*args, **kwargs)

            return sync_wrapper  # type: ignore

        return decorator

    def get_spans(self) -> list[Span]:
        """保存されたスパンを取得.

        Returns:
            スパンリスト
        """
        with self._lock:
            return self._spans.copy()

    def clear(self) -> None:
        """スパンをクリア."""
        with self._lock:
            self._spans.clear()


def asyncio_is_coroutine_function(func: Any) -> bool:
    """関数がコルーチンかどうかを判定."""
    import asyncio
    import inspect

    return asyncio.iscoroutinefunction(func) or inspect.iscoroutinefunction(func)


def setup_tracing(
    service_name: str = "agentflow",
    exporter: SpanExporter | None = None,
) -> Tracer:
    """トレーサーをセットアップ.

    Args:
        service_name: サービス名
        exporter: スパンエクスポーター

    Returns:
        Tracer インスタンス
    """
    global _tracer_instance
    _tracer_instance = Tracer(service_name, exporter)
    return _tracer_instance


def get_tracer() -> Tracer:
    """トレーサーを取得.

    Returns:
        Tracer インスタンス
    """
    global _tracer_instance
    if _tracer_instance is None:
        _tracer_instance = Tracer()
    return _tracer_instance

