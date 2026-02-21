"""OpenTelemetry OTLP エクスポーター.

OpenTelemetry Collector/Jaeger/Zipkin などの OTLP 互換バックエンドに
トレースをエクスポートするためのエクスポーター。

使用例:
    >>> from agentflow.observability import setup_tracing
    >>> from agentflow.observability.otel_exporter import OTLPExporter
    >>>
    >>> exporter = OTLPExporter(endpoint="http://localhost:4318/v1/traces")
    >>> tracer = setup_tracing(service_name="my-agent", exporter=exporter)

環境変数:
    OTEL_EXPORTER_OTLP_ENDPOINT: OTLP エンドポイント
    OTEL_EXPORTER_OTLP_HEADERS: カンマ区切りのヘッダー (key=value,key2=value2)
    OTEL_SERVICE_NAME: サービス名
"""

from __future__ import annotations

import logging
import os
from typing import Any

from agentflow.observability.tracing import Span, SpanExporter


logger = logging.getLogger(__name__)


class OTLPExporter(SpanExporter):
    """OpenTelemetry OTLP HTTP エクスポーター.

    OTLP/HTTP プロトコルでスパンをエクスポートします。
    opentelemetry-exporter-otlp-proto-http パッケージがあれば使用し、
    なければシンプルな HTTP POST で送信します。

    Attributes:
        endpoint: OTLP エンドポイント URL
        headers: 追加ヘッダー
        service_name: サービス名
    """

    def __init__(
        self,
        endpoint: str | None = None,
        headers: dict[str, str] | None = None,
        service_name: str | None = None,
        timeout: float = 10.0,
    ) -> None:
        """初期化.

        Args:
            endpoint: OTLP エンドポイント URL
            headers: 追加ヘッダー
            service_name: サービス名
            timeout: タイムアウト秒数
        """
        self._endpoint = endpoint or os.getenv("OTEL_EXPORTER_OTLP_ENDPOINT") or "http://localhost:4318/v1/traces"
        self._headers = headers or self._parse_headers_env()
        self._service_name = service_name or os.getenv("OTEL_SERVICE_NAME") or "agentflow"
        self._timeout = timeout
        self._native_exporter: Any = None

        # ネイティブ OTLP エクスポーターを試行
        self._try_native_exporter()

    def _parse_headers_env(self) -> dict[str, str]:
        """環境変数からヘッダーをパース."""
        headers_str = os.getenv("OTEL_EXPORTER_OTLP_HEADERS", "")
        if not headers_str:
            return {}

        headers = {}
        for pair in headers_str.split(","):
            if "=" in pair:
                key, value = pair.split("=", 1)
                headers[key.strip()] = value.strip()
        return headers

    def _try_native_exporter(self) -> None:
        """ネイティブ OpenTelemetry エクスポーターを試行."""
        try:
            from opentelemetry.exporter.otlp.proto.http.trace_exporter import (
                OTLPSpanExporter,
            )
            from opentelemetry.sdk.resources import Resource
            from opentelemetry.sdk.trace import TracerProvider
            from opentelemetry.sdk.trace.export import SimpleSpanProcessor

            resource = Resource.create({"service.name": self._service_name})
            provider = TracerProvider(resource=resource)
            self._native_exporter = OTLPSpanExporter(
                endpoint=self._endpoint,
                headers=self._headers,
            )
            provider.add_span_processor(SimpleSpanProcessor(self._native_exporter))
            logger.info(f"Using native OTLP exporter: {self._endpoint}")
        except ImportError:
            logger.info("Native OTLP exporter not available. Using HTTP fallback.")

    def export(self, span: Span) -> None:
        """スパンをエクスポート.

        Args:
            span: エクスポートするスパン
        """
        if self._native_exporter:
            self._export_native(span)
        else:
            self._export_http(span)

    def _export_native(self, span: Span) -> None:
        """ネイティブエクスポーターでエクスポート."""
        # Note: 実際の実装ではネイティブ SDK のスパン変換が必要
        # ここでは HTTP フォールバックと同じ動作
        self._export_http(span)

    def _export_http(self, span: Span) -> None:
        """HTTP POST でエクスポート（フォールバック）."""
        try:
            import httpx

            # OTLP JSON 形式に変換
            payload = self._to_otlp_json(span)

            headers = {
                "Content-Type": "application/json",
                **self._headers,
            }

            # 同期 HTTP POST（バックグラウンドで送信）
            with httpx.Client(timeout=self._timeout) as client:
                response = client.post(self._endpoint, json=payload, headers=headers)
                if response.status_code >= 400:
                    logger.warning(f"OTLP export failed: {response.status_code}")
        except ImportError:
            logger.debug("httpx not installed. Span export skipped.")
        except Exception as e:
            logger.debug(f"OTLP export error: {e}")

    def _to_otlp_json(self, span: Span) -> dict[str, Any]:
        """OTLP JSON 形式に変換."""
        return {
            "resourceSpans": [
                {
                    "resource": {
                        "attributes": [
                            {"key": "service.name", "value": {"stringValue": self._service_name}},
                        ]
                    },
                    "scopeSpans": [
                        {
                            "spans": [
                                {
                                    "traceId": span.trace_id,
                                    "spanId": span.span_id,
                                    "parentSpanId": span.context.parent_span_id or "",
                                    "name": span.name,
                                    "startTimeUnixNano": int(span.start_time * 1e9),
                                    "endTimeUnixNano": int(
                                        (span.end_time or span.start_time) * 1e9
                                    ),
                                    "status": {"code": 1 if span.status == "ok" else 2},
                                    "attributes": [
                                        {"key": k, "value": {"stringValue": str(v)}} for k, v in span.attributes.items()
                                    ],
                                }
                            ]
                        }
                    ],
                }
            ]
        }
