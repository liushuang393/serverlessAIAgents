"""kernel/interfaces/metrics_service.py — メトリクス収集の抽象."""

from __future__ import annotations

from typing import Protocol, runtime_checkable


@runtime_checkable
class MetricsService(Protocol):
    """メトリクス収集の抽象インターフェース.

    Kernel はこの Protocol を通じてメトリクスを記録し、
    Prometheus / Datadog 等の具体実装を知らない。
    """

    def increment(self, name: str, value: float = 1.0, tags: dict[str, str] | None = None) -> None:
        """カウンターをインクリメント.

        Args:
            name: メトリクス名
            value: 増分値（デフォルト 1.0）
            tags: メトリクスタグ
        """
        ...

    def gauge(self, name: str, value: float, tags: dict[str, str] | None = None) -> None:
        """ゲージ値を設定.

        Args:
            name: メトリクス名
            value: ゲージ値
            tags: メトリクスタグ
        """
        ...

    def histogram(self, name: str, value: float, tags: dict[str, str] | None = None) -> None:
        """ヒストグラム値を記録.

        Args:
            name: メトリクス名
            value: 記録する値
            tags: メトリクスタグ
        """
        ...
