"""kernel MetricsService Protocol のテスト."""
from __future__ import annotations


def test_metrics_service_protocol_exists() -> None:
    """MetricsService Protocol がインポートできること."""
    from kernel.interfaces.metrics_service import MetricsService

    assert hasattr(MetricsService, "increment")
    assert hasattr(MetricsService, "gauge")
    assert hasattr(MetricsService, "histogram")


def test_metrics_service_is_runtime_checkable() -> None:
    """MetricsService が runtime_checkable であること."""
    from kernel.interfaces.metrics_service import MetricsService

    assert isinstance(MetricsService, type)


class _DummyMetrics:
    """テスト用のダミー実装."""

    def increment(
        self, name: str, value: float = 1.0, tags: dict[str, str] | None = None
    ) -> None:
        pass

    def gauge(
        self, name: str, value: float, tags: dict[str, str] | None = None
    ) -> None:
        pass

    def histogram(
        self, name: str, value: float, tags: dict[str, str] | None = None
    ) -> None:
        pass


def test_metrics_service_isinstance_check() -> None:
    """ダミー実装が MetricsService の isinstance チェックを通ること."""
    from kernel.interfaces.metrics_service import MetricsService

    dummy = _DummyMetrics()
    assert isinstance(dummy, MetricsService)


def test_metrics_service_reexported_from_init() -> None:
    """kernel.interfaces から MetricsService を直接インポートできること."""
    from kernel.interfaces import MetricsService

    assert hasattr(MetricsService, "increment")
