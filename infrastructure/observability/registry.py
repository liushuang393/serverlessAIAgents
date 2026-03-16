"""観測 exporter レジストリ."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.observability.exporters import InMemoryTraceExporter, LoggingTraceExporter, NoOpTraceExporter


if TYPE_CHECKING:
    from infrastructure.observability.ports import TraceExporter


class TraceExporterRegistry:
    """Trace exporter の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[TraceExporter] = ToggleableFactoryRegistry(
            component_name="trace_exporter"
        )
        self._registry.register(
            ComponentSpec(name="trace_exporter", layer=LayerName.INFRASTRUCTURE, implementation="default"),
            LoggingTraceExporter,
        )
        self._registry.register(
            ComponentSpec(name="trace_exporter", layer=LayerName.INFRASTRUCTURE, implementation="noop"),
            NoOpTraceExporter,
        )
        self._registry.register(
            ComponentSpec(name="trace_exporter", layer=LayerName.INFRASTRUCTURE, implementation="memory"),
            InMemoryTraceExporter,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> TraceExporter:
        """トグル設定から exporter を返す."""
        return self._registry.resolve(toggle)


_default_registry = TraceExporterRegistry()


def get_trace_exporter(toggle: ComponentToggle | None = None) -> TraceExporter:
    """既定 registry から exporter を返す."""
    return _default_registry.resolve(toggle)
