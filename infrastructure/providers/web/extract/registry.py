"""extract provider registry."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.providers.web.extract.adapters import BrowserJsonExtractor, JsonSchemaExtractor


if TYPE_CHECKING:
    from infrastructure.providers.web.extract.base import StructuredExtractionProvider


class ExtractionProviderRegistry:
    """extract provider の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[StructuredExtractionProvider] = ToggleableFactoryRegistry(
            component_name="web_extract_provider",
        )
        self._registry.register(
            ComponentSpec(
                name="web_extract_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            JsonSchemaExtractor,
        )
        self._registry.register(
            ComponentSpec(
                name="web_extract_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="browser_json",
            ),
            BrowserJsonExtractor,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> StructuredExtractionProvider:
        """トグル設定で provider を解決する."""
        return self._registry.resolve(toggle)


_default_registry = ExtractionProviderRegistry()


def get_extraction_provider(toggle: ComponentToggle | None = None) -> StructuredExtractionProvider:
    """既定 extract provider を返す."""
    return _default_registry.resolve(toggle)
