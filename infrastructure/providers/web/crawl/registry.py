"""crawl provider registry."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.providers.web.crawl.adapters import RecursiveFetchProvider, SiteCrawlProvider


if TYPE_CHECKING:
    from infrastructure.providers.web.crawl.base import CrawlProvider


class CrawlProviderRegistry:
    """crawl provider の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[CrawlProvider] = ToggleableFactoryRegistry(
            component_name="web_crawl_provider",
        )
        self._registry.register(
            ComponentSpec(
                name="web_crawl_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            SiteCrawlProvider,
        )
        self._registry.register(
            ComponentSpec(
                name="web_crawl_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="recursive",
            ),
            RecursiveFetchProvider,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> CrawlProvider:
        """トグル設定で provider を解決する."""
        return self._registry.resolve(toggle)


_default_registry = CrawlProviderRegistry()


def get_crawl_provider(toggle: ComponentToggle | None = None) -> CrawlProvider:
    """既定 crawl provider を返す."""
    return _default_registry.resolve(toggle)
