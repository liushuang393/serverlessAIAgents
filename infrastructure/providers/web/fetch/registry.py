"""fetch provider registry."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.providers.web.fetch.adapters import (
    DirectMarkdownFetcher,
    FallbackChainFetcher,
    HtmlReadabilityFetcher,
    RenderedMarkdownFetcher,
)


if TYPE_CHECKING:
    from infrastructure.providers.web.fetch.base import ContentFetchProvider


class FetchProviderRegistry:
    """fetch provider の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[ContentFetchProvider] = ToggleableFactoryRegistry(
            component_name="web_fetch_provider",
        )
        self._registry.register(
            ComponentSpec(
                name="web_fetch_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            lambda: FallbackChainFetcher(
                providers=[
                    DirectMarkdownFetcher(),
                    HtmlReadabilityFetcher(),
                    RenderedMarkdownFetcher(),
                ]
            ),
        )
        self._registry.register(
            ComponentSpec(
                name="web_fetch_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="direct",
            ),
            DirectMarkdownFetcher,
        )
        self._registry.register(
            ComponentSpec(
                name="web_fetch_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="html",
            ),
            HtmlReadabilityFetcher,
        )
        self._registry.register(
            ComponentSpec(
                name="web_fetch_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="rendered",
            ),
            RenderedMarkdownFetcher,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> ContentFetchProvider:
        """トグル設定で provider を解決する."""
        return self._registry.resolve(toggle)


_default_registry = FetchProviderRegistry()


def get_fetch_provider(toggle: ComponentToggle | None = None) -> ContentFetchProvider:
    """既定 fetch provider を返す."""
    return _default_registry.resolve(toggle)
