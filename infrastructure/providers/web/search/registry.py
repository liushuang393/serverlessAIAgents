"""search provider registry."""

from __future__ import annotations

import os
from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.providers.web.search.adapters import ExternalSearchAPIProvider, InternalSearchProvider


if TYPE_CHECKING:
    from infrastructure.providers.web.search.base import SearchProvider


def _create_default_search_provider() -> SearchProvider:
    """環境変数に応じて既定の search provider を返す."""
    search_env_keys = ("BING_SEARCH_API_KEY", "SERPAPI_API_KEY", "TAVILY_API_KEY")
    if any(os.getenv(key, "").strip() for key in search_env_keys):
        return ExternalSearchAPIProvider()
    return InternalSearchProvider()


class SearchProviderRegistry:
    """search provider の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[SearchProvider] = ToggleableFactoryRegistry(
            component_name="web_search_provider",
        )
        self._registry.register(
            ComponentSpec(
                name="web_search_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            _create_default_search_provider,
        )
        self._registry.register(
            ComponentSpec(
                name="web_search_provider",
                layer=LayerName.INFRASTRUCTURE,
                implementation="external",
            ),
            ExternalSearchAPIProvider,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> SearchProvider:
        """トグル設定で provider を解決する."""
        return self._registry.resolve(toggle)


_default_registry = SearchProviderRegistry()


def get_search_provider(toggle: ComponentToggle | None = None) -> SearchProvider:
    """既定 search provider を返す."""
    return _default_registry.resolve(toggle)
