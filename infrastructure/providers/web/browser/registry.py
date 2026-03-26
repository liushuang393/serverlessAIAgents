"""browser provider registry."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.providers.web.browser.adapters import LegacyPlaywrightBrowserOperator, MCPBrowserOperator


if TYPE_CHECKING:
    from infrastructure.providers.web.browser.base import BrowserOperator


class BrowserProviderRegistry:
    """browser operator の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[BrowserOperator] = ToggleableFactoryRegistry(
            component_name="web_browser_operator",
        )
        self._registry.register(
            ComponentSpec(
                name="web_browser_operator",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            MCPBrowserOperator,
        )
        self._registry.register(
            ComponentSpec(
                name="web_browser_operator",
                layer=LayerName.INFRASTRUCTURE,
                implementation="mcp",
            ),
            MCPBrowserOperator,
        )
        self._registry.register(
            ComponentSpec(
                name="web_browser_operator",
                layer=LayerName.INFRASTRUCTURE,
                implementation="legacy_playwright",
            ),
            LegacyPlaywrightBrowserOperator,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> BrowserOperator:
        """トグル設定で operator を解決する."""
        return self._registry.resolve(toggle)


_default_registry = BrowserProviderRegistry()


def get_browser_operator(toggle: ComponentToggle | None = None) -> BrowserOperator:
    """既定 browser operator を返す."""
    return _default_registry.resolve(toggle)
