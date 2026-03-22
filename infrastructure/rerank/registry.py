"""Layer 1 rerank backend レジストリ."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.rerank.adapters import MockRerankBackend, NoOpRerankBackend


if TYPE_CHECKING:
    from infrastructure.rerank.ports import RerankBackend


class RerankBackendRegistry:
    """rerank backend の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[RerankBackend] = ToggleableFactoryRegistry(
            component_name="rerank_backend"
        )
        self._registry.register(
            ComponentSpec(
                name="rerank_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            MockRerankBackend,
        )
        self._registry.register(
            ComponentSpec(
                name="rerank_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="noop",
            ),
            NoOpRerankBackend,
        )
        self._registry.register(
            ComponentSpec(
                name="rerank_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="mock",
            ),
            MockRerankBackend,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> RerankBackend:
        """トグル設定から backend を返す。"""
        return self._registry.resolve(toggle)

    def implementations(self) -> list[str]:
        """登録済み実装一覧を返す。"""
        return self._registry.list_implementations()


_default_registry = RerankBackendRegistry()


def get_rerank_backend(toggle: ComponentToggle | None = None) -> RerankBackend:
    """既定 registry から rerank backend を返す。"""
    return _default_registry.resolve(toggle)
