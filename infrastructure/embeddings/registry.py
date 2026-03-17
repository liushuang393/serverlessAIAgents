"""Layer 1 embeddings backend レジストリ."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry

from infrastructure.embeddings.adapters import (
    LegacyEmbeddingBackend,
    MockEmbeddingBackend,
    NoOpEmbeddingBackend,
)


if TYPE_CHECKING:
    from infrastructure.embeddings.ports import EmbeddingBackend


class EmbeddingBackendRegistry:
    """embeddings backend の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[EmbeddingBackend] = ToggleableFactoryRegistry(
            component_name="embedding_backend"
        )
        self._registry.register(
            ComponentSpec(
                name="embedding_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="default",
            ),
            LegacyEmbeddingBackend,
        )
        self._registry.register(
            ComponentSpec(
                name="embedding_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="noop",
            ),
            NoOpEmbeddingBackend,
        )
        self._registry.register(
            ComponentSpec(
                name="embedding_backend",
                layer=LayerName.INFRASTRUCTURE,
                implementation="mock",
            ),
            MockEmbeddingBackend,
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> EmbeddingBackend:
        """トグル設定から backend を解決する。"""
        return self._registry.resolve(toggle)

    def implementations(self) -> list[str]:
        """登録済み実装一覧を返す。"""
        return self._registry.list_implementations()


_default_registry = EmbeddingBackendRegistry()


def get_embedding_backend(toggle: ComponentToggle | None = None) -> EmbeddingBackend:
    """既定 registry から embeddings backend を返す。"""
    return _default_registry.resolve(toggle)
