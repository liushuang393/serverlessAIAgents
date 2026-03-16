"""LLM backend レジストリ."""

from __future__ import annotations

from typing import TYPE_CHECKING

from contracts.base import ComponentSpec, ComponentToggle, LayerName
from contracts.registry import ToggleableFactoryRegistry
from infrastructure.llm.adapters import AgentFlowLLMBackend, MockLLMBackend, NoOpLLMBackend


if TYPE_CHECKING:
    from infrastructure.llm.ports import LLMBackend


class LLMBackendRegistry:
    """LLM backend の差し替え口."""

    def __init__(self) -> None:
        self._registry: ToggleableFactoryRegistry[LLMBackend] = ToggleableFactoryRegistry(
            component_name="llm_backend"
        )
        self._registry.register(
            ComponentSpec(name="llm_backend", layer=LayerName.INFRASTRUCTURE, implementation="default"),
            lambda: AgentFlowLLMBackend(),
        )
        self._registry.register(
            ComponentSpec(name="llm_backend", layer=LayerName.INFRASTRUCTURE, implementation="noop"),
            lambda: NoOpLLMBackend(),
        )
        self._registry.register(
            ComponentSpec(name="llm_backend", layer=LayerName.INFRASTRUCTURE, implementation="mock"),
            lambda: MockLLMBackend(),
        )

    def resolve(self, toggle: ComponentToggle | None = None) -> LLMBackend:
        """トグル設定から backend を返す."""
        return self._registry.resolve(toggle)

    def implementations(self) -> list[str]:
        """登録済み実装一覧を返す."""
        return self._registry.list_implementations()


_default_registry = LLMBackendRegistry()


def get_llm_backend(toggle: ComponentToggle | None = None) -> LLMBackend:
    """既定 registry から backend を解決する."""
    return _default_registry.resolve(toggle)
