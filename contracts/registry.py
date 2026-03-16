"""差し替え可能コンポーネント向けの共通レジストリ（L0 契約層）。

``ToggleableFactoryRegistry`` は複数層（infrastructure, shared 等）で使用されるため、
Layer 0 (contracts) に配置し、上位層が境界違反なくインポートできるようにする。
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Generic, TypeVar

from contracts.base import ComponentSpec, ComponentToggle

if TYPE_CHECKING:
    from collections.abc import Callable


T = TypeVar("T")


@dataclass(frozen=True, slots=True)
class RegisteredComponent(Generic[T]):
    """登録済みコンポーネント。"""

    spec: ComponentSpec
    factory: Callable[[], T]


class ToggleableFactoryRegistry(Generic[T]):
    """Feature Toggle を考慮して実装を解決する共通レジストリ。"""

    def __init__(self, *, component_name: str) -> None:
        self._component_name = component_name
        self._components: dict[str, RegisteredComponent[T]] = {}

    def register(self, spec: ComponentSpec, factory: Callable[[], T]) -> None:
        """実装を登録する。"""
        self._components[spec.implementation] = RegisteredComponent(spec=spec, factory=factory)

    def resolve(self, toggle: ComponentToggle | None = None) -> T:
        """トグル設定から実装を解決する。"""
        effective_toggle = toggle or ComponentToggle()
        if not effective_toggle.enabled:
            return self._resolve_fallback(effective_toggle)

        implementation = effective_toggle.implementation
        component = self._components.get(implementation)
        if component is not None:
            return component.factory()
        return self._resolve_fallback(effective_toggle)

    def list_implementations(self) -> list[str]:
        """登録済み実装名を返す。"""
        return sorted(self._components.keys())

    def _resolve_fallback(self, toggle: ComponentToggle) -> T:
        fallback_name = toggle.fallback_implementation
        if fallback_name is not None and fallback_name in self._components:
            return self._components[fallback_name].factory()

        if "default" in self._components:
            return self._components["default"].factory()

        msg = f"{self._component_name} の実装を解決できません: {toggle.implementation}"
        raise LookupError(msg)

