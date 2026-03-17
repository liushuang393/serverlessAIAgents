"""Control-plane 環境バインディング管理."""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True, slots=True)
class EnvironmentBinding:
    """実行環境の定義."""

    name: str
    variables: dict[str, str] = field(default_factory=dict)


class EnvironmentService:
    """環境設定を保持する."""

    def __init__(self) -> None:
        self._bindings: dict[str, EnvironmentBinding] = {}

    def register(self, binding: EnvironmentBinding) -> None:
        """環境設定を登録する。"""
        self._bindings[binding.name] = binding

    def get(self, name: str) -> EnvironmentBinding | None:
        """環境設定を返す。"""
        return self._bindings.get(name)

    def list_bindings(self) -> list[EnvironmentBinding]:
        """登録済み環境設定を返す。"""
        return sorted(self._bindings.values(), key=lambda item: item.name)
