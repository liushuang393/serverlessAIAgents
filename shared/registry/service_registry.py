"""Layer 2 の共有サービス registry."""

from __future__ import annotations

from typing import Any


class ServiceRegistry:
    """共有サービスの簡易レジストリ."""

    def __init__(self) -> None:
        self._services: dict[str, Any] = {}

    def register(self, name: str, service: Any) -> None:
        """サービスを登録する."""
        self._services[name] = service

    def get(self, name: str) -> Any:
        """サービスを取得する."""
        return self._services[name]

    def get_optional(self, name: str) -> Any | None:
        """存在しない場合は None を返す."""
        return self._services.get(name)
