"""認証・認可サービスの抽象."""

from __future__ import annotations

from typing import Any, Protocol, runtime_checkable


@runtime_checkable
class AuthServiceProtocol(Protocol):
    """認証・認可の抽象インターフェース."""

    async def authenticate(self, token: str) -> dict[str, Any]:
        """トークンを検証してクレーム情報を返す."""
        ...

    async def authorize(self, user_id: str, action: str, resource: str) -> bool:
        """アクションの認可判定."""
        ...
