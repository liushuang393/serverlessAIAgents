"""app_config 契約駆動の HTTP/WS 認証ガード."""

from __future__ import annotations

import json
import os
from collections.abc import Awaitable, Callable
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar

from fastapi import HTTPException, Request, WebSocket
from fastapi.responses import JSONResponse


if TYPE_CHECKING:
    from pathlib import Path


_T = TypeVar("_T")

HTTPAuthBackend = Callable[[Request], Awaitable[_T | None] | _T | None]
WSAuthBackend = Callable[[WebSocket], Awaitable[_T | None] | _T | None]


@dataclass(slots=True)
class ContractAuthGuardConfig:
    """契約駆動認証ガード設定."""

    app_config_path: Path
    public_http_paths: set[str] = field(default_factory=set)
    http_path_prefixes: tuple[str, ...] = ("/api/",)
    auth_header_name: str = "x-api-key"
    ws_query_key: str = "api_key"
    api_key_env_selector_var: str | None = None
    default_api_key_env_var: str = "APP_API_KEY"


class ContractAuthGuard:
    """contracts.auth を基準に HTTP/WS 認証を統一する."""

    def __init__(
        self,
        config: ContractAuthGuardConfig,
        *,
        http_backend: HTTPAuthBackend[Any] | None = None,
        ws_backend: WSAuthBackend[Any] | None = None,
    ) -> None:
        self._config = config
        self._http_backend = http_backend
        self._ws_backend = ws_backend
        self._cached_app_config: dict[str, Any] | None = None

    def load_app_config(self) -> dict[str, Any]:
        """app_config.json を読み込む."""
        if self._cached_app_config is not None:
            return self._cached_app_config
        path = self._config.app_config_path
        if not path.is_file():
            self._cached_app_config = {}
            return self._cached_app_config
        try:
            self._cached_app_config = json.loads(path.read_text("utf-8"))
        except json.JSONDecodeError:
            self._cached_app_config = {}
        return self._cached_app_config

    def auth_contract(self) -> dict[str, Any]:
        raw = self.load_app_config()
        contracts = raw.get("contracts", {})
        if not isinstance(contracts, dict):
            return {}
        auth = contracts.get("auth", {})
        if not isinstance(auth, dict):
            return {}
        return auth

    def is_auth_required(self) -> bool:
        auth = self.auth_contract()
        enabled = bool(auth.get("enabled", False))
        allow_anonymous = bool(auth.get("allow_anonymous", True))
        return enabled and not allow_anonymous

    def should_protect_http_path(self, path: str) -> bool:
        if path in self._config.public_http_paths:
            return False
        return any(path.startswith(prefix) for prefix in self._config.http_path_prefixes)

    def _api_key_env_name(self) -> str:
        selector = self._config.api_key_env_selector_var
        if selector:
            selected = os.getenv(selector, "").strip()
            if selected:
                return selected
        return self._config.default_api_key_env_var

    def verify_api_key(self, incoming_key: str | None) -> None:
        if not self.is_auth_required():
            return

        env_name = self._api_key_env_name()
        expected_key = os.getenv(env_name)
        if not expected_key:
            raise HTTPException(
                status_code=503,
                detail=f"Auth required but env '{env_name}' is not configured",
            )
        if incoming_key != expected_key:
            raise HTTPException(status_code=401, detail="Invalid API key")

    async def require_http(self, request: Request) -> Any | None:
        """HTTP リクエストの認証を強制する."""
        if not self.should_protect_http_path(request.url.path):
            return None
        if not self.is_auth_required():
            return None

        if self._http_backend is not None:
            user = await _maybe_await(self._http_backend(request))
            if user is None:
                raise HTTPException(status_code=401, detail="Unauthorized")
            return user

        self.verify_api_key(request.headers.get(self._config.auth_header_name))
        return None

    async def require_ws(self, websocket: WebSocket) -> tuple[bool, Any | None]:
        """WS ハンドシェイク認証を強制する."""
        if not self.is_auth_required():
            return True, None

        try:
            if self._ws_backend is not None:
                user = await _maybe_await(self._ws_backend(websocket))
                if user is None:
                    raise HTTPException(status_code=401, detail="Unauthorized")
                return True, user

            incoming_key = websocket.headers.get(
                self._config.auth_header_name,
            ) or websocket.query_params.get(self._config.ws_query_key)
            self.verify_api_key(incoming_key)
            return True, None
        except HTTPException as exc:
            close_code = 4401 if exc.status_code == 401 else 1011
            await websocket.close(code=close_code, reason=str(exc.detail))
            return False, None

    async def http_middleware(self, request: Request, call_next: Any) -> Any:
        """FastAPI middleware 連携."""
        try:
            await self.require_http(request)
        except HTTPException as exc:
            return JSONResponse({"detail": exc.detail}, status_code=exc.status_code)
        return await call_next(request)

    def reset_cache(self) -> None:
        """app_config キャッシュを破棄."""
        self._cached_app_config = None


async def _maybe_await(value: _T | Awaitable[_T]) -> _T:
    if hasattr(value, "__await__"):
        return await value
    return value


__all__ = [
    "ContractAuthGuard",
    "ContractAuthGuardConfig",
    "HTTPAuthBackend",
    "WSAuthBackend",
]
