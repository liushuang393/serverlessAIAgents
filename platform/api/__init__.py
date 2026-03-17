"""Layer 5 API 公開 facade.

`platform.api.auth_app` のような部分 import 時に、
不要な legacy 初期化を巻き込まないよう遅延解決する。
"""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "DEFAULT_API_HOST": ("platform.api.app", "DEFAULT_API_HOST"),
    "DEFAULT_API_PORT": ("platform.api.app", "DEFAULT_API_PORT"),
    "PlatformAPISurface": ("platform.api.service", "PlatformAPISurface"),
    "RouteDefinition": ("platform.api.service", "RouteDefinition"),
    "app": ("platform.api.app", "app"),
    "create_app": ("platform.api.app", "create_app"),
    "main": ("platform.api.app", "main"),
    # response
    "APIResponse": ("platform.api.response", "APIResponse"),
    "APIError": ("platform.api.response", "APIError"),
    "ErrorCode": ("platform.api.response", "ErrorCode"),
    "PagedResponse": ("platform.api.response", "PagedResponse"),
    "StreamEvent": ("platform.api.response", "StreamEvent"),
    "StreamEventType": ("platform.api.response", "StreamEventType"),
    # rich_builder / sse / websocket
    "RichResponseBuilder": ("platform.api.rich_builder", "RichResponseBuilder"),
    "SSEEmitter": ("platform.api.sse_emitter", "SSEEmitter"),
    "WebSocketHub": ("platform.api.websocket_hub", "WebSocketHub"),
    "WSMessage": ("platform.api.websocket_hub", "WSMessage"),
    "WSMessageType": ("platform.api.websocket_hub", "WSMessageType"),
}


def __getattr__(name: str) -> Any:
    """公開シンボルを必要時にのみ解決する。"""
    target = _EXPORT_MAP.get(name)
    if target is None:
        msg = f"module {__name__!r} has no attribute {name!r}"
        raise AttributeError(msg)
    module_path, symbol_name = target
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
