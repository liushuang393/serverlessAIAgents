"""Control-plane API public facade."""

from __future__ import annotations

import importlib
from typing import Any


_EXPORT_MAP: dict[str, tuple[str, str]] = {
    "DEFAULT_API_HOST": ("control_plane.api.app", "DEFAULT_API_HOST"),
    "DEFAULT_API_PORT": ("control_plane.api.app", "DEFAULT_API_PORT"),
    "PlatformAPISurface": ("control_plane.api.service", "PlatformAPISurface"),
    "RouteDefinition": ("control_plane.api.service", "RouteDefinition"),
    "app": ("control_plane.api.app", "app"),
    "create_app": ("control_plane.api.app", "create_app"),
    "main": ("control_plane.api.app", "main"),
    # response
    "APIResponse": ("control_plane.api.response", "APIResponse"),
    "APIError": ("control_plane.api.response", "APIError"),
    "ErrorCode": ("control_plane.api.response", "ErrorCode"),
    "PagedResponse": ("control_plane.api.response", "PagedResponse"),
    "StreamEvent": ("control_plane.api.response", "StreamEvent"),
    "StreamEventType": ("control_plane.api.response", "StreamEventType"),
    # rich_builder / sse / websocket
    "RichResponseBuilder": ("control_plane.api.rich_builder", "RichResponseBuilder"),
    "SSEEmitter": ("control_plane.api.sse_emitter", "SSEEmitter"),
    "WebSocketHub": ("control_plane.api.websocket_hub", "WebSocketHub"),
    "WSMessage": ("control_plane.api.websocket_hub", "WSMessage"),
    "WSMessageType": ("control_plane.api.websocket_hub", "WSMessageType"),
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
