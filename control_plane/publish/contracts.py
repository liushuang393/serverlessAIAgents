"""Control-plane publish 契約の互換窓口."""

from __future__ import annotations

from typing import Any

from control_plane._legacy import resolve_export


_EXPORT_MAP = {
    "PublishEvent": ("control_plane.schemas.publish_schemas", "PublishEvent"),
    "PublishRequest": ("control_plane.schemas.publish_schemas", "PublishRequest"),
    "PublishResponse": ("control_plane.schemas.publish_schemas", "PublishResponse"),
    "PublishTarget": ("control_plane.schemas.publish_schemas", "PublishTarget"),
}


def __getattr__(name: str) -> Any:
    """publish 契約を遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
