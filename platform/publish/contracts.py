"""Layer 5 publish 契約の互換窓口."""

from __future__ import annotations

from platform._legacy import resolve_export
from typing import Any


_EXPORT_MAP = {
    "PublishEvent": ("apps.platform.schemas.publish_schemas", "PublishEvent"),
    "PublishRequest": ("apps.platform.schemas.publish_schemas", "PublishRequest"),
    "PublishResponse": ("apps.platform.schemas.publish_schemas", "PublishResponse"),
    "PublishTarget": ("apps.platform.schemas.publish_schemas", "PublishTarget"),
}


def __getattr__(name: str) -> Any:
    """publish 契約を遅延解決する。"""
    return resolve_export(_EXPORT_MAP, name, __name__)


__all__ = tuple(sorted(_EXPORT_MAP.keys()))
