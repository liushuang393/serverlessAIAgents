"""App manifest の読み込みと変換."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from contracts.app import AppManifest


def load_app_manifest(path: str | Path) -> AppManifest:
    """app_config.json を AppManifest 契約へ読み替える."""
    manifest_path = Path(path)
    payload = json.loads(manifest_path.read_text(encoding="utf-8"))
    return AppManifest(
        name=str(payload.get("name", manifest_path.parent.name)),
        display_name=str(payload.get("display_name", payload.get("name", manifest_path.parent.name))),
        product_line=str(payload.get("product_line", "framework")),
        surface_profile=str(payload.get("surface_profile", "developer")),
        audit_profile=str(payload.get("audit_profile", "developer")),
        version=str(payload.get("version", "1.0.0")),
        entry_points=_as_dict(payload.get("entry_points")),
        ports=_coerce_ports(_as_dict(payload.get("ports"))),
        runtime=_as_dict(payload.get("runtime")),
        tags=[str(item) for item in payload.get("tags", []) if isinstance(item, str)],
        metadata={"config_path": str(manifest_path)},
    )


def _as_dict(value: object) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


def _coerce_ports(value: dict[str, Any]) -> dict[str, int | None]:
    result: dict[str, int | None] = {}
    for key, item in value.items():
        if isinstance(item, int):
            result[key] = item
            continue
        if isinstance(item, str) and item.strip().isdigit():
            result[key] = int(item.strip())
            continue
        result[key] = None
    return result
