"""App manifest の読み込みと正規化."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from contracts.app import AppManifest


def load_app_manifest(path: str | Path) -> AppManifest:
    """app_config.json を canonical AppManifest 契約へ正規化する."""
    manifest_path = _resolve_manifest_path(path)
    return load_app_manifest_text(
        manifest_path.read_text(encoding="utf-8"),
        manifest_path=manifest_path,
    )


def load_app_manifest_text(text: str, *, manifest_path: str | Path) -> AppManifest:
    """デコード済み JSON text を canonical AppManifest 契約へ正規化する."""
    resolved_path = _resolve_manifest_path(manifest_path)
    payload = json.loads(text)
    return load_app_manifest_payload(payload, manifest_path=resolved_path)


def load_app_manifest_payload(payload: object, *, manifest_path: str | Path) -> AppManifest:
    """JSON payload を canonical AppManifest 契約へ正規化する."""
    resolved_path = _resolve_manifest_path(manifest_path)
    normalized = _normalize_payload(payload, resolved_path)
    return AppManifest.model_validate(normalized)


def load_app_manifest_dict(path: str | Path) -> dict[str, Any]:
    """app_config.json を canonical dict へ正規化する."""
    manifest_path = _resolve_manifest_path(path)
    return load_app_manifest_dict_text(
        manifest_path.read_text(encoding="utf-8"),
        manifest_path=manifest_path,
    )


def load_app_manifest_dict_text(text: str, *, manifest_path: str | Path) -> dict[str, Any]:
    """デコード済み JSON text を canonical dict へ正規化する."""
    resolved_path = _resolve_manifest_path(manifest_path)
    payload = json.loads(text)
    return load_app_manifest_dict_payload(payload, manifest_path=resolved_path)


def load_app_manifest_dict_payload(payload: object, *, manifest_path: str | Path) -> dict[str, Any]:
    """JSON payload を canonical dict へ正規化する."""
    manifest = load_app_manifest_payload(payload, manifest_path=manifest_path)
    normalized = _manifest_to_dict(manifest)
    normalized["contracts"] = _normalize_contracts(payload)
    return normalized


def _manifest_to_dict(manifest: AppManifest) -> dict[str, Any]:
    payload = manifest.model_dump(mode="python")
    return payload if isinstance(payload, dict) else {}


def _resolve_manifest_path(path: str | Path) -> Path:
    return Path(path).resolve()


def _normalize_payload(payload: object, manifest_path: Path) -> dict[str, Any]:
    raw = payload if isinstance(payload, dict) else {}
    metadata = raw.get("metadata")
    normalized_metadata = metadata if isinstance(metadata, dict) else {}
    return {
        "name": str(raw.get("name", manifest_path.parent.name)),
        "display_name": str(raw.get("display_name", raw.get("name", manifest_path.parent.name))),
        "description": str(raw.get("description", "")),
        "business_base": raw.get("business_base"),
        "product_line": str(raw.get("product_line", "framework")),
        "surface_profile": str(raw.get("surface_profile", "developer")),
        "audit_profile": str(raw.get("audit_profile", "developer")),
        "version": str(raw.get("version", "1.0.0")),
        "icon": str(raw.get("icon", "📦")),
        "ports": _as_dict(raw.get("ports")),
        "entry_points": _as_dict(raw.get("entry_points")),
        "agents": raw.get("agents", []),
        "services": _as_dict(raw.get("services")),
        "dependencies": _as_dict(raw.get("dependencies")),
        "runtime": _as_dict(raw.get("runtime")),
        "contracts": _normalize_contracts(payload),
        "evolution": _as_dict(raw.get("evolution")),
        "plugin_bindings": raw.get("plugin_bindings", []),
        "security_mode": raw.get("security_mode"),
        "blueprint": _as_dict(raw.get("blueprint")),
        "visibility": _as_dict(raw.get("visibility")),
        "tags": [str(item) for item in raw.get("tags", []) if isinstance(item, str)],
        "metadata": {
            **normalized_metadata,
            "config_path": str(manifest_path),
        },
    }


def _as_dict(value: object) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


def _normalize_contracts(payload: object) -> dict[str, Any]:
    raw = payload if isinstance(payload, dict) else {}
    contracts = raw.get("contracts")
    if not isinstance(contracts, dict):
        return {}

    normalized: dict[str, Any] = {}
    for section_name in ("auth", "llm", "rag", "skills", "release"):
        section = contracts.get(section_name)
        if isinstance(section, dict):
            normalized[section_name] = section
    return normalized
