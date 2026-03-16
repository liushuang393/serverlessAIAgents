"""共有 scope resolver."""

from __future__ import annotations

from typing import Any


def resolve_scope(metadata: dict[str, Any] | None = None) -> dict[str, Any]:
    """metadata から最小限の scope を切り出す."""
    payload = metadata or {}
    return {
        "tenant_id": payload.get("tenant_id"),
        "app_name": payload.get("app_name"),
        "product_line": payload.get("product_line"),
    }
