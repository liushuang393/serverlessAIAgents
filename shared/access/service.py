"""共有 access resolver."""

from __future__ import annotations

from typing import Any

from shared.access.contracts import AccessContext


def build_access_context(
    *,
    user_id: str,
    permissions: list[str] | None = None,
    tenant_id: str | None = None,
    resource: dict[str, Any] | None = None,
    action: str = "",
) -> AccessContext:
    """共通 access context を組み立てる."""
    return AccessContext(
        subject={
            "user_id": user_id,
            "permissions": permissions or [],
        },
        resource=resource or {},
        action=action,
        tenant_id=tenant_id,
    )
