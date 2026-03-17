"""Runtime context management for AgentFlow.

This module provides a per-request/per-tenant context to avoid global singletons
in multi-tenant environments.

RuntimeContext dataclass は contracts 層で定義し、ここで re-export する。
ヘルパー関数（get/set/use_runtime_context, resolve_settings, get_env）は
kernel 層に留まる。
"""

from __future__ import annotations

from contextlib import contextmanager
from contextvars import ContextVar
from typing import TYPE_CHECKING, Any

# --- contracts 層から RuntimeContext を re-export ---
from contracts.runtime.context import RuntimeContext

if TYPE_CHECKING:
    from collections.abc import Iterator

    from infrastructure.config import AgentFlowSettings


_current_context: ContextVar[RuntimeContext | None] = ContextVar("agentflow_runtime_context", default=None)


def get_runtime_context() -> RuntimeContext | None:
    """Get current runtime context."""
    return _current_context.get()


def set_runtime_context(context: RuntimeContext | None) -> None:
    """Set current runtime context."""
    _current_context.set(context)


@contextmanager
def use_runtime_context(context: RuntimeContext | None) -> Iterator[None]:
    """Temporarily set runtime context within a block."""
    token = _current_context.set(context)
    # Sync observability context if available
    old_obs_context = None
    if context is not None:
        try:
            from infrastructure.observability.logging import get_context, replace_context, set_context

            old_obs_context = get_context()
            payload: dict[str, Any] = {}
            if context.tenant_id:
                payload["tenant_id"] = context.tenant_id
            if context.request_id:
                payload["request_id"] = context.request_id
            if context.trace_id:
                payload["trace_id"] = context.trace_id
            if payload:
                set_context(**payload)
        except Exception:
            old_obs_context = None
    try:
        yield
    finally:
        _current_context.reset(token)
        if old_obs_context is not None:
            try:
                from infrastructure.observability.logging import replace_context

                replace_context(old_obs_context)
            except Exception:
                pass


def resolve_settings(context: RuntimeContext | None = None) -> AgentFlowSettings:
    """Resolve settings using runtime context override if provided."""
    if context is not None and context.settings is not None:
        return context.settings
    from infrastructure.config import get_settings  # noqa: WPS433 — 循環回避

    return get_settings()


def get_env(key: str, default: str | None = None, *, context: RuntimeContext | None = None) -> str | None:
    """Get environment variable with optional runtime override."""
    if context is not None and key in context.env_overrides:
        return context.env_overrides[key]
    import os

    return os.getenv(key, default)


__all__ = [
    "RuntimeContext",
    "get_env",
    "get_runtime_context",
    "resolve_settings",
    "set_runtime_context",
    "use_runtime_context",
]

