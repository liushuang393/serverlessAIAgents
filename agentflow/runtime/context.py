"""Runtime context management for AgentFlow.

This module provides a per-request/per-tenant context to avoid global singletons
in multi-tenant environments.
"""

from __future__ import annotations

from contextlib import contextmanager
from contextvars import ContextVar
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from agentflow.config import AgentFlowSettings, get_settings


if TYPE_CHECKING:
    from collections.abc import Iterator


@dataclass(frozen=True)
class RuntimeContext:
    """Runtime context for multi-tenant execution.

    Attributes:
        tenant_id: Tenant identifier (multi-tenant isolation).
        request_id: Request identifier (tracing).
        trace_id: Trace identifier (distributed tracing).
        settings: Optional settings override (per-tenant config).
        env_overrides: Optional env overrides for providers that still rely on env.
        metadata: Arbitrary metadata to propagate.
    """

    tenant_id: str | None = None
    request_id: str | None = None
    trace_id: str | None = None
    settings: AgentFlowSettings | None = None
    env_overrides: dict[str, str] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)


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
            from agentflow.observability.logging import get_context, replace_context, set_context

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
                from agentflow.observability.logging import replace_context

                replace_context(old_obs_context)
            except Exception:
                pass


def resolve_settings(context: RuntimeContext | None = None) -> AgentFlowSettings:
    """Resolve settings using runtime context override if provided."""
    if context is not None and context.settings is not None:
        return context.settings
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
