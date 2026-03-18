"""RuntimeContext 契約 — 7コア層すべてから参照可能な実行時コンテキスト.

contracts 層は依存ゼロであるため、settings の型は Any で保持する。
ContextVar と純粋ヘルパー (get_runtime_context, set_runtime_context, get_env) も
stdlib のみに依存するため contracts 層に配置する。
kernel/runtime/context.py が resolve_settings 等を追加し re-export する。
"""

from __future__ import annotations

import os
from contextvars import ContextVar
from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True)
class RuntimeContext:
    """実行時コンテキスト（マルチテナント対応）.

    Attributes:
        tenant_id: テナント識別子（マルチテナント分離）。
        request_id: リクエスト識別子（トレーシング）。
        trace_id: トレース識別子（分散トレーシング）。
        settings: 設定オーバーライド（per-tenant config）。contracts 層では Any。
        env_overrides: 環境変数オーバーライド。
        metadata: 任意のメタデータ。
    """

    tenant_id: str | None = None
    request_id: str | None = None
    trace_id: str | None = None
    settings: Any = None
    env_overrides: dict[str, str] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# ContextVar — 全層から参照可能
# ---------------------------------------------------------------------------

_current_context: ContextVar[RuntimeContext | None] = ContextVar(
    "bizcore_runtime_context", default=None,
)


def get_runtime_context() -> RuntimeContext | None:
    """Get current runtime context."""
    return _current_context.get()


def set_runtime_context(context: RuntimeContext | None) -> None:
    """Set current runtime context."""
    _current_context.set(context)


# ---------------------------------------------------------------------------
# 環境変数ヘルパー (stdlib のみ依存)
# ---------------------------------------------------------------------------


def get_env(
    key: str,
    default: str | None = None,
    *,
    context: RuntimeContext | None = None,
) -> str | None:
    """Get environment variable with optional runtime override."""
    if context is not None and key in context.env_overrides:
        return context.env_overrides[key]
    return os.getenv(key, default)


__all__ = [
    "RuntimeContext",
    "_current_context",
    "get_env",
    "get_runtime_context",
    "set_runtime_context",
]
