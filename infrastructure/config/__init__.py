"""infrastructure.config — 設定管理モジュール.

AgentFlowフレームワークの設定管理を提供する。
正規パスは infrastructure.config。agentflow.config は後方互換shim。
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from infrastructure.config.settings import AgentFlowSettings, clear_settings_cache, get_settings

if TYPE_CHECKING:
    from contracts.runtime.context import RuntimeContext


def resolve_settings(context: RuntimeContext | None = None) -> AgentFlowSettings:
    """Resolve settings: use runtime context override if available, else global.

    This is the infrastructure-layer equivalent of kernel.runtime.resolve_settings,
    safe for use within infrastructure/ without crossing the layer boundary.
    """
    if context is not None and context.settings is not None:
        return context.settings  # type: ignore[return-value]
    return get_settings()


__all__ = [
    "AgentFlowSettings",
    "clear_settings_cache",
    "get_settings",
    "resolve_settings",
]

