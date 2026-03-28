"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings

from kernel.skills.builtin.conversation_export.conversation_export import (
    ConversationExportSkill,
    ExportConfig,
    ExportFormat,
    ExportMessage,
)


warnings.warn(
    "shared.skills.builtin.conversation_export は非推奨です。kernel.skills.builtin.conversation_export を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = [
    "ConversationExportSkill",
    "ExportConfig",
    "ExportFormat",
    "ExportMessage",
]
