"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_chatbot = import_module("kernel.skills.builtin.chatbot.chatbot")
ChatBotConfig = _chatbot.ChatBotConfig
ChatBotSkill = _chatbot.ChatBotSkill
ChatSession = _chatbot.ChatSession


warnings.warn(
    "shared.skills.builtin.chatbot は非推奨です。kernel.skills.builtin.chatbot を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["ChatBotConfig", "ChatBotSkill", "ChatSession"]
