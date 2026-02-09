"""ChatBot Skill - 対話管理と Agent 連携能力.

このモジュールは、マルチターン対話と Agent 呼び出しを統合した
ChatBot 機能を提供します。SKILL.md と Python 実装が同じディレクトリに配置されています。

Example:
    >>> from agentflow.skills.builtin.chatbot import ChatBotSkill, ChatBotConfig
    >>> bot = ChatBotSkill(config=ChatBotConfig())
    >>> await bot.start()
    >>> response = await bot.chat("こんにちは")
"""

from agentflow.skills.builtin.chatbot.chatbot import (
    ChatBotConfig,
    ChatBotSkill,
    ChatMessage,
    ChatSession,
)


__all__ = [
    "ChatBotConfig",
    "ChatBotSkill",
    "ChatMessage",
    "ChatSession",
]

