"""AgentFlow 内置 Skills - Claude Code Skills 互換の能力パッケージ.

各 Skill は SKILL.md と Python 実装が同じディレクトリに配置されています。
これは Claude Code Skills の規範に準拠した構造です。

内置 Skills:
- rag: 検索増強生成（RAG）
- chatbot: 対話管理と Agent 連携
- vision: 画像認識・分析
- voice: 音声認識・合成
- conversation_export: 会話エクスポート
- bi_analytics: BI分析
- database-manager: データベース管理
- stripe-payment: Stripe 決済
- deployment-manager: デプロイ管理
- auth-provider: 認証

使用示例:
    >>> from agentflow.skills.builtin.rag import RAGSkill, RAGConfig
    >>> from agentflow.skills.builtin.chatbot import ChatBotSkill
    >>> from agentflow.skills.builtin.vision import VisionSkill
    >>> from agentflow.skills.builtin.voice import VoiceSkill
"""

# RAG Skill
# ChatBot Skill
from agentflow.skills.builtin.chatbot import (
    ChatBotConfig,
    ChatBotSkill,
    ChatMessage,
    ChatSession,
)

# Conversation Export Skill
from agentflow.skills.builtin.conversation_export import (
    ConversationExportSkill,
    ExportConfig,
    ExportFormat,
    ExportMessage,
)
from agentflow.skills.builtin.rag import RAGConfig, RAGResult, RAGSkill

# Vision Skill
from agentflow.skills.builtin.vision import (
    VisionConfig,
    VisionProvider,
    VisionResult,
    VisionSkill,
)

# Voice Skill
from agentflow.skills.builtin.voice import TTSVoice, VoiceConfig, VoiceProvider, VoiceSkill


__all__ = [
    "ChatBotConfig",
    # ChatBot
    "ChatBotSkill",
    "ChatMessage",
    "ChatSession",
    # Conversation Export
    "ConversationExportSkill",
    "ExportConfig",
    "ExportFormat",
    "ExportMessage",
    "RAGConfig",
    "RAGResult",
    # RAG
    "RAGSkill",
    "TTSVoice",
    "VisionConfig",
    "VisionProvider",
    "VisionResult",
    # Vision
    "VisionSkill",
    "VoiceConfig",
    "VoiceProvider",
    # Voice
    "VoiceSkill",
]

