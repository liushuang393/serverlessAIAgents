"""AgentFlow Skills - 自動進化能力システム（Claude Code Skills 互換）.

このモジュールは Claude Code Skills 完全互換の自動進化能力システムを提供します：

核心理念：
  用户需求 → 技能匹配 → 存在なら実行
                     → 不在なら自動生成 → 検証 → 固化 → 実行
  = 越用越厉害（使うほど強くなる）

ディレクトリ構造：
- core/: フレームワーク基盤（SkillEngine, SkillLoader, etc.）
- builtin/: 組み込み Skill（SKILL.md + Python 実装が同じディレクトリ）
- os/: OS 制御スキル
- browser/: ブラウザ制御スキル

参考：
- Anthropic Claude Code Skills 仕様
- https://code.claude.com/docs/en/skills
"""

# ========== フレームワーク基盤（core/）==========
# 基本クラス
# Browser スキル
from agentflow.skills.browser import (
    BrowserSkill,
    BrowserSkillConfig,
    BrowserSkillError,
)

# ChatBot Skill
from agentflow.skills.builtin.chatbot import (
    ChatBotConfig,
    ChatBotSkill,
    ChatMessage,
    ChatSession,
)

# ========== 組み込みスキル（builtin/）==========
# RAG Skill
from agentflow.skills.builtin.rag import RAGConfig, RAGResult, RAGSkill
from agentflow.skills.core.base import Skill, SkillMetadata

# 統合エンジン
from agentflow.skills.core.engine import SkillEngine, SkillExecutionResult
from agentflow.skills.core.generator import GenerationResult, SkillGenerator

# ローダーとレジストリ
from agentflow.skills.core.loader import SkillLoader, SkillRegistry

# 自動進化コンポーネント
from agentflow.skills.core.matcher import MatchResult, SkillMatcher
from agentflow.skills.core.persister import SkillPersister

# ルーター（Anthropic Skills体系準拠）
from agentflow.skills.core.router import RoutingResult, SkillMeta, SkillRouter

# ランタイム（Anthropic Skills体系準拠）
from agentflow.skills.core.runtime import ScriptResult, SkillRuntime
from agentflow.skills.core.validator import SkillValidator, ValidationResult

# ファクトリ
from agentflow.skills.factory import create_skill_gateway

# ========== OS/Browser 制御スキル（セキュリティ隔離設計） ==========
# ゲートウェイ
from agentflow.skills.gateway import (
    GatewayConfig,
    HumanConfirmationRequired,
    RiskLevel,
    SkillCategory,
    SkillDefinition,
    SkillGateway,
    SkillGatewayError,
    SkillNotFoundError,
    SkillPermissionError,
    SkillResult,
)

# モード切替機構
from agentflow.skills.mode_switcher import (
    ModeSwitchDenied,
    ModeSwitcher,
    ModeSwitchError,
    ModeTransition,
    SwitchDirection,
)

# OS スキル
from agentflow.skills.os import (
    CommandSkill,
    ExecutionMode,
    FileSystemSkill,
    NetworkSkill,
    OSSkillConfig,
    ProcessSkill,
    SystemInfoSkill,
)


# Vision スキル（画像認識）
try:
    from agentflow.skills.builtin.vision import (
        VisionConfig,
        VisionProvider,
        VisionResult,
        VisionSkill,
    )
except ImportError:
    VisionSkill = None  # type: ignore[misc, assignment]
    VisionConfig = None  # type: ignore[misc, assignment]
    VisionResult = None  # type: ignore[misc, assignment]
    VisionProvider = None  # type: ignore[misc, assignment]

# Voice スキル（音声認識・合成）
try:
    from agentflow.skills.builtin.voice import (
        TTSVoice,
        VoiceConfig,
        VoiceProvider,
        VoiceSkill,
    )
except ImportError:
    VoiceSkill = None  # type: ignore[misc, assignment]
    VoiceConfig = None  # type: ignore[misc, assignment]
    VoiceProvider = None  # type: ignore[misc, assignment]
    TTSVoice = None  # type: ignore[misc, assignment]

# 会話エクスポートスキル
# カレンダースキル
from agentflow.skills.builtin.calendar import (
    CalendarEvent,
    CalendarSkill,
    EventStatus,
    RecurrenceType,
    TimeSlot,
)
from agentflow.skills.builtin.conversation_export import (
    ConversationExportSkill,
    ExportConfig,
    ExportFormat,
    ExportMessage,
)


__all__ = [
    "BrowserSkill",
    # Browser スキル
    "BrowserSkillConfig",
    "BrowserSkillError",
    # カレンダースキル
    "CalendarEvent",
    "CalendarSkill",
    # 組み込みスキル - ChatBot
    "ChatBotConfig",
    "ChatBotSkill",
    "ChatMessage",
    "ChatSession",
    "CommandSkill",
    # 会話エクスポートスキル
    "ConversationExportSkill",
    "EventStatus",
    "ExecutionMode",
    "ExportConfig",
    "ExportFormat",
    "ExportMessage",
    "FileSystemSkill",
    "GatewayConfig",
    "GenerationResult",
    "HumanConfirmationRequired",
    "MatchResult",
    "ModeSwitchDenied",
    "ModeSwitchError",
    # モード切替機構
    "ModeSwitcher",
    "ModeTransition",
    "NetworkSkill",
    # OS スキル
    "OSSkillConfig",
    "ProcessSkill",
    # 組み込みスキル - RAG
    "RAGConfig",
    "RAGResult",
    "RAGSkill",
    "RecurrenceType",
    "RiskLevel",
    "RoutingResult",
    "ScriptResult",
    # 基本
    "Skill",
    "SkillCategory",
    "SkillDefinition",
    # 統合エンジン
    "SkillEngine",
    "SkillExecutionResult",
    # ========== OS/Browser 制御スキル ==========
    # ゲートウェイ
    "SkillGateway",
    "SkillGatewayError",
    "SkillGenerator",
    # ローダー
    "SkillLoader",
    # 自動進化
    "SkillMatcher",
    "SkillMeta",
    "SkillMetadata",
    "SkillNotFoundError",
    "SkillPermissionError",
    "SkillPersister",
    "SkillRegistry",
    "SkillResult",
    # ルーター（Anthropic Skills体系準拠）
    "SkillRouter",
    # ランタイム（Anthropic Skills体系準拠）
    "SkillRuntime",
    "SkillValidator",
    "SwitchDirection",
    "SystemInfoSkill",
    "TTSVoice",
    "TimeSlot",
    "ValidationResult",
    "VisionConfig",
    "VisionProvider",
    "VisionResult",
    # Vision スキル
    "VisionSkill",
    "VoiceConfig",
    "VoiceProvider",
    # Voice スキル
    "VoiceSkill",
    # ファクトリ
    "create_skill_gateway",
]

