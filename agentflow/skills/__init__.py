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
from agentflow.skills.core.base import Skill, SkillMetadata

# ローダーとレジストリ
from agentflow.skills.core.loader import SkillLoader, SkillRegistry

# 自動進化コンポーネント
from agentflow.skills.core.matcher import MatchResult, SkillMatcher
from agentflow.skills.core.generator import GenerationResult, SkillGenerator
from agentflow.skills.core.validator import SkillValidator, ValidationResult
from agentflow.skills.core.persister import SkillPersister

# 統合エンジン
from agentflow.skills.core.engine import SkillEngine, SkillExecutionResult

# ルーター（Anthropic Skills体系準拠）
from agentflow.skills.core.router import RoutingResult, SkillMeta, SkillRouter

# ランタイム（Anthropic Skills体系準拠）
from agentflow.skills.core.runtime import ScriptResult, SkillRuntime

# ========== 組み込みスキル（builtin/）==========
# RAG Skill
from agentflow.skills.builtin.rag import RAGConfig, RAGResult, RAGSkill

# ChatBot Skill
from agentflow.skills.builtin.chatbot import (
    ChatBotConfig,
    ChatBotSkill,
    ChatMessage,
    ChatSession,
)

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
    ModeSwitcher,
    ModeSwitchError,
    ModeSwitchDenied,
    ModeTransition,
    SwitchDirection,
)

# ファクトリ
from agentflow.skills.factory import create_skill_gateway

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

# Browser スキル
from agentflow.skills.browser import (
    BrowserSkill,
    BrowserSkillConfig,
    BrowserSkillError,
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
from agentflow.skills.builtin.conversation_export import (
    ConversationExportSkill,
    ExportConfig,
    ExportFormat,
    ExportMessage,
)

# カレンダースキル
from agentflow.skills.builtin.calendar import (
    CalendarEvent,
    CalendarSkill,
    EventStatus,
    RecurrenceType,
    TimeSlot,
)

__all__ = [
    # 基本
    "Skill",
    "SkillMetadata",
    # ローダー
    "SkillLoader",
    "SkillRegistry",
    # 自動進化
    "SkillMatcher",
    "MatchResult",
    "SkillGenerator",
    "GenerationResult",
    "SkillValidator",
    "ValidationResult",
    "SkillPersister",
    # 統合エンジン
    "SkillEngine",
    "SkillExecutionResult",
    # ルーター（Anthropic Skills体系準拠）
    "SkillRouter",
    "SkillMeta",
    "RoutingResult",
    # ランタイム（Anthropic Skills体系準拠）
    "SkillRuntime",
    "ScriptResult",
    # 組み込みスキル - RAG
    "RAGConfig",
    "RAGResult",
    "RAGSkill",
    # 組み込みスキル - ChatBot
    "ChatBotConfig",
    "ChatBotSkill",
    "ChatMessage",
    "ChatSession",
    # ========== OS/Browser 制御スキル ==========
    # ゲートウェイ
    "SkillGateway",
    "GatewayConfig",
    "SkillDefinition",
    "SkillResult",
    "SkillCategory",
    "RiskLevel",
    "SkillGatewayError",
    "SkillNotFoundError",
    "SkillPermissionError",
    "HumanConfirmationRequired",
    # モード切替機構
    "ModeSwitcher",
    "ModeSwitchError",
    "ModeSwitchDenied",
    "ModeTransition",
    "SwitchDirection",
    # ファクトリ
    "create_skill_gateway",
    # OS スキル
    "OSSkillConfig",
    "ExecutionMode",
    "FileSystemSkill",
    "CommandSkill",
    "ProcessSkill",
    "NetworkSkill",
    "SystemInfoSkill",
    # Browser スキル
    "BrowserSkillConfig",
    "BrowserSkill",
    "BrowserSkillError",
    # Vision スキル
    "VisionSkill",
    "VisionConfig",
    "VisionResult",
    "VisionProvider",
    # Voice スキル
    "VoiceSkill",
    "VoiceConfig",
    "VoiceProvider",
    "TTSVoice",
    # 会話エクスポートスキル
    "ConversationExportSkill",
    "ExportConfig",
    "ExportFormat",
    "ExportMessage",
    # カレンダースキル
    "CalendarEvent",
    "CalendarSkill",
    "EventStatus",
    "RecurrenceType",
    "TimeSlot",
]

