"""AgentFlow Skills - 自動進化能力システム.

このモジュールは Claude Code Skills 完全互換の自動進化能力システムを提供します：

核心理念：
  用户需求 → 技能匹配 → 存在なら実行
                     → 不在なら自動生成 → 検証 → 固化 → 実行
  = 越用越厉害（使うほど強くなる）

機能：
- SKILL.md ベースの能力定義（Claude Code Skills 互換）
- 自動マッチング（triggers, description）
- 自動生成（LLM による新 Skill 作成）
- 自動固化（learned_skills ディレクトリへ保存）
- RAG/ChatBot などの組み込みスキル

参考：
- Anthropic Claude Code Skills 仕様
- https://code.claude.com/docs/en/skills
"""

# 基本クラス
from agentflow.skills.base import Skill, SkillMetadata

# ローダーとレジストリ
from agentflow.skills.loader import SkillLoader, SkillRegistry

# 自動進化コンポーネント
from agentflow.skills.matcher import MatchResult, SkillMatcher
from agentflow.skills.generator import GenerationResult, SkillGenerator
from agentflow.skills.validator import SkillValidator, ValidationResult
from agentflow.skills.persister import SkillPersister

# 統合エンジン
from agentflow.skills.engine import SkillEngine, SkillExecutionResult

# ルーター（Anthropic Skills体系準拠）
from agentflow.skills.router import RoutingResult, SkillMeta, SkillRouter

# ランタイム（Anthropic Skills体系準拠）
from agentflow.skills.runtime import ScriptResult, SkillRuntime

# 組み込みスキル
from agentflow.skills.chatbot import ChatBotConfig, ChatBotSkill, ChatMessage, ChatSession
from agentflow.skills.rag import RAGConfig, RAGResult, RAGSkill

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
]

