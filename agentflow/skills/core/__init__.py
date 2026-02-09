"""Skills フレームワーク基盤 - 自動進化システムのコアコンポーネント.

このモジュールは Skills システムの基盤インフラを提供します：
- Skill 基本クラスとメタデータ
- ローダーとレジストリ
- マッチング・生成・検証・固化
- 統合エンジン
- ルーターとランタイム

これらは Skills システムの内部実装であり、
通常は agentflow.skills から直接インポートして使用します。

Example:
    >>> from agentflow.skills import SkillEngine, Skill
    >>> engine = SkillEngine()
    >>> result = await engine.resolve("PDF からテキストを抽出")
"""

# 基本クラス
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


__all__ = [
    "GenerationResult",
    "MatchResult",
    "RoutingResult",
    "ScriptResult",
    # 基本
    "Skill",
    # 統合エンジン
    "SkillEngine",
    "SkillExecutionResult",
    "SkillGenerator",
    # ローダー
    "SkillLoader",
    # 自動進化
    "SkillMatcher",
    "SkillMeta",
    "SkillMetadata",
    "SkillPersister",
    "SkillRegistry",
    # ルーター
    "SkillRouter",
    # ランタイム
    "SkillRuntime",
    "SkillValidator",
    "ValidationResult",
]

