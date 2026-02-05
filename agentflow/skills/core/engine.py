# -*- coding: utf-8 -*-
"""Skill エンジン - 自動進化システムの統合インターフェース.

このモジュールは Skill システムの統合エンジンを提供します：
- Skill のマッチング
- 自動生成（マッチなし時）
- 検証と固化
- 越用越厉害（使うほど強くなる）

核心理念：
  用户需求 → 匹配技能 → 存在なら実行
                     → 不在なら自動生成 → 検証 → 固化 → 実行

設計原則：
- 松耦合：LLM プロバイダーを意識しない
"""

import logging
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

from agentflow.providers import get_llm
from agentflow.skills.core.base import Skill
from agentflow.skills.core.generator import GenerationResult, SkillGenerator
from agentflow.skills.core.loader import SkillLoader, SkillRegistry
from agentflow.skills.core.matcher import MatchResult, SkillMatcher
from agentflow.skills.core.persister import SkillPersister
from agentflow.skills.core.validator import SkillValidator

if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


@dataclass
class SkillExecutionResult:
    """Skill 実行結果.

    Attributes:
        skill: 使用した Skill
        matched: 既存 Skill にマッチしたか
        generated: 新規生成されたか
        saved: 固化されたか
        instructions: 実行指示内容
    """

    skill: Skill
    matched: bool
    generated: bool
    saved: bool
    instructions: str


class SkillEngine:
    """Skill 自動進化エンジン - 越用越厉害（松耦合設計）.

    LLM プロバイダー/モデルは環境変数から自動検出されます。

    Example:
        >>> engine = SkillEngine()
        >>> # 初回: マッチなし → 自動生成 → 固化
        >>> result = await engine.resolve("PDFからテキストを抽出")
        >>> print(result.generated)  # True
        >>>
        >>> # 2回目: 学習済み Skill にマッチ
        >>> result = await engine.resolve("PDFのテキストを取得")
        >>> print(result.matched)  # True
    """

    def __init__(
        self,
        skills_dirs: list[Path] | None = None,
        auto_learn: bool = True,
        match_threshold: float = 0.3,
        *,
        temperature: float | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM プロバイダーは環境変数から自動検出されます（松耦合設計）。

        Args:
            skills_dirs: Skill 検索ディレクトリ
            auto_learn: 自動学習を有効にするか
            match_threshold: マッチ閾値
            temperature: LLM 温度パラメータ（省略時はデフォルト）
        """
        # LLM プロバイダー（環境変数から自動検出・松耦合）
        self._llm: "LLMProvider" = get_llm(temperature=temperature)
        self._auto_learn = auto_learn
        self._logger = logging.getLogger(__name__)

        # コンポーネント初期化
        self._registry = SkillRegistry()
        self._loader = SkillLoader(self._registry)
        self._matcher = SkillMatcher(threshold=match_threshold)
        self._generator = SkillGenerator(self._llm)
        self._validator = SkillValidator()
        self._persister = SkillPersister()

        # Skill ロード
        self._load_skills(skills_dirs)

    def _load_skills(self, extra_dirs: list[Path] | None = None) -> None:
        """Skill を全ディレクトリから読み込み."""
        # デフォルトディレクトリ
        default_dirs = [
            Path.home() / ".agentflow" / "skills",        # グローバル
            Path.home() / ".agentflow" / "learned_skills", # 学習済み
            Path(".agentflow") / "skills",                 # プロジェクト
        ]

        all_dirs = default_dirs + (extra_dirs or [])

        for skill_dir in all_dirs:
            if skill_dir.exists():
                skills = self._loader.load_directory(skill_dir, recursive=True)
                for skill in skills:
                    self._matcher.add_skill(skill)

        self._logger.info(f"Loaded {len(self._matcher.skills)} skills")

    async def resolve(self, query: str) -> SkillExecutionResult:
        """クエリを Skill に解決（なければ生成）.

        Args:
            query: ユーザー要求

        Returns:
            Skill 実行結果
        """
        # 1. 既存 Skill を検索
        results = self._matcher.match(query)
        if results:
            skill = results[0].skill
            skill.increment_usage()
            self._logger.info(f"Matched skill: {skill.name}")
            return SkillExecutionResult(
                skill=skill,
                matched=True,
                generated=False,
                saved=False,
                instructions=skill.to_prompt(),
            )

        # 2. 自動学習が無効なら終了
        if not self._auto_learn:
            msg = f"No matching skill found for: {query}"
            raise ValueError(msg)

        # 3. 新規生成
        self._logger.info(f"No match found, generating new skill for: {query}")
        gen_result = await self._generator.generate(query)

        if not gen_result.success or not gen_result.skill:
            msg = f"Failed to generate skill: {gen_result.error}"
            raise RuntimeError(msg)

        skill = gen_result.skill

        # 4. 検証
        val_result = self._validator.validate(skill)
        if not val_result.valid:
            self._logger.warning(f"Generated skill validation failed: {val_result.errors}")
            # 警告があっても続行（learned skill として信頼度を下げる）
            skill.metadata.confidence = 0.5

        # 5. 固化
        saved = False
        try:
            self._persister.save(skill, scope="learned", force=True)
            self._matcher.add_skill(skill)
            saved = True
            self._logger.info(f"Saved new skill: {skill.name}")
        except Exception as e:
            self._logger.error(f"Failed to save skill: {e}")

        return SkillExecutionResult(
            skill=skill,
            matched=False,
            generated=True,
            saved=saved,
            instructions=skill.to_prompt(),
        )

    def find(self, query: str, top_k: int = 5) -> list[MatchResult]:
        """クエリにマッチする Skill を検索（生成なし）."""
        return self._matcher.match(query, top_k)

    def get_skill(self, name: str) -> Skill | None:
        """名前で Skill を取得."""
        return self._registry.get(name)

    def list_skills(self) -> list[Skill]:
        """全 Skill を取得."""
        return self._matcher.skills

    def add_skill(self, skill: Skill) -> None:
        """Skill を追加."""
        self._registry.register(skill.name, skill)
        self._matcher.add_skill(skill)

    def get_registry(self) -> SkillRegistry:
        """SkillRegistry を取得.

        Returns:
            内部の SkillRegistry インスタンス
        """
        return self._registry

