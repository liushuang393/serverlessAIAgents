"""Skill Router - 軽量Skill選択層（Anthropic Skills体系準拠）.

このモジュールは、Agent Loop から Skill を選択するための軽量ルーター層を提供します。
Anthropic の「漸進的披露」戦略に従い、3層読み込みを実装：

1. メタ情報層: name + description のみ（Router常駐）
2. SKILL.md層: 命中後に指示内容を読み込み
3. references/層: 必要時のみ参照資料を読み込み

これにより、Skill数が増えてもコンテキスト爆発を防止できます。

使用例:
    >>> router = SkillRouter()
    >>> await router.initialize()
    >>>
    >>> # 軽量ルーティング（メタ情報のみ使用）
    >>> result = router.route("市場トレンドを分析")
    >>> if result.matched:
    ...     # 命中した場合のみSKILL.mdを読み込み
    ...     instructions = result.skill.instructions
    ...     # Agentに渡す
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.skills.base import Skill


@dataclass
class RoutingResult:
    """Skill ルーティング結果.

    Attributes:
        matched: Skillにマッチしたか
        skill: マッチしたSkill（Noneなら未マッチ）
        score: マッチスコア (0.0-1.0)
        reason: マッチ理由
        fallback_to_rag: RAGフォールバック推奨か
    """

    matched: bool = False
    skill: Skill | None = None
    score: float = 0.0
    reason: str = ""
    fallback_to_rag: bool = False


@dataclass
class SkillMeta:
    """Skill軽量メタ情報（Router常駐用）.

    SKILL.md全文は読み込まず、name + description のみ保持。
    これによりSkill数が多くてもコンテキスト効率を維持。

    Attributes:
        name: Skill名
        description: 説明（触発場景 + 產出物）
        triggers: トリガーワード
        tags: タグ
        version: バージョン
        owner: オーナー
        stable: 安定版フラグ
    """

    name: str
    description: str = ""
    triggers: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)
    version: str = "1.0.0"
    owner: str = ""
    stable: bool = True


class SkillRouter:
    """Skill Router - Anthropic Skills体系準拠の軽量選択層.

    Agent Loop と Skills Library の間に位置し、
    「判断下一步做什么」と「这类事应该怎么做」を分離。

    設計原則:
    - メタ情報のみ常駐（description 20-50文字推奨）
    - SKILL.md は命中後に遅延読み込み
    - experimental Skill は自動ルーティング対象外

    Example:
        >>> router = SkillRouter()
        >>> await router.initialize()
        >>>
        >>> result = router.route("PDFからテキスト抽出")
        >>> if result.matched:
        ...     print(f"Matched: {result.skill.name}")
        ...     # SKILL.md の指示を取得
        ...     instructions = result.skill.instructions
    """

    # ルーティングの最低スコア閾値
    DEFAULT_THRESHOLD = 0.4

    def __init__(
        self,
        threshold: float = DEFAULT_THRESHOLD,
        *,
        include_experimental: bool = False,
    ) -> None:
        """初期化.

        Args:
            threshold: マッチと見なす最低スコア
            include_experimental: 実験的Skillを含めるか
        """
        self._threshold = threshold
        self._include_experimental = include_experimental
        self._skill_metas: list[SkillMeta] = []
        self._skill_cache: dict[str, Skill] = {}  # 遅延読み込みキャッシュ
        self._logger = logging.getLogger(__name__)
        self._initialized = False

    async def initialize(self, skill_dirs: list[str] | None = None) -> None:
        """ルーターを初期化（メタ情報のみ読み込み）.

        Args:
            skill_dirs: Skillディレクトリパス（省略時はデフォルト）
        """
        from pathlib import Path

        from agentflow.skills.loader import SkillLoader

        loader = SkillLoader()
        skills: list[Skill] = []

        if skill_dirs:
            # 指定ディレクトリから読み込み
            for dir_path in skill_dirs:
                loaded = loader.load_directory(Path(dir_path), recursive=True)
                skills.extend(loaded)
        else:
            # デフォルト: ビルトインSkillディレクトリから読み込み
            builtin_dir = Path(__file__).parent / "builtin"
            if builtin_dir.exists():
                skills = loader.load_directory(builtin_dir, recursive=True)

        for skill in skills:
            meta = SkillMeta(
                name=skill.name,
                description=skill.metadata.description,
                triggers=skill.metadata.triggers,
                tags=skill.metadata.tags,
                version=skill.metadata.version,
                owner=skill.metadata.author,
                stable=not skill.metadata.learned,  # 学習済み = experimental
            )

            # experimental フィルタ
            if not meta.stable and not self._include_experimental:
                continue

            self._skill_metas.append(meta)
            self._skill_cache[skill.name] = skill

        self._initialized = True
        self._logger.info(f"SkillRouter initialized: {len(self._skill_metas)} skills loaded")

    def route(self, query: str) -> RoutingResult:
        """クエリをSkillにルーティング（軽量判断）."""
        if not self._initialized:
            self._logger.warning("SkillRouter not initialized")
            return RoutingResult(matched=False, fallback_to_rag=True)

        if not query.strip():
            return RoutingResult(matched=False, fallback_to_rag=True)

        best_match: SkillMeta | None = None
        best_score = 0.0
        match_reason = ""

        query_lower = query.lower()

        for meta in self._skill_metas:
            score, reason = self._calculate_match_score(meta, query_lower)
            if score > best_score:
                best_score = score
                best_match = meta
                match_reason = reason

        if best_match and best_score >= self._threshold:
            skill = self._skill_cache.get(best_match.name)
            return RoutingResult(
                matched=True,
                skill=skill,
                score=best_score,
                reason=match_reason,
                fallback_to_rag=False,
            )

        return RoutingResult(matched=False, fallback_to_rag=True, reason="No matching skill")

    def _calculate_match_score(
        self, meta: SkillMeta, query_lower: str
    ) -> tuple[float, str]:
        """マッチスコアを計算（軽量・高速）.

        Args:
            meta: Skillメタ情報
            query_lower: 小文字化されたクエリ

        Returns:
            (スコア, 理由) のタプル
        """
        score = 0.0
        reasons: list[str] = []

        # 1. トリガー完全一致（最高優先度）
        for trigger in meta.triggers:
            if trigger.lower() in query_lower:
                score = max(score, 0.9)
                reasons.append(f"trigger:{trigger}")

        # 2. 名前一致
        if meta.name.lower() in query_lower:
            score = max(score, 0.8)
            reasons.append("name")

        # 3. 説明部分一致
        desc_words = meta.description.lower().split()
        matched_words = [w for w in desc_words if len(w) > 2 and w in query_lower]
        if matched_words:
            desc_score = min(len(matched_words) * 0.15, 0.7)
            if desc_score > score:
                score = desc_score
                reasons.append(f"desc:{','.join(matched_words[:3])}")

        # 4. タグ一致
        for tag in meta.tags:
            if tag.lower() in query_lower:
                score = max(score, 0.6)
                reasons.append(f"tag:{tag}")

        return score, "; ".join(reasons) if reasons else "no match"

    def get_skill(self, name: str) -> Skill | None:
        """名前でSkillを取得（遅延読み込み済みキャッシュから）.

        Args:
            name: Skill名

        Returns:
            Skillインスタンス、見つからない場合None
        """
        return self._skill_cache.get(name)

    def list_skills(self) -> list[SkillMeta]:
        """登録済みSkillメタ情報一覧.

        Returns:
            SkillMetaリスト
        """
        return self._skill_metas.copy()

    @property
    def skill_count(self) -> int:
        """登録Skill数."""
        return len(self._skill_metas)

