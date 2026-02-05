# -*- coding: utf-8 -*-
"""Skill マッチャー - ユーザー要求と Skill のマッチング.

このモジュールは入力クエリに最適な Skill を見つける機能を提供します：
- トリガーワードマッチング
- 説明文ベースのセマンティックマッチング
- スコアリングとランキング
"""

import logging
from dataclasses import dataclass

from agentflow.skills.core.base import Skill


@dataclass
class MatchResult:
    """マッチング結果.

    Attributes:
        skill: マッチした Skill
        score: マッチスコア (0.0-1.0)
        reason: マッチ理由
    """

    skill: Skill
    score: float
    reason: str


class SkillMatcher:
    """Skill マッチャー - クエリに最適な Skill を検索.

    Example:
        >>> matcher = SkillMatcher(skills)
        >>> results = matcher.match("PDFからテキストを抽出したい")
        >>> if results:
        ...     best = results[0]
        ...     print(f"Best match: {best.skill.name} ({best.score:.2f})")
    """

    # マッチと見なす最低スコア
    DEFAULT_THRESHOLD = 0.3

    def __init__(
        self,
        skills: list[Skill] | None = None,
        threshold: float = DEFAULT_THRESHOLD,
    ) -> None:
        """初期化.

        Args:
            skills: 検索対象 Skill リスト
            threshold: マッチと見なす最低スコア
        """
        self._skills: list[Skill] = skills or []
        self._threshold = threshold
        self._logger = logging.getLogger(__name__)

    def add_skill(self, skill: Skill) -> None:
        """Skill を追加.

        Args:
            skill: 追加する Skill
        """
        self._skills.append(skill)

    def remove_skill(self, name: str) -> bool:
        """Skill を削除.

        Args:
            name: 削除する Skill 名

        Returns:
            削除成功したか
        """
        for i, skill in enumerate(self._skills):
            if skill.name == name:
                self._skills.pop(i)
                return True
        return False

    def match(self, query: str, top_k: int = 5) -> list[MatchResult]:
        """クエリにマッチする Skill を検索.

        Args:
            query: ユーザー入力
            top_k: 返す結果の最大数

        Returns:
            マッチ結果リスト（スコア降順）
        """
        if not query.strip():
            return []

        results: list[MatchResult] = []

        for skill in self._skills:
            score = skill.matches(query)
            if score >= self._threshold:
                reason = self._explain_match(skill, query, score)
                results.append(MatchResult(skill=skill, score=score, reason=reason))

        # スコア降順でソート
        results.sort(key=lambda x: x.score, reverse=True)
        return results[:top_k]

    def find_best(self, query: str) -> Skill | None:
        """最もマッチする Skill を取得.

        Args:
            query: ユーザー入力

        Returns:
            最適な Skill、見つからない場合 None
        """
        results = self.match(query, top_k=1)
        return results[0].skill if results else None

    def has_match(self, query: str) -> bool:
        """マッチする Skill が存在するか確認.

        Args:
            query: ユーザー入力

        Returns:
            マッチが存在するか
        """
        return self.find_best(query) is not None

    def _explain_match(self, skill: Skill, query: str, score: float) -> str:
        """マッチ理由を説明.

        Args:
            skill: マッチした Skill
            query: クエリ
            score: スコア

        Returns:
            説明文
        """
        query_lower = query.lower()
        reasons: list[str] = []

        # トリガーマッチをチェック
        for trigger in skill.metadata.triggers:
            if trigger.lower() in query_lower:
                reasons.append(f"trigger '{trigger}'")

        # 名前マッチをチェック
        if skill.name.lower() in query_lower:
            reasons.append("name match")

        # タグマッチをチェック
        for tag in skill.metadata.tags:
            if tag.lower() in query_lower:
                reasons.append(f"tag '{tag}'")

        if reasons:
            return f"Matched by: {', '.join(reasons)}"
        return f"Semantic similarity: {score:.2f}"

    @property
    def skills(self) -> list[Skill]:
        """登録済み Skill リスト."""
        return self._skills.copy()

    @property
    def threshold(self) -> float:
        """マッチ閾値."""
        return self._threshold

