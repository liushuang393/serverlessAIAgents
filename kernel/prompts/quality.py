"""プロンプト品質チェックユーティリティ.

プロンプトの冗長性、完全性、最小性をテストする。
テスト用途 + CI連携向け。

チェック項目（設計文書 Section 5.6 準拠）:
    1. 目的に直接答えているか
    2. 不要な説明が混ざっていないか（冗長性）
    3. 必須条件を満たしているか（完全性）
    4. もっと短くしても意味が落ちないか（最小性）
"""

from __future__ import annotations

import re
from collections import Counter
from dataclasses import dataclass, field
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from kernel.prompts.models import AssembledPrompt
    from kernel.prompts.patterns import PatternConfig


@dataclass
class QualityReport:
    """品質レポート.

    Attributes:
        redundancy_score: 0.0 = 冗長なし、1.0 = 完全に冗長
        completeness_score: 0.0 = 不完全、1.0 = 完全
        minimality_score: 0.0 = 膨張、1.0 = 最小
        issues: 検出された問題リスト
        suggestions: 改善提案リスト
    """

    redundancy_score: float = 0.0
    completeness_score: float = 0.0
    minimality_score: float = 0.0
    issues: list[str] = field(default_factory=list)
    suggestions: list[str] = field(default_factory=list)

    @property
    def overall_score(self) -> float:
        """総合スコア（3指標の平均）."""
        scores = [
            1.0 - self.redundancy_score,  # 冗長性は反転
            self.completeness_score,
            self.minimality_score,
        ]
        return sum(scores) / len(scores)

    @property
    def passed(self) -> bool:
        """品質基準を満たしているか（総合スコア0.6以上）."""
        return self.overall_score >= 0.6


# 冗長性検出: 繰り返しフレーズの最小長
_MIN_PHRASE_LENGTH = 8


class PromptQualityChecker:
    """プロンプト品質チェッカー."""

    def check_redundancy(self, prompt: AssembledPrompt) -> float:
        """冗長性を検出.

        同一フレーズの繰り返し、レイヤー間の重複を検出する。

        Args:
            prompt: チェック対象プロンプト

        Returns:
            冗長性スコア（0.0=冗長なし、1.0=完全冗長）
        """
        text = prompt.system_prompt
        if not text:
            return 0.0

        lines = [line.strip() for line in text.split("\n") if line.strip()]
        if len(lines) <= 1:
            return 0.0

        # 行レベルの重複検出
        line_counts = Counter(lines)
        duplicates = sum(count - 1 for count in line_counts.values() if count > 1)
        line_redundancy = duplicates / len(lines) if lines else 0.0

        # フレーズレベルの重複検出（N-gram方式）
        phrases = self._extract_phrases(text)
        phrase_counts = Counter(phrases)
        phrase_duplicates = sum(count - 1 for count in phrase_counts.values() if count > 1)
        phrase_redundancy = phrase_duplicates / len(phrases) if phrases else 0.0

        # 加重平均（行重複をより重視）
        return min(1.0, line_redundancy * 0.7 + phrase_redundancy * 0.3)

    def check_completeness(
        self,
        prompt: AssembledPrompt,
        pattern: PatternConfig,
    ) -> float:
        """パターン要件に対する完全性を検証.

        必須レイヤーが全て含まれているかチェック。

        Args:
            prompt: チェック対象プロンプト
            pattern: パターン設定

        Returns:
            完全性スコア（0.0=不完全、1.0=完全）
        """
        if not pattern.required_layers:
            return 1.0

        included = set(prompt.layers_used)
        required = set(pattern.required_layers)
        matched = included & required
        return len(matched) / len(required)

    def check_minimality(
        self,
        prompt: AssembledPrompt,
    ) -> float:
        """トークン予算の使用効率を評価.

        予算に対する使用率を基にスコアを算出。
        使用率50%-90%が最適（低すぎは活用不足、高すぎは圧迫）。

        Args:
            prompt: チェック対象プロンプト

        Returns:
            最小性スコア（0.0=膨張、1.0=最小）
        """
        if prompt.token_budget <= 0:
            return 1.0

        usage_ratio = prompt.token_count / prompt.token_budget

        # 最適ゾーン: 30%-80%
        if 0.3 <= usage_ratio <= 0.8:
            return 1.0
        if usage_ratio < 0.3:
            # 活用不足（情報が少なすぎる可能性）
            return usage_ratio / 0.3
        # 圧迫（トークン超過に近い）
        return max(0.0, 1.0 - (usage_ratio - 0.8) / 0.2)

    def full_check(
        self,
        prompt: AssembledPrompt,
        pattern: PatternConfig,
    ) -> QualityReport:
        """全項目チェック.

        Args:
            prompt: チェック対象プロンプト
            pattern: パターン設定

        Returns:
            品質レポート
        """
        redundancy = self.check_redundancy(prompt)
        completeness = self.check_completeness(prompt, pattern)
        minimality = self.check_minimality(prompt)

        issues: list[str] = []
        suggestions: list[str] = []

        # 冗長性の問題
        if redundancy > 0.3:
            issues.append(f"冗長性が高い (スコア: {redundancy:.2f})")
            suggestions.append("重複するフレーズや行を統合してください")

        # 完全性の問題
        if completeness < 1.0:
            missing = set(pattern.required_layers) - set(prompt.layers_used)
            issues.append(f"必須レイヤー欠落: {', '.join(missing)}")
            suggestions.append("欠落レイヤーのデータを提供してください")

        # 最小性の問題
        if minimality < 0.5:
            usage_ratio = prompt.token_count / prompt.token_budget if prompt.token_budget > 0 else 0
            if usage_ratio < 0.3:
                issues.append("トークン使用率が低い（情報不足の可能性）")
                suggestions.append("必要な情報が欠けていないか確認してください")
            else:
                issues.append("トークン予算に対して過大")
                suggestions.append("不要な情報を削減してください")

        # ドロップされたレイヤーの警告
        if prompt.layers_dropped:
            issues.append(f"予算不足でドロップされたレイヤー: {', '.join(prompt.layers_dropped)}")
            suggestions.append("トークン予算を増やすか、情報を圧縮してください")

        return QualityReport(
            redundancy_score=redundancy,
            completeness_score=completeness,
            minimality_score=minimality,
            issues=issues,
            suggestions=suggestions,
        )

    def _extract_phrases(self, text: str) -> list[str]:
        """テキストからフレーズを抽出（冗長性チェック用）."""
        # 句読点で分割してフレーズを抽出
        phrases = re.split(r"[。、\n,.\-:：]", text)
        return [p.strip() for p in phrases if len(p.strip()) >= _MIN_PHRASE_LENGTH]
