# -*- coding: utf-8 -*-
"""品質評価コンポーネント - 多次元品質評価.

DeepAgentのQualityReviewから抽出した汎用品質評価器。

使用例:
    >>> evaluator = QualityEvaluator(threshold=70.0)
    >>> result = evaluator.evaluate({"stage1": "done"}, progress={"completed": 3, "total": 3})
"""

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

_logger = logging.getLogger(__name__)


class QualityDimension(str, Enum):
    """品質評価次元."""
    COMPLETENESS = "completeness"  # 完全性
    ACCURACY = "accuracy"          # 正確性
    CONSISTENCY = "consistency"    # 一貫性
    EFFICIENCY = "efficiency"      # 効率性
    CLARITY = "clarity"            # 明確性


@dataclass
class QualityResult:
    """品質評価結果."""
    is_acceptable: bool
    score: float
    verdict: str  # pass/revise/reject
    dimension_scores: dict[str, float] = field(default_factory=dict)
    issues: list[str] = field(default_factory=list)
    suggestions: list[str] = field(default_factory=list)
    confidence: float = 0.5


class QualityEvaluator:
    """多次元品質評価器.
    
    完全性・正確性・一貫性・効率性・明確性の5次元で評価。
    """
    
    def __init__(
        self,
        threshold: float = 70.0,
        weights: dict[str, float] | None = None,
    ):
        """初期化.
        
        Args:
            threshold: 合格閾値（0-100）
            weights: 次元別重み（省略時は均等）
        """
        self._threshold = threshold
        self._weights = weights or {
            QualityDimension.COMPLETENESS.value: 0.25,
            QualityDimension.ACCURACY.value: 0.25,
            QualityDimension.CONSISTENCY.value: 0.2,
            QualityDimension.EFFICIENCY.value: 0.15,
            QualityDimension.CLARITY.value: 0.15,
        }
    
    def evaluate(
        self,
        results: dict[str, Any],
        progress: dict[str, Any] | None = None,
        context: dict[str, Any] | None = None,
    ) -> QualityResult:
        """結果を多次元評価.
        
        Args:
            results: 実行結果
            progress: 進捗情報 {completed, failed, total}
            context: 追加コンテキスト
            
        Returns:
            品質評価結果
        """
        progress = progress or {}
        
        # 各次元のスコアを計算
        dimension_scores = self._calculate_dimensions(results, progress)
        
        # 加重平均スコア
        total_score = sum(
            dimension_scores.get(dim, 70.0) * weight
            for dim, weight in self._weights.items()
        )
        
        # 問題点・改善提案を生成
        issues, suggestions = self._analyze_issues(dimension_scores, results)
        
        # 判定
        is_acceptable = total_score >= self._threshold and not self._has_critical_issue(issues)
        verdict = self._determine_verdict(total_score, issues)
        
        return QualityResult(
            is_acceptable=is_acceptable,
            score=round(total_score, 1),
            verdict=verdict,
            dimension_scores=dimension_scores,
            issues=issues,
            suggestions=suggestions,
            confidence=0.7,
        )
    
    def _calculate_dimensions(
        self,
        results: dict[str, Any],
        progress: dict[str, Any],
    ) -> dict[str, float]:
        """各次元のスコアを計算."""
        scores = {}
        
        # 完全性: タスク完了率
        completed = progress.get("completed", 0)
        total = progress.get("total", 1)
        failed = progress.get("failed", 0)
        scores[QualityDimension.COMPLETENESS.value] = (completed / max(total, 1)) * 100
        
        # 正確性: エラー率から推定
        if failed == 0:
            scores[QualityDimension.ACCURACY.value] = 80.0
        else:
            scores[QualityDimension.ACCURACY.value] = max(40.0, 80 - failed * 10)
        
        # 一貫性: 結果の整合性（簡易チェック）
        scores[QualityDimension.CONSISTENCY.value] = 75.0 if results else 50.0
        
        # 効率性: デフォルト
        scores[QualityDimension.EFFICIENCY.value] = 70.0
        
        # 明確性: 結果の存在チェック
        scores[QualityDimension.CLARITY.value] = 70.0 if results else 40.0
        
        return scores
    
    def _analyze_issues(
        self,
        scores: dict[str, float],
        results: dict[str, Any],
    ) -> tuple[list[str], list[str]]:
        """問題点と改善提案を分析."""
        issues = []
        suggestions = []
        
        for dim, score in scores.items():
            if score < 60:
                issues.append(f"{dim}が低い（{score:.0f}点）")
                suggestions.append(f"{dim}の改善が必要")
        
        if not results:
            issues.append("結果が空")
            suggestions.append("処理を再実行してください")
        
        return issues[:5], suggestions[:3]
    
    def _has_critical_issue(self, issues: list[str]) -> bool:
        """重大な問題があるか."""
        critical_keywords = ["空", "失敗", "エラー"]
        return any(kw in issue for issue in issues for kw in critical_keywords)
    
    def _determine_verdict(self, score: float, issues: list[str]) -> str:
        """判定を決定."""
        if score >= self._threshold and not issues:
            return "pass"
        elif score >= self._threshold * 0.8:
            return "revise"
        else:
            return "reject"

