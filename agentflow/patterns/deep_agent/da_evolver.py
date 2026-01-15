# -*- coding: utf-8 -*-
"""DeepAgent 自己進化システム.

DeepAgentsの「自己進化」原則を実装:
- 成功パターンの学習と再利用
- フィードバックからの改善
- 動的なSkill/Tool拡張

進化の3つの軸:
1. Pattern Learning: 成功した実行パターンを記録・再利用
2. Feedback Integration: ユーザー/システムフィードバックを統合
3. Capability Extension: 新しいSkill/Toolの動的追加

参考:
- DeepAgents Self-Evolution (arXiv:2504.04755)
- Anthropic Context Engineering (2025.09)
"""

from __future__ import annotations

import hashlib
import logging
from datetime import datetime
from typing import Any

from agentflow.patterns.deep_agent.da_models import (
    CognitiveAnalysis,
    EvolutionRecord,
    QualityReview,
    TodoItem,
)
from agentflow.patterns.deep_agent.da_stores import EvolutionStore, MemoryEvolutionStore

_logger = logging.getLogger(__name__)


class SelfEvolver:
    """自己進化エンジン.

    成功パターンの学習、フィードバック統合、能力拡張を担当。

    Example:
        >>> evolver = SelfEvolver(store=my_evolution_store)
        >>> await evolver.learn_success_pattern(todos, results)
        >>> similar = await evolver.find_similar_pattern(analysis)
    """

    def __init__(
        self,
        store: EvolutionStore | None = None,
        min_confidence: float = 0.6,
    ) -> None:
        """初期化.

        Args:
            store: 進化ストア（永続化用）
            min_confidence: パターン適用の最小信頼度
        """
        self._store = store or MemoryEvolutionStore()
        self._min_confidence = min_confidence
        self._session_patterns: dict[str, dict[str, Any]] = {}

    # =========================================================================
    # パターン学習
    # =========================================================================

    async def learn_success_pattern(
        self,
        todos: list[TodoItem],
        results: dict[str, Any],
        analysis: CognitiveAnalysis | None = None,
        review: QualityReview | None = None,
    ) -> str | None:
        """成功パターンを学習.

        Args:
            todos: 実行されたTodoList
            results: 実行結果
            analysis: 認知分析結果
            review: 品質評審結果

        Returns:
            パターンキー（保存された場合）
        """
        # 品質チェック
        if review and not review.is_acceptable:
            _logger.debug("品質不合格のためパターン学習スキップ")
            return None

        if review and review.score < 70:
            _logger.debug("スコア不足のためパターン学習スキップ: %.1f", review.score)
            return None

        # パターンキー生成
        pattern_key = self._generate_pattern_key(todos, analysis)

        # パターンデータ構築
        pattern_data = {
            "key": pattern_key,
            "intent": analysis.intent if analysis else "",
            "domains": analysis.domains if analysis else [],
            "complexity": analysis.complexity if analysis else "medium",
            "todo_structure": [
                {
                    "agent_type": t.agent_type,
                    "task_template": self._extract_template(t.task),
                    "tools": t.tools,
                    "skills": t.skills,
                    "dependencies_count": len(t.dependencies),
                }
                for t in todos
            ],
            "success_count": 1,
            "avg_score": review.score if review else 80.0,
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat(),
        }

        # 既存パターンとマージ
        existing = await self._store.load_pattern(pattern_key)
        if existing:
            pattern_data["success_count"] = existing.get("success_count", 0) + 1
            pattern_data["avg_score"] = (
                existing.get("avg_score", 80) * existing.get("success_count", 1)
                + (review.score if review else 80)
            ) / pattern_data["success_count"]
            pattern_data["created_at"] = existing.get("created_at", pattern_data["created_at"])

        await self._store.save_pattern(pattern_key, pattern_data)
        self._session_patterns[pattern_key] = pattern_data

        _logger.info("パターン学習完了: %s (成功回数: %d)", pattern_key, pattern_data["success_count"])
        return pattern_key

    def _generate_pattern_key(
        self,
        todos: list[TodoItem],
        analysis: CognitiveAnalysis | None,
    ) -> str:
        """パターンキーを生成."""
        components = []
        if analysis:
            components.append(analysis.intent[:50])
            components.extend(sorted(analysis.domains)[:3])
            components.append(analysis.complexity)
        components.extend(t.agent_type for t in todos[:5])
        key_str = "|".join(str(c) for c in components)
        return f"pattern-{hashlib.md5(key_str.encode()).hexdigest()[:12]}"

    def _extract_template(self, task: str) -> str:
        """タスクからテンプレートを抽出（具体値を抽象化）."""
        import re
        template = re.sub(r'\d+', '{N}', task)
        template = re.sub(r'"[^"]*"', '"{STR}"', template)
        template = re.sub(r"'[^']*'", "'{STR}'", template)
        return template[:100]

    # =========================================================================
    # パターン検索・適用
    # =========================================================================

    async def find_similar_pattern(
        self,
        analysis: CognitiveAnalysis,
        threshold: float = 0.5,
    ) -> dict[str, Any] | None:
        """類似パターンを検索.

        Args:
            analysis: 認知分析結果
            threshold: 類似度閾値

        Returns:
            最も類似したパターン（なければNone）
        """
        patterns = await self._store.list_patterns(limit=50)
        if not patterns:
            return None

        best_match = None
        best_score = threshold

        for pattern in patterns:
            similarity = self._calculate_similarity(analysis, pattern)
            if similarity > best_score:
                best_score = similarity
                best_match = pattern

        if best_match:
            _logger.info("類似パターン発見: %s (類似度: %.2f)", best_match.get("key"), best_score)

        return best_match

    def _calculate_similarity(
        self,
        analysis: CognitiveAnalysis,
        pattern: dict[str, Any],
    ) -> float:
        """類似度を計算."""
        score = 0.0

        # ドメイン一致
        pattern_domains = set(pattern.get("domains", []))
        analysis_domains = set(analysis.domains)
        if pattern_domains and analysis_domains:
            domain_overlap = len(pattern_domains & analysis_domains)
            domain_total = len(pattern_domains | analysis_domains)
            score += 0.4 * (domain_overlap / domain_total if domain_total > 0 else 0)

        # 複雑度一致
        if pattern.get("complexity") == analysis.complexity:
            score += 0.2

        # 意図の類似性（簡易版：キーワード一致）
        pattern_intent = pattern.get("intent", "").lower()
        analysis_intent = analysis.intent.lower()
        if pattern_intent and analysis_intent:
            pattern_words = set(pattern_intent.split())
            analysis_words = set(analysis_intent.split())
            word_overlap = len(pattern_words & analysis_words)
            word_total = len(pattern_words | analysis_words)
            score += 0.3 * (word_overlap / word_total if word_total > 0 else 0)

        # 成功回数ボーナス
        success_count = pattern.get("success_count", 1)
        if success_count >= 5:
            score += 0.1

        return min(1.0, score)

    async def apply_pattern(
        self,
        pattern: dict[str, Any],
        context: dict[str, Any],  # noqa: ARG002 - 将来の拡張用
    ) -> list[TodoItem]:
        """パターンを適用してTodoListを生成.

        Args:
            pattern: 適用するパターン
            context: 現在のコンテキスト（将来の拡張用）

        Returns:
            生成されたTodoList

        Note:
            asyncは将来のLLM統合のために保持。
        """
        todos = []
        for i, todo_template in enumerate(pattern.get("todo_structure", [])):
            todo = TodoItem(
                task=f"[パターン適用] {todo_template.get('task_template', 'タスク')}",
                agent_type=todo_template.get("agent_type", "execution"),
                tools=todo_template.get("tools", []),
                skills=todo_template.get("skills", []),
                priority=len(pattern.get("todo_structure", [])) - i,
            )
            todos.append(todo)

        _logger.info("パターン適用: %d個のTodo生成", len(todos))
        return todos

    # =========================================================================
    # フィードバック統合
    # =========================================================================

    async def process_feedback(
        self,
        feedback_type: str,
        feedback_content: str,
        related_pattern_key: str | None = None,
    ) -> EvolutionRecord:
        """フィードバックを処理.

        Args:
            feedback_type: フィードバック種別（positive/negative/suggestion）
            feedback_content: フィードバック内容
            related_pattern_key: 関連パターンキー

        Returns:
            進化記録
        """
        record = EvolutionRecord(
            event_type="feedback",
            pattern=related_pattern_key or "",
            confidence=0.7 if feedback_type == "positive" else 0.5,
        )

        await self._store.save_feedback(record)

        # 関連パターンの信頼度を調整
        if related_pattern_key:
            pattern = await self._store.load_pattern(related_pattern_key)
            if pattern:
                adjustment = 0.1 if feedback_type == "positive" else -0.1
                pattern["avg_score"] = max(0, min(100, pattern.get("avg_score", 80) + adjustment * 10))
                await self._store.save_pattern(related_pattern_key, pattern)

        _logger.info("フィードバック処理完了: %s", feedback_type)
        return record

    # =========================================================================
    # 統計
    # =========================================================================

    async def get_evolution_stats(self) -> dict[str, Any]:
        """進化統計を取得."""
        store_stats = await self._store.get_stats()
        return {
            **store_stats,
            "session_patterns": len(self._session_patterns),
            "min_confidence": self._min_confidence,
        }


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["SelfEvolver"]

