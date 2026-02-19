"""Reflexion Pattern - 失敗学習と自己反省.

このモジュールは、Reflexion 論文に基づいた失敗学習パターンを提供します。

接口設計原則:
- Reflexion 論文の思想を忠実に実装
- 将来の他フレームワーク統合を考慮
- DeepAgentCoordinator との統合が容易

参考論文:
- Reflexion: Language Agents with Verbal Reinforcement Learning (NeurIPS 2023)
- Self-Refine: Iterative Refinement with Self-Feedback (NeurIPS 2023)
- CRITIC: Large Language Models Can Self-Correct with Tool-Interactive Critiquing

使用例:
    >>> from agentflow.patterns.reflexion import ReflectiveEvolver
    >>>
    >>> evolver = ReflectiveEvolver(llm_client=my_llm)
    >>>
    >>> # 失敗から学習
    >>> record = await evolver.learn_from_failure(
    ...     task="データベース接続",
    ...     error=ConnectionError("timeout"),
    ...     context={"retry_count": 3},
    ... )
    >>>
    >>> # 学習した反省を取得
    >>> reflections = evolver.get_reflections(task="データベース")
"""

from __future__ import annotations

import logging
import uuid
from abc import ABC, abstractmethod
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


# =============================================================================
# データモデル
# =============================================================================


class ReflectionType(str, Enum):
    """反省タイプ."""

    FAILURE_ANALYSIS = "failure_analysis"  # 失敗原因分析
    STRATEGY_REFLECTION = "strategy_reflection"  # 戦略反省
    TOOL_REFLECTION = "tool_reflection"  # ツール使用反省
    KNOWLEDGE_GAP = "knowledge_gap"  # 知識ギャップ
    PROCESS_IMPROVEMENT = "process_improvement"  # プロセス改善


class Severity(str, Enum):
    """深刻度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class Reflection(BaseModel):
    """反省記録（Reflexion論文のverbal reflectionに相当）.

    失敗から生成された言語的反省。
    将来の同様の失敗を回避するために使用。
    """

    id: str = Field(default_factory=lambda: f"ref_{uuid.uuid4().hex[:12]}")
    type: ReflectionType = Field(default=ReflectionType.FAILURE_ANALYSIS)
    timestamp: datetime = Field(default_factory=datetime.now)

    # 失敗コンテキスト
    task_pattern: str = Field(..., description="タスクパターン（正規化）")
    error_type: str = Field(..., description="エラータイプ")
    error_message: str = Field(default="", description="エラーメッセージ")
    severity: Severity = Field(default=Severity.MEDIUM)

    # 反省内容
    what_went_wrong: str = Field(..., description="何が問題だったか")
    why_it_failed: str = Field(default="", description="なぜ失敗したか")
    how_to_avoid: str = Field(default="", description="どう回避するか")
    alternative_approach: str = Field(default="", description="代替アプローチ")

    # メタデータ
    confidence: float = Field(default=0.5, ge=0.0, le=1.0)
    applied_count: int = Field(default=0, description="適用回数")
    success_after_apply: int = Field(default=0, description="適用後成功回数")
    context: dict[str, Any] = Field(default_factory=dict)


class FailurePattern(BaseModel):
    """失敗パターン.

    同じ失敗パターンを追跡し、学習効果を測定。
    """

    id: str = Field(default_factory=lambda: f"fp_{uuid.uuid4().hex[:12]}")
    pattern_key: str = Field(..., description="パターンキー")
    error_type: str = Field(..., description="エラータイプ")
    occurrence_count: int = Field(default=1)
    last_occurred: datetime = Field(default_factory=datetime.now)
    reflections: list[str] = Field(default_factory=list, description="関連反省ID")
    resolved: bool = Field(default=False)


class LearningOutcome(BaseModel):
    """学習結果."""

    reflection_id: str = Field(...)
    task: str = Field(...)
    applied: bool = Field(default=False)
    success: bool = Field(default=False)
    feedback: str = Field(default="")
    timestamp: datetime = Field(default_factory=datetime.now)


# =============================================================================
# 反省生成器（抽象）
# =============================================================================


class ReflectionGenerator(ABC):
    """反省生成器抽象接口.

    LLMベースまたはルールベースの反省生成を抽象化。
    """

    @abstractmethod
    async def generate_reflection(
        self,
        task: str,
        error: Exception,
        context: dict[str, Any],
    ) -> Reflection:
        """失敗から反省を生成.

        Args:
            task: 失敗したタスク
            error: 発生したエラー
            context: 実行コンテキスト

        Returns:
            生成された反省
        """

    @abstractmethod
    async def refine_reflection(
        self,
        reflection: Reflection,
        feedback: str,
    ) -> Reflection:
        """反省を改善.

        Args:
            reflection: 元の反省
            feedback: フィードバック

        Returns:
            改善された反省
        """


class LLMReflectionGenerator(ReflectionGenerator):
    """LLMベースの反省生成器.

    LLMを使用して自然言語の反省を生成。
    Reflexion論文の verbal reflection に相当。
    """

    def __init__(
        self,
        llm_client: Any = None,
        system_prompt: str | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            system_prompt: システムプロンプト
        """
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)
        self._system_prompt = system_prompt or self._default_system_prompt()

    def _default_system_prompt(self) -> str:
        """デフォルトシステムプロンプト."""
        return """あなたは失敗分析の専門家です。
タスクの失敗を分析し、将来の改善に役立つ反省を生成してください。

反省は以下を含む必要があります：
1. 何が問題だったか（具体的に）
2. なぜ失敗したか（根本原因）
3. どう回避できるか（具体的な戦略）
4. 代替アプローチ（もしあれば）

簡潔かつ実用的な反省を生成してください。"""

    async def generate_reflection(
        self,
        task: str,
        error: Exception,
        context: dict[str, Any],
    ) -> Reflection:
        """LLMで反省を生成."""
        task_pattern = self._extract_pattern(task)
        error_type = type(error).__name__
        error_message = str(error)

        # LLMがない場合はルールベース
        if self._llm is None:
            return self._rule_based_reflection(task_pattern, error_type, error_message, context)

        # LLMプロンプト
        prompt = f"""タスク失敗を分析してください：

タスク: {task}
エラータイプ: {error_type}
エラーメッセージ: {error_message}
コンテキスト: {context}

以下の形式でJSON回答してください：
{{
    "what_went_wrong": "何が問題だったか",
    "why_it_failed": "なぜ失敗したか",
    "how_to_avoid": "どう回避するか",
    "alternative_approach": "代替アプローチ",
    "severity": "low/medium/high/critical",
    "reflection_type": "failure_analysis/strategy_reflection/tool_reflection/knowledge_gap"
}}"""

        try:
            response = await self._llm.chat(
                [
                    {"role": "system", "content": self._system_prompt},
                    {"role": "user", "content": prompt},
                ],
                response_format={"type": "json_object"},
            )

            import json

            content = response.get("content", "") if isinstance(response, dict) else str(response)
            if hasattr(response, "content"):
                content = response.content
            data = json.loads(content)

            return Reflection(
                type=ReflectionType(data.get("reflection_type", "failure_analysis")),
                task_pattern=task_pattern,
                error_type=error_type,
                error_message=error_message,
                severity=Severity(data.get("severity", "medium")),
                what_went_wrong=data.get("what_went_wrong", "Unknown issue"),
                why_it_failed=data.get("why_it_failed", ""),
                how_to_avoid=data.get("how_to_avoid", ""),
                alternative_approach=data.get("alternative_approach", ""),
                confidence=0.8,
                context=context,
            )

        except Exception as e:
            self._logger.warning(f"LLM reflection generation failed: {e}")
            return self._rule_based_reflection(task_pattern, error_type, error_message, context)

    async def refine_reflection(
        self,
        reflection: Reflection,
        feedback: str,
    ) -> Reflection:
        """反省を改善."""
        if self._llm is None:
            # フィードバックを追加するだけ
            reflection.context["feedback"] = feedback
            return reflection

        prompt = f"""以下の反省をフィードバックに基づいて改善してください：

元の反省:
- 何が問題だったか: {reflection.what_went_wrong}
- なぜ失敗したか: {reflection.why_it_failed}
- どう回避するか: {reflection.how_to_avoid}

フィードバック: {feedback}

改善された反省をJSON形式で返してください。"""

        try:
            response = await self._llm.chat(
                [
                    {"role": "system", "content": "あなたは反省を改善する専門家です。"},
                    {"role": "user", "content": prompt},
                ],
                response_format={"type": "json_object"},
            )

            import json

            content = response.get("content", "") if isinstance(response, dict) else str(response)
            if hasattr(response, "content"):
                content = response.content
            data = json.loads(content)

            reflection.what_went_wrong = data.get("what_went_wrong", reflection.what_went_wrong)
            reflection.why_it_failed = data.get("why_it_failed", reflection.why_it_failed)
            reflection.how_to_avoid = data.get("how_to_avoid", reflection.how_to_avoid)
            reflection.confidence = min(1.0, reflection.confidence + 0.1)

            return reflection

        except Exception as e:
            self._logger.warning(f"Reflection refinement failed: {e}")
            return reflection

    def _extract_pattern(self, task: str) -> str:
        """タスクからパターンを抽出."""
        normalized = task[:50].strip().lower()
        return "".join(c if c.isalnum() else "_" for c in normalized)

    def _rule_based_reflection(
        self,
        task_pattern: str,
        error_type: str,
        error_message: str,
        context: dict[str, Any],
    ) -> Reflection:
        """ルールベースの反省生成."""
        # エラータイプに基づく反省テンプレート
        templates = {
            "TimeoutError": {
                "what_went_wrong": "操作がタイムアウトしました",
                "why_it_failed": "処理時間が想定を超えた、またはリソースが応答しなかった",
                "how_to_avoid": "タイムアウト値を増やす、または操作を分割する",
                "severity": Severity.MEDIUM,
            },
            "ConnectionError": {
                "what_went_wrong": "接続に失敗しました",
                "why_it_failed": "ネットワーク問題、またはサービスがダウンしている",
                "how_to_avoid": "リトライ機構を追加し、接続前にヘルスチェックを行う",
                "severity": Severity.HIGH,
            },
            "ValueError": {
                "what_went_wrong": "無効な値が渡されました",
                "why_it_failed": "入力検証が不十分、または想定外のデータ形式",
                "how_to_avoid": "入力検証を強化し、型チェックを追加する",
                "severity": Severity.LOW,
            },
            "KeyError": {
                "what_went_wrong": "必要なキーが存在しませんでした",
                "why_it_failed": "データ構造の想定が間違っている",
                "how_to_avoid": ".get() を使用し、デフォルト値を設定する",
                "severity": Severity.LOW,
            },
        }

        template = templates.get(
            error_type,
            {
                "what_went_wrong": f"エラーが発生: {error_type}",
                "why_it_failed": error_message[:100],
                "how_to_avoid": "エラーハンドリングを追加し、原因を調査する",
                "severity": Severity.MEDIUM,
            },
        )

        return Reflection(
            type=ReflectionType.FAILURE_ANALYSIS,
            task_pattern=task_pattern,
            error_type=error_type,
            error_message=error_message,
            severity=template.get("severity", Severity.MEDIUM),
            what_went_wrong=template["what_went_wrong"],
            why_it_failed=template["why_it_failed"],
            how_to_avoid=template["how_to_avoid"],
            confidence=0.6,
            context=context,
        )


# =============================================================================
# ReflectiveEvolver - 失敗学習統合
# =============================================================================


class ReflectiveEvolver:
    """反省型進化器（Reflexion + 既存Evolver統合）.

    失敗から学習し、将来の実行を改善する。
    DeepAgentCoordinator と統合可能。

    Reflexion パターン:
    1. Actor: タスクを実行
    2. Evaluator: 結果を評価
    3. Self-Reflection: 失敗を反省
    4. Memory: 反省を保存
    5. Retry: 反省を活用して再試行

    使用例:
        >>> evolver = ReflectiveEvolver(llm_client=my_llm)
        >>>
        >>> # 失敗から学習
        >>> await evolver.learn_from_failure(task, error, context)
        >>>
        >>> # 反省を取得してプロンプトに含める
        >>> reflections = evolver.get_relevant_reflections(task)
        >>> enhanced_prompt = f"{task}\\n\\n過去の反省:\\n{reflections}"
    """

    def __init__(
        self,
        llm_client: Any = None,
        reflection_generator: ReflectionGenerator | None = None,
        max_reflections: int = 100,
        enable_auto_apply: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            reflection_generator: 反省生成器
            max_reflections: 最大反省保持数
            enable_auto_apply: 自動適用を有効化
        """
        self._llm = llm_client
        self._generator = reflection_generator or LLMReflectionGenerator(llm_client)
        self._max_reflections = max_reflections
        self._enable_auto_apply = enable_auto_apply
        self._logger = logging.getLogger(__name__)

        # ストレージ
        self._reflections: dict[str, Reflection] = {}
        self._failure_patterns: dict[str, FailurePattern] = {}
        self._learning_outcomes: list[LearningOutcome] = []

        # 成功パターン（既存Evolverとの統合用）
        self._success_patterns: dict[str, dict[str, Any]] = {}
        self._pattern_scores: dict[str, float] = {}

    async def learn_from_failure(
        self,
        task: str,
        error: Exception,
        context: dict[str, Any] | None = None,
        retry_count: int = 0,
    ) -> Reflection:
        """失敗から学習.

        Args:
            task: 失敗したタスク
            error: 発生したエラー
            context: 実行コンテキスト
            retry_count: リトライ回数

        Returns:
            生成された反省
        """
        context = context or {}
        context["retry_count"] = retry_count

        # 反省を生成
        reflection = await self._generator.generate_reflection(task, error, context)

        # 保存
        self._reflections[reflection.id] = reflection

        # 失敗パターンを更新
        pattern_key = reflection.task_pattern
        error_type = reflection.error_type

        fp_key = f"{pattern_key}:{error_type}"
        if fp_key in self._failure_patterns:
            self._failure_patterns[fp_key].occurrence_count += 1
            self._failure_patterns[fp_key].last_occurred = datetime.now()
            self._failure_patterns[fp_key].reflections.append(reflection.id)
        else:
            self._failure_patterns[fp_key] = FailurePattern(
                pattern_key=pattern_key,
                error_type=error_type,
                reflections=[reflection.id],
            )

        # パターンスコアを下げる
        if pattern_key in self._pattern_scores:
            self._pattern_scores[pattern_key] = max(
                0.0,
                self._pattern_scores[pattern_key] - 0.15,
            )

        self._logger.info(
            f"Learned from failure: {pattern_key} ({error_type}) "
            f"-> {reflection.how_to_avoid[:50]}..."
        )

        # 最大数を超えたら古いものを削除
        self._cleanup_old_reflections()

        return reflection

    async def learn_from_success(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> None:
        """成功から学習（既存Evolverとの互換性）.

        Args:
            task: 成功したタスク
            result: 実行結果
            context: 実行コンテキスト
        """
        pattern_key = self._extract_pattern(task)

        # パターンスコアを上げる
        current_score = self._pattern_scores.get(pattern_key, 0.5)
        self._pattern_scores[pattern_key] = min(1.0, current_score + 0.1)

        # 成功パターンを保存
        self._success_patterns[pattern_key] = {
            "task": task[:200],
            "approach": result.get("approach", ""),
            "timestamp": datetime.now().isoformat(),
        }

        # 関連する失敗パターンを解決済みにマーク
        for fp_key, fp in self._failure_patterns.items():
            if fp.pattern_key == pattern_key and not fp.resolved:
                fp.resolved = True
                self._logger.info(f"Failure pattern resolved: {fp_key}")

                # 適用した反省の成功カウントを増やす
                for ref_id in fp.reflections:
                    if ref_id in self._reflections:
                        self._reflections[ref_id].success_after_apply += 1

    def get_relevant_reflections(
        self,
        task: str,
        max_count: int = 3,
        min_confidence: float = 0.5,
    ) -> list[Reflection]:
        """関連する反省を取得.

        Args:
            task: タスク
            max_count: 最大取得数
            min_confidence: 最小信頼度

        Returns:
            関連する反省リスト
        """
        pattern_key = self._extract_pattern(task)

        # パターンマッチング
        relevant = []
        for ref in self._reflections.values():
            if ref.confidence < min_confidence:
                continue

            # パターン類似度
            similarity = self._pattern_similarity(pattern_key, ref.task_pattern)
            if similarity > 0.3:
                relevant.append((similarity, ref))

        # 類似度順にソート
        relevant.sort(key=lambda x: (-x[0], -x[1].confidence))

        return [ref for _, ref in relevant[:max_count]]

    def get_reflection_prompt(
        self,
        task: str,
        max_reflections: int = 3,
    ) -> str:
        """反省を含むプロンプト補足を生成.

        タスクに関連する過去の反省をプロンプトに含める形式で返す。

        Args:
            task: タスク
            max_reflections: 最大反省数

        Returns:
            プロンプト補足文字列
        """
        reflections = self.get_relevant_reflections(task, max_reflections)
        if not reflections:
            return ""

        lines = ["## 過去の失敗からの学び"]
        for i, ref in enumerate(reflections, 1):
            lines.append(f"\n### 学び {i}")
            lines.append(f"- 問題: {ref.what_went_wrong}")
            lines.append(f"- 回避策: {ref.how_to_avoid}")
            if ref.alternative_approach:
                lines.append(f"- 代替: {ref.alternative_approach}")

        return "\n".join(lines)

    def get_failure_patterns(
        self,
        resolved: bool | None = None,
        min_occurrences: int = 1,
    ) -> list[FailurePattern]:
        """失敗パターンを取得.

        Args:
            resolved: 解決済みフィルター
            min_occurrences: 最小発生回数

        Returns:
            失敗パターンリスト
        """
        patterns = []
        for fp in self._failure_patterns.values():
            if resolved is not None and fp.resolved != resolved:
                continue
            if fp.occurrence_count < min_occurrences:
                continue
            patterns.append(fp)

        return sorted(patterns, key=lambda x: -x.occurrence_count)

    def record_outcome(
        self,
        reflection_id: str,
        task: str,
        success: bool,
        feedback: str = "",
    ) -> None:
        """学習結果を記録.

        Args:
            reflection_id: 反省ID
            task: タスク
            success: 成功したか
            feedback: フィードバック
        """
        outcome = LearningOutcome(
            reflection_id=reflection_id,
            task=task,
            applied=True,
            success=success,
            feedback=feedback,
        )
        self._learning_outcomes.append(outcome)

        # 反省の統計を更新
        if reflection_id in self._reflections:
            ref = self._reflections[reflection_id]
            ref.applied_count += 1
            if success:
                ref.success_after_apply += 1
                # 成功したら信頼度を上げる
                ref.confidence = min(1.0, ref.confidence + 0.1)
            else:
                # 失敗したら信頼度を下げる
                ref.confidence = max(0.0, ref.confidence - 0.1)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        total_reflections = len(self._reflections)
        avg_confidence = (
            sum(r.confidence for r in self._reflections.values()) / total_reflections
            if total_reflections > 0
            else 0.0
        )

        return {
            "total_reflections": total_reflections,
            "avg_confidence": avg_confidence,
            "failure_patterns": len(self._failure_patterns),
            "resolved_patterns": sum(1 for fp in self._failure_patterns.values() if fp.resolved),
            "success_patterns": len(self._success_patterns),
            "learning_outcomes": len(self._learning_outcomes),
            "high_confidence_reflections": sum(
                1 for r in self._reflections.values() if r.confidence >= 0.8
            ),
        }

    def _extract_pattern(self, task: str) -> str:
        """タスクからパターンを抽出."""
        normalized = task[:50].strip().lower()
        return "".join(c if c.isalnum() else "_" for c in normalized)

    def _pattern_similarity(self, pattern1: str, pattern2: str) -> float:
        """パターン類似度を計算."""
        words1 = set(pattern1.split("_"))
        words2 = set(pattern2.split("_"))

        if not words1 or not words2:
            return 0.0

        intersection = len(words1 & words2)
        union = len(words1 | words2)

        return intersection / union if union > 0 else 0.0

    def _cleanup_old_reflections(self) -> None:
        """古い反省を削除."""
        if len(self._reflections) <= self._max_reflections:
            return

        # 信頼度と最終更新でソート
        sorted_refs = sorted(
            self._reflections.items(),
            key=lambda x: (x[1].confidence, x[1].timestamp),
        )

        # 古いものを削除
        to_remove = len(self._reflections) - self._max_reflections
        for ref_id, _ in sorted_refs[:to_remove]:
            del self._reflections[ref_id]

        self._logger.debug(f"Cleaned up {to_remove} old reflections")


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "FailurePattern",
    "LLMReflectionGenerator",
    "LearningOutcome",
    "Reflection",
    # 生成器
    "ReflectionGenerator",
    # データモデル
    "ReflectionType",
    # メイン
    "ReflectiveEvolver",
    "Severity",
]
