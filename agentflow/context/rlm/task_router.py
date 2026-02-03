"""Task Router - RLM発動判定.

入力を分析し、RLMモードを発動すべきかどうかを判定する。

判定基準:
1. 主要: 総Token数 > activation_threshold
2. 副次: ドキュメント分析タスクの検出
3. 副次: 複数コンテキストの存在

設計原則:
- 保守的: 不要なRLM発動を避ける
- 高速: 判定自体がオーバーヘッドにならない
- 説明可能: 判定理由を明示

使用例:
    >>> router = TaskRouter(config)
    >>> decision = router.should_activate(
    ...     query="認証要件は？",
    ...     long_inputs=["長いドキュメント..."],
    ... )
    >>> if decision.should_activate:
    ...     print(f"RLM発動: {decision.reason}")
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any

from agentflow.context.rlm.config import RLMConfig


@dataclass
class ActivationDecision:
    """RLM発動判定結果.

    Attributes:
        should_activate: RLMを発動すべきか
        reason: 判定理由
        total_tokens: 推定総Token数
        context_count: コンテキスト数
        confidence: 判定の確信度（0-1）
        metadata: 追加メタデータ
    """

    should_activate: bool
    reason: str
    total_tokens: int = 0
    context_count: int = 0
    confidence: float = 1.0
    metadata: dict[str, Any] = field(default_factory=dict)


class TaskRouter:
    """RLM発動判定ルーター.

    入力を分析し、RLMモードを発動すべきかを判定。

    使用例:
        >>> config = RLMConfig(activation_threshold=15000)
        >>> router = TaskRouter(config)
        >>>
        >>> decision = router.should_activate(
        ...     query="APIの認証要件を教えて",
        ...     long_inputs=[large_document],
        ... )
        >>>
        >>> if decision.should_activate:
        ...     # RLMコントローラーを起動
        ...     controller = RLMController(config)
        ...     result = await controller.run(query, long_inputs)
    """

    # ドキュメント分析を示唆するキーワード
    DOC_ANALYSIS_KEYWORDS = [
        "要約",
        "summarize",
        "summary",
        "抽出",
        "extract",
        "分析",
        "analyze",
        "analysis",
        "比較",
        "compare",
        "comparison",
        "レビュー",
        "review",
        "全体",
        "全文",
        "entire",
        "whole",
        "ドキュメント",
        "document",
        "文書",
        "仕様",
        "specification",
        "spec",
    ]

    # 単純なQ&Aを示唆するキーワード
    SIMPLE_QA_KEYWORDS = [
        "とは",
        "what is",
        "define",
        "教えて",
        "tell me",
        "どこ",
        "where",
        "いつ",
        "when",
    ]

    def __init__(self, config: RLMConfig | None = None) -> None:
        """初期化.

        Args:
            config: RLM設定
        """
        self._config = config or RLMConfig()

    def _estimate_tokens(self, text: str) -> int:
        """Token数を推定.

        Args:
            text: 対象テキスト

        Returns:
            推定Token数
        """
        if not text:
            return 0

        # CJK文字をカウント
        cjk_chars = len(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))
        ascii_chars = len(re.findall(r"[a-zA-Z0-9\s]", text))
        other_chars = len(text) - cjk_chars - ascii_chars

        return int(cjk_chars * 1.5 + ascii_chars * 0.25 + other_chars)

    def _is_document_analysis_task(self, query: str) -> tuple[bool, float]:
        """ドキュメント分析タスクかを判定.

        Args:
            query: クエリ

        Returns:
            (判定結果, 確信度)
        """
        query_lower = query.lower()

        # 分析キーワードの検出
        analysis_matches = sum(1 for kw in self.DOC_ANALYSIS_KEYWORDS if kw.lower() in query_lower)

        # 単純Q&Aキーワードの検出
        simple_matches = sum(1 for kw in self.SIMPLE_QA_KEYWORDS if kw.lower() in query_lower)

        # スコア計算
        if analysis_matches > 0 and simple_matches == 0:
            confidence = min(0.5 + analysis_matches * 0.2, 0.9)
            return True, confidence
        if analysis_matches > simple_matches:
            confidence = 0.4 + (analysis_matches - simple_matches) * 0.1
            return True, confidence

        return False, 0.0

    def _is_multi_context_query(
        self,
        query: str,
        context_count: int,
    ) -> tuple[bool, float]:
        """複数コンテキストを跨ぐクエリかを判定.

        Args:
            query: クエリ
            context_count: コンテキスト数

        Returns:
            (判定結果, 確信度)
        """
        if context_count <= 1:
            return False, 0.0

        # 複数参照を示唆するキーワード
        multi_ref_keywords = [
            "全て",
            "全部",
            "all",
            "それぞれ",
            "each",
            "比較",
            "compare",
            "統合",
            "merge",
            "combine",
            "横断",
            "across",
        ]

        query_lower = query.lower()
        matches = sum(1 for kw in multi_ref_keywords if kw.lower() in query_lower)

        if matches > 0:
            confidence = min(0.6 + matches * 0.15, 0.9)
            return True, confidence

        # コンテキストが多い場合は弱い判定
        if context_count >= 3:
            return True, 0.4

        return False, 0.0

    def should_activate(
        self,
        query: str,
        long_inputs: list[str] | None = None,
        existing_context_tokens: int = 0,
    ) -> ActivationDecision:
        """RLM発動を判定.

        Args:
            query: ユーザークエリ
            long_inputs: 長いコンテキストリスト
            existing_context_tokens: 既存コンテキストのToken数

        Returns:
            ActivationDecision
        """
        long_inputs = long_inputs or []

        # Token数を計算
        query_tokens = self._estimate_tokens(query)
        input_tokens = sum(self._estimate_tokens(inp) for inp in long_inputs)
        total_tokens = query_tokens + input_tokens + existing_context_tokens

        context_count = len(long_inputs)

        # 判定理由と確信度を収集
        reasons: list[tuple[str, float]] = []

        # 1. 主要判定: Token閾値
        if total_tokens > self._config.activation_threshold:
            excess_ratio = total_tokens / self._config.activation_threshold
            confidence = min(0.5 + excess_ratio * 0.2, 1.0)
            reasons.append(
                (
                    f"Total tokens ({total_tokens:,}) exceeds threshold "
                    f"({self._config.activation_threshold:,})",
                    confidence,
                )
            )

        # 2. 副次判定: ドキュメント分析タスク
        is_doc_analysis, doc_confidence = self._is_document_analysis_task(query)
        if is_doc_analysis:
            reasons.append(
                (
                    "Document analysis task detected",
                    doc_confidence,
                )
            )

        # 3. 副次判定: 複数コンテキスト
        is_multi, multi_confidence = self._is_multi_context_query(query, context_count)
        if is_multi:
            reasons.append(
                (
                    f"Multi-context query ({context_count} contexts)",
                    multi_confidence,
                )
            )

        # 判定結果を決定
        if not reasons:
            return ActivationDecision(
                should_activate=False,
                reason="Below activation threshold, simple query",
                total_tokens=total_tokens,
                context_count=context_count,
                confidence=0.9,
            )

        # 最も確信度の高い理由を選択
        primary_reason = max(reasons, key=lambda x: x[1])
        all_reasons = "; ".join(r[0] for r in reasons)

        # Token閾値を超えていなくても、他の条件で発動する場合
        # ただし、入力が十分小さい場合は発動しない
        min_activation_tokens = self._config.activation_threshold // 3
        if total_tokens < min_activation_tokens:
            return ActivationDecision(
                should_activate=False,
                reason=f"Total tokens ({total_tokens:,}) too small for RLM benefit",
                total_tokens=total_tokens,
                context_count=context_count,
                confidence=0.8,
            )

        return ActivationDecision(
            should_activate=True,
            reason=all_reasons,
            total_tokens=total_tokens,
            context_count=context_count,
            confidence=primary_reason[1],
            metadata={
                "reasons": reasons,
                "threshold": self._config.activation_threshold,
            },
        )

    def estimate_complexity(
        self,
        query: str,
        long_inputs: list[str] | None = None,
    ) -> dict[str, Any]:
        """タスク複雑度を推定.

        Args:
            query: クエリ
            long_inputs: 長いコンテキストリスト

        Returns:
            複雑度推定辞書
        """
        long_inputs = long_inputs or []

        total_tokens = self._estimate_tokens(query) + sum(
            self._estimate_tokens(inp) for inp in long_inputs
        )

        # 複雑度レベルを判定
        threshold = self._config.activation_threshold
        if total_tokens < threshold // 2:
            complexity = "low"
            estimated_iterations = 1
        elif total_tokens < threshold:
            complexity = "medium"
            estimated_iterations = 3
        elif total_tokens < threshold * 2:
            complexity = "high"
            estimated_iterations = 8
        else:
            complexity = "very_high"
            estimated_iterations = 15

        # ドキュメント分析タスクは複雑度を上げる
        is_doc_analysis, _ = self._is_document_analysis_task(query)
        if is_doc_analysis:
            estimated_iterations = int(estimated_iterations * 1.5)

        return {
            "complexity": complexity,
            "total_tokens": total_tokens,
            "context_count": len(long_inputs),
            "estimated_iterations": min(estimated_iterations, self._config.max_iterations),
            "is_document_analysis": is_doc_analysis,
            "activation_threshold": threshold,
            "would_activate": total_tokens > threshold,
        }
