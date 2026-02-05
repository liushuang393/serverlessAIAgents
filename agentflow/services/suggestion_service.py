# -*- coding: utf-8 -*-
"""Suggestion Service - フレームワーク級提案生成サービス.

ユーザーの質問やコンテキストに基づいて、次のアクション提案を生成する再利用可能なサービス。

機能:
- フォローアップ質問の提案
- 関連トピックの提案
- アクション提案

使用例:
    >>> from agentflow.services import SuggestionService
    >>> 
    >>> service = SuggestionService()
    >>> result = await service.execute(
    ...     question="売上データを見せて",
    ...     context={"query_type": "sql", "data_found": True},
    ... )
"""

from __future__ import annotations

import logging
import time
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
    ResultEvent,
)

logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


class SuggestionType(str, Enum):
    """提案タイプ."""
    FOLLOW_UP = "follow_up"  # フォローアップ質問
    RELATED = "related"  # 関連トピック
    ACTION = "action"  # アクション提案
    DRILL_DOWN = "drill_down"  # 詳細分析
    # 増強: DSL連携・データ分析提案
    INSIGHT = "insight"  # データインサイト
    ANOMALY = "anomaly"  # 異常検知アラート
    COMPARISON = "comparison"  # 比較分析提案
    TREND = "trend"  # トレンド分析提案


class SuggestionPriority(str, Enum):
    """提案優先度."""
    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


@dataclass
class SuggestionConfig:
    """提案設定."""

    max_suggestions: int = 5
    types: list[SuggestionType] = field(default_factory=lambda: [SuggestionType.FOLLOW_UP])
    use_llm: bool = True
    language: str = "ja"
    # 増強: DSL連携・パターン分析
    enable_dsl_integration: bool = True
    enable_data_analysis: bool = True
    query_pattern_weight: float = 0.3  # クエリパターンに基づく提案の重み

    @classmethod
    def get_config_fields(cls) -> list[dict[str, Any]]:
        """Studio 設定フィールド定義."""
        return [
            {
                "name": "max_suggestions",
                "type": "number",
                "label": "最大提案数",
                "default": 5,
                "min": 1,
                "max": 10,
            },
            {
                "name": "types",
                "type": "multiselect",
                "label": "提案タイプ",
                "options": [e.value for e in SuggestionType],
                "default": ["follow_up"],
            },
            {
                "name": "use_llm",
                "type": "boolean",
                "label": "LLM使用",
                "default": True,
            },
            {
                "name": "language",
                "type": "select",
                "label": "言語",
                "options": ["ja", "en", "zh"],
                "default": "ja",
            },
            {
                "name": "enable_dsl_integration",
                "type": "boolean",
                "label": "DSL連携有効",
                "default": True,
            },
            {
                "name": "enable_data_analysis",
                "type": "boolean",
                "label": "データ分析提案有効",
                "default": True,
            },
        ]


@dataclass
class Suggestion:
    """提案."""
    text: str
    type: SuggestionType
    confidence: float = 1.0
    priority: SuggestionPriority = SuggestionPriority.MEDIUM
    metadata: dict[str, Any] = field(default_factory=dict)
    # 増強: 根拠・DSL情報
    reason: str = ""  # 提案根拠
    suggested_dsl: dict[str, Any] | None = None  # 提案クエリのDSL表現


# =============================================================================
# Suggestion Service 実装
# =============================================================================


class SuggestionService(ServiceBase):
    """Suggestion Service - フレームワーク級サービス.

    DSL連携・データ分析機能を増強した提案生成サービス。

    Actions:
    - generate: 提案を生成（クエリパターン・データ結果に基づく）
    - analyze_data: データ結果を分析してインサイト提案を生成
    """

    def __init__(self, config: SuggestionConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or SuggestionConfig()
        self._llm = None
        self._started = False
        # 増強: クエリパターン辞書（BM25 との連携用）
        self._pattern_templates: dict[str, list[str]] = {
            "ranking": ["上位/下位の詳細を見る", "期間を変更して比較", "条件を追加して絞り込み"],
            "aggregation": ["グループ別に詳細表示", "時系列で推移を見る", "割合で表示"],
            "comparison": ["別の指標で比較", "期間を揃えて比較", "トレンドを確認"],
            "time_series": ["期間を拡大して表示", "前年同期と比較", "異常値を検出"],
            "filter": ["フィルタ条件を変更", "複数条件で絞り込み", "除外条件を追加"],
        }

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return

        if self._config.use_llm:
            from agentflow.providers import get_llm
            self._llm = get_llm(temperature=0.7)

        self._started = True

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        if not self._started:
            await self.start()

        action = kwargs.get("action", "generate")
        if action == "analyze_data":
            async for event in self._do_analyze_data(execution_id, **kwargs):
                yield event
        else:
            async for event in self._do_generate(execution_id, **kwargs):
                yield event

    async def _do_generate(
        self,
        execution_id: str,
        question: str = "",
        context: dict[str, Any] | None = None,
        query_pattern: str | None = None,
        dsl: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """提案を生成（DSL連携対応）."""
        start_time = time.time()
        context = context or {}

        yield self._emit_progress(execution_id, 20, "提案を生成中...", phase="generate")

        suggestions: list[Suggestion] = []

        # 増強: クエリパターンに基づく提案を優先
        if query_pattern and self._config.enable_dsl_integration:
            pattern_suggestions = self._generate_pattern_based(question, query_pattern, dsl)
            suggestions.extend(pattern_suggestions)

        for stype in self._config.types:
            if stype == SuggestionType.FOLLOW_UP:
                new_suggestions = await self._generate_follow_up(question, context)
            elif stype == SuggestionType.RELATED:
                new_suggestions = await self._generate_related(question, context)
            elif stype == SuggestionType.ACTION:
                new_suggestions = await self._generate_actions(question, context)
            elif stype == SuggestionType.DRILL_DOWN:
                new_suggestions = await self._generate_drill_down(question, context)
            elif stype == SuggestionType.INSIGHT:
                new_suggestions = self._generate_insights(context)
            elif stype == SuggestionType.COMPARISON:
                new_suggestions = self._generate_comparison_suggestions(question, context)
            elif stype == SuggestionType.TREND:
                new_suggestions = self._generate_trend_suggestions(question, context)
            else:
                new_suggestions = []

            suggestions.extend(new_suggestions)

        # 優先度とconfidenceでソート
        suggestions.sort(key=lambda s: (s.priority.value, -s.confidence))
        suggestions = suggestions[:self._config.max_suggestions]

        yield self._emit_result(execution_id, {
            "suggestions": [
                {
                    "text": s.text,
                    "type": s.type.value,
                    "confidence": s.confidence,
                    "priority": s.priority.value,
                    "reason": s.reason,
                    "suggested_dsl": s.suggested_dsl,
                }
                for s in suggestions
            ],
            "count": len(suggestions),
        }, (time.time() - start_time) * 1000)

    async def _generate_follow_up(
        self,
        question: str,
        context: dict[str, Any],
    ) -> list[Suggestion]:
        """フォローアップ質問を生成."""
        if not self._llm:
            return self._get_default_follow_up(context)
        
        query_type = context.get("query_type", "faq")
        data_found = context.get("data_found", False)
        
        prompt = f"""ユーザーの質問と結果に基づいて、次に聞くべきフォローアップ質問を3つ提案してください。

質問: {question}
クエリタイプ: {query_type}
データ有無: {"あり" if data_found else "なし"}

要件:
- 質問は具体的で実行可能なものにする
- 元の質問を深掘りする方向で
- 日本語で出力

提案（1行1つ、番号なし）:"""

        try:
            response = await self._llm.chat([{"role": "user", "content": prompt}])
            lines = response["content"].strip().split("\n")
            
            suggestions = []
            for line in lines:
                text = line.strip().lstrip("・-123456789.） ")
                if text and len(text) > 5:
                    suggestions.append(Suggestion(
                        text=text,
                        type=SuggestionType.FOLLOW_UP,
                        confidence=0.9,
                    ))
            return suggestions[:3]
        except Exception as e:
            self._logger.warning(f"LLM提案生成失敗: {e}")
            return self._get_default_follow_up(context)

    async def _generate_related(
        self,
        question: str,
        context: dict[str, Any],
    ) -> list[Suggestion]:
        """関連トピックを生成."""
        if not self._llm:
            return []
        
        prompt = f"""以下の質問に関連するトピックを2つ提案してください。

質問: {question}

要件:
- 関連性の高いトピックを選ぶ
- 日本語で出力
- 簡潔に

関連トピック（1行1つ）:"""

        try:
            response = await self._llm.chat([{"role": "user", "content": prompt}])
            lines = response["content"].strip().split("\n")
            
            suggestions = []
            for line in lines:
                text = line.strip().lstrip("・-123456789.） ")
                if text and len(text) > 3:
                    suggestions.append(Suggestion(
                        text=text,
                        type=SuggestionType.RELATED,
                        confidence=0.7,
                    ))
            return suggestions[:2]
        except Exception:
            return []

    async def _generate_actions(
        self,
        question: str,
        context: dict[str, Any],
    ) -> list[Suggestion]:
        """アクション提案を生成."""
        actions = []
        
        query_type = context.get("query_type", "faq")
        data_found = context.get("data_found", False)
        has_chart = context.get("has_chart", False)
        
        if query_type == "sql" and data_found:
            actions.append(Suggestion(
                text="データをCSVでエクスポート",
                type=SuggestionType.ACTION,
                confidence=0.9,
            ))
            if not has_chart:
                actions.append(Suggestion(
                    text="チャートで可視化",
                    type=SuggestionType.ACTION,
                    confidence=0.85,
                ))
        
        if query_type == "faq":
            actions.append(Suggestion(
                text="関連ドキュメントを表示",
                type=SuggestionType.ACTION,
                confidence=0.8,
            ))
        
        return actions

    async def _generate_drill_down(
        self,
        question: str,
        context: dict[str, Any],
    ) -> list[Suggestion]:
        """詳細分析提案を生成."""
        drill_downs = []
        
        columns = context.get("columns", [])
        if columns:
            for col in columns[:2]:
                drill_downs.append(Suggestion(
                    text=f"{col}別に詳細分析",
                    type=SuggestionType.DRILL_DOWN,
                    confidence=0.75,
                ))
        
        return drill_downs

    def _get_default_follow_up(self, context: dict[str, Any]) -> list[Suggestion]:
        """デフォルトのフォローアップ提案."""
        query_type = context.get("query_type", "faq")

        if query_type == "sql":
            return [
                Suggestion(text="先月との比較を見せて", type=SuggestionType.FOLLOW_UP),
                Suggestion(text="上位10件の詳細を教えて", type=SuggestionType.FOLLOW_UP),
            ]
        else:
            return [
                Suggestion(text="もっと詳しく教えて", type=SuggestionType.FOLLOW_UP),
                Suggestion(text="関連する情報はある？", type=SuggestionType.FOLLOW_UP),
            ]

    # =========================================================================
    # 増強: パターン・データ分析ベースの提案生成
    # =========================================================================

    def _generate_pattern_based(
        self,
        question: str,
        query_pattern: str,
        dsl: dict[str, Any] | None,
    ) -> list[Suggestion]:
        """クエリパターンに基づく提案を生成."""
        suggestions: list[Suggestion] = []

        templates = self._pattern_templates.get(query_pattern, [])
        for i, template in enumerate(templates[:2]):
            suggestions.append(Suggestion(
                text=template,
                type=SuggestionType.FOLLOW_UP,
                confidence=0.85 - i * 0.1,
                priority=SuggestionPriority.HIGH,
                reason=f"クエリパターン「{query_pattern}」に基づく推薦",
                suggested_dsl=dsl,
            ))

        return suggestions

    def _generate_insights(self, context: dict[str, Any]) -> list[Suggestion]:
        """データインサイト提案を生成."""
        suggestions: list[Suggestion] = []
        data = context.get("data", [])

        if not data:
            return suggestions

        # 数値カラムの統計情報に基づくインサイト
        if isinstance(data, list) and data:
            first_row = data[0] if data else {}
            for col, val in first_row.items():
                if isinstance(val, (int, float)):
                    values = [r.get(col, 0) for r in data if isinstance(r.get(col), (int, float))]
                    if values:
                        avg_val = sum(values) / len(values)
                        max_val = max(values)
                        if max_val > avg_val * 2:
                            suggestions.append(Suggestion(
                                text=f"{col}の最大値が平均の2倍以上です。詳細を確認しますか？",
                                type=SuggestionType.INSIGHT,
                                confidence=0.8,
                                priority=SuggestionPriority.HIGH,
                                reason="外れ値検出に基づく提案",
                            ))

        return suggestions[:2]

    def _generate_comparison_suggestions(
        self, question: str, context: dict[str, Any]
    ) -> list[Suggestion]:
        """比較分析提案を生成."""
        suggestions: list[Suggestion] = []
        dimensions = context.get("dimensions", [])

        if dimensions:
            suggestions.append(Suggestion(
                text=f"{dimensions[0]}別の比較チャートを表示",
                type=SuggestionType.COMPARISON,
                confidence=0.75,
                priority=SuggestionPriority.MEDIUM,
                reason="ディメンション分析の推薦",
            ))

        # 期間比較の提案
        suggestions.append(Suggestion(
            text="前期との比較分析を実行",
            type=SuggestionType.COMPARISON,
            confidence=0.70,
            priority=SuggestionPriority.MEDIUM,
            reason="時系列比較の推薦",
        ))

        return suggestions

    def _generate_trend_suggestions(
        self, question: str, context: dict[str, Any]
    ) -> list[Suggestion]:
        """トレンド分析提案を生成."""
        suggestions: list[Suggestion] = []

        has_time_data = context.get("has_time_series", False)
        if has_time_data:
            suggestions.append(Suggestion(
                text="過去1年間のトレンドを表示",
                type=SuggestionType.TREND,
                confidence=0.80,
                priority=SuggestionPriority.MEDIUM,
                reason="時系列データ検出に基づく提案",
            ))
            suggestions.append(Suggestion(
                text="月別の推移グラフを生成",
                type=SuggestionType.TREND,
                confidence=0.75,
                priority=SuggestionPriority.MEDIUM,
                reason="時系列集計の推薦",
            ))

        return suggestions

    async def _do_analyze_data(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """データ結果を分析してインサイト提案を生成."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "分析するデータがありません")
            return

        yield self._emit_progress(execution_id, 50, "データを分析中...", phase="analyze")

        context = {"data": data, "columns": columns or []}
        insights = self._generate_insights(context)
        comparisons = self._generate_comparison_suggestions("", context)
        trends = self._generate_trend_suggestions("", context)

        all_suggestions = insights + comparisons + trends
        all_suggestions.sort(key=lambda s: (s.priority.value, -s.confidence))

        yield self._emit_result(execution_id, {
            "insights": [
                {"text": s.text, "type": s.type.value, "confidence": s.confidence, "reason": s.reason}
                for s in all_suggestions
            ],
            "count": len(all_suggestions),
        }, (time.time() - start_time) * 1000)

    # =========================================================================
    # Studio 統合用メソッド
    # =========================================================================

    @classmethod
    def get_node_definition(cls) -> dict[str, Any]:
        """Studio ノード定義."""
        return {
            "type": "suggestion",
            "label": "提案生成",
            "category": "ai",
            "icon": "lightbulb",
            "description": "フォローアップ質問やアクションを提案",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問"},
                {"name": "context", "type": "object", "label": "コンテキスト", "optional": True},
            ],
            "outputs": [
                {"name": "suggestions", "type": "array", "label": "提案リスト"},
            ],
            "config": SuggestionConfig.get_config_fields(),
        }


__all__ = [
    "SuggestionService",
    "SuggestionConfig",
    "Suggestion",
    "SuggestionType",
    # 増強
    "SuggestionPriority",
]
