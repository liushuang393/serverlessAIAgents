# -*- coding: utf-8 -*-
"""Sales Agent - 売上分析専門Agent.

売上データ分析、トレンド可視化、予測に特化したAgent。

機能:
- 売上データのSQL分析
- トレンド・シーズナリティ分析
- 前年比・目標比較
- 予測と提案

使用例:
    >>> from agentflow.agents import SalesAgent
    >>> 
    >>> agent = SalesAgent(config=SalesAgentConfig(
    ...     sql_schema={"sales": ["id", "product", "amount", "date", "region"]},
    ... ))
    >>> 
    >>> result = await agent.run({"question": "今月の売上トレンドを教えて"})
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any, AsyncIterator

from pydantic import BaseModel, Field

# 循環インポート回避: agentflow.core から直接インポート
from agentflow.core import ResilientAgent

logger = logging.getLogger(__name__)


# =============================================================================
# 設定
# =============================================================================


@dataclass
class SalesAgentConfig:
    """SalesAgent 設定.
    
    Attributes:
        sql_schema: DBスキーマ
        sql_dialect: SQLダイアレクト
        default_period: デフォルト分析期間（日数）
        compare_yoy: 前年同期比較を含める
        auto_forecast: 予測を自動生成
    """
    
    sql_schema: dict[str, list[str]] = field(default_factory=dict)
    sql_dialect: str = "postgresql"
    default_period: int = 30
    compare_yoy: bool = True
    auto_forecast: bool = False


# =============================================================================
# Pydantic スキーマ（型安全な入出力）
# =============================================================================


class SalesInput(BaseModel):
    """SalesAgent 入力スキーマ."""

    question: str = Field(..., description="質問文")
    period: int | None = Field(None, description="分析期間（日数）")


class SalesOutput(BaseModel):
    """SalesAgent 出力スキーマ."""

    question: str = Field(..., description="元の質問")
    query_type: str = Field("sales_analysis", description="クエリタイプ")
    answer: str = Field("", description="分析結果の説明")
    sql: str = Field("", description="生成SQL")
    data: list[dict[str, Any]] = Field(default_factory=list, description="クエリ結果")
    columns: list[str] = Field(default_factory=list, description="カラム名")
    chart: dict[str, Any] | None = Field(None, description="チャートデータ")
    insights: list[str] = Field(default_factory=list, description="分析インサイト")
    suggestions: list[dict[str, Any]] = Field(default_factory=list, description="提案")
    error: str | None = Field(None, description="エラーメッセージ")


# =============================================================================
# Sales Agent
# =============================================================================


class SalesAgent(ResilientAgent[SalesInput, SalesOutput]):
    """売上分析専門Agent（ResilientAgent 準拠）.

    入力:
        - question: 質問文（必須）
        - period: 分析期間（オプション）

    出力:
        - answer: 分析結果の説明
        - sql: 生成SQL
        - data: クエリ結果
        - chart: チャートデータ
        - insights: 分析インサイト
        - suggestions: 次のアクション提案
    """

    name = "SalesAgent"
    temperature = 0.3

    def __init__(
        self,
        config: SalesAgentConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._config = config or SalesAgentConfig()
        self._services_initialized = False

        # サービスインスタンス（遅延初期化・私有化）
        self.__sql_service = None
        self.__suggestion_service = None

    def _parse_input(self, input_data: dict[str, Any]) -> SalesInput:
        """入力データをパース."""
        return SalesInput(**input_data)

    async def __ensure_services(self) -> None:
        """サービスの遅延初期化（私有）."""
        if self._services_initialized:
            return

        from agentflow.services import (
            Text2SQLService, Text2SQLConfig,
            SuggestionService, SuggestionConfig,
        )

        self.__sql_service = Text2SQLService(Text2SQLConfig(
            schema=self._config.sql_schema,
            dialect=self._config.sql_dialect,
            auto_chart=True,
        ))

        self.__suggestion_service = SuggestionService(SuggestionConfig(
            max_suggestions=5,
        ))

        self._services_initialized = True

    async def process(self, input_data: SalesInput) -> SalesOutput:
        """売上分析を実行（ResilientAgent 準拠）.

        Args:
            input_data: 入力データ（SalesInput）

        Returns:
            SalesOutput: 分析結果
        """
        await self.__ensure_services()

        question = input_data.question
        if not question:
            return SalesOutput(
                question="",
                error="質問が指定されていません",
            )

        # 売上分析用のコンテキストを追加
        enhanced_question = self.__enhance_question(question, input_data.period)

        try:
            # SQLクエリ実行
            sql_result = await self.__sql_service.execute(
                action="query",
                question=enhanced_question,
            )

            answer = sql_result.data.get("answer", "")
            sql = sql_result.data.get("sql", "")
            data = sql_result.data.get("data", [])
            columns = sql_result.data.get("columns", [])
            chart = sql_result.data.get("chart")

            # インサイトを生成
            insights = []
            if data:
                insights = await self.__generate_insights(question, data, columns)

            # 提案を生成
            suggestion_result = await self.__suggestion_service.execute(
                question=question,
                context={
                    "query_type": "sales",
                    "data_found": bool(data),
                    "columns": columns,
                },
            )
            suggestions = suggestion_result.data.get("suggestions", [])

            return SalesOutput(
                question=question,
                answer=answer,
                sql=sql,
                data=data,
                columns=columns,
                chart=chart,
                insights=insights,
                suggestions=suggestions,
            )

        except Exception as e:
            logger.error(f"SalesAgent実行エラー: {e}")
            return SalesOutput(
                question=question,
                error=str(e),
                answer=f"申し訳ありません。エラーが発生しました: {e}",
            )

    def __enhance_question(self, question: str, period: int | None = None) -> str:
        """質問を売上分析用に強化（私有）."""
        period_keywords = ["日", "週", "月", "年", "期間", "から", "まで"]
        has_period = any(k in question for k in period_keywords)

        actual_period = period or self._config.default_period
        if not has_period:
            return f"{question}（過去{actual_period}日間）"
        return question

    async def __generate_insights(
        self,
        question: str,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> list[str]:
        """インサイトを生成（私有）."""
        insights = []

        if not data:
            return insights

        # 基本統計を計算
        numeric_cols = []
        for col in columns:
            try:
                values = [float(row.get(col, 0)) for row in data if row.get(col) is not None]
                if values:
                    numeric_cols.append({
                        "name": col,
                        "sum": sum(values),
                        "avg": sum(values) / len(values),
                        "max": max(values),
                        "min": min(values),
                    })
            except (ValueError, TypeError):
                continue

        # インサイト生成
        for col_stats in numeric_cols:
            if "amount" in col_stats["name"].lower() or "売上" in col_stats["name"]:
                insights.append(f"合計{col_stats['name']}: ¥{col_stats['sum']:,.0f}")
                insights.append(f"平均{col_stats['name']}: ¥{col_stats['avg']:,.0f}")

        # 傾向分析
        if len(data) >= 2:
            first_half = data[:len(data)//2]
            second_half = data[len(data)//2:]

            for col_stats in numeric_cols:
                col = col_stats["name"]
                first_sum = sum(float(r.get(col, 0)) for r in first_half if r.get(col))
                second_sum = sum(float(r.get(col, 0)) for r in second_half if r.get(col))

                if first_sum > 0:
                    change = (second_sum - first_sum) / first_sum * 100
                    trend = "上昇" if change > 0 else "下降"
                    insights.append(f"{col}の傾向: {change:+.1f}%（{trend}傾向）")

        return insights[:5]

    async def run_stream(
        self,
        inputs: dict[str, Any],
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        await self.__ensure_services()

        question = inputs.get("question", "")
        if not question:
            yield {"type": "error", "message": "質問が指定されていません"}
            return

        enhanced_question = self.__enhance_question(question)

        yield {"type": "status", "message": "売上データを分析中..."}

        try:
            async for event in self.__sql_service.execute_stream(
                action="query",
                question=enhanced_question,
            ):
                yield event.to_dict()

            # 提案生成
            async for event in self.__suggestion_service.execute_stream(
                question=question,
                context={"query_type": "sales"},
            ):
                if event.type.value == "result":
                    yield {"type": "suggestions", "data": event.data.get("suggestions", [])}

        except Exception as e:
            yield {"type": "error", "message": str(e)}

    @property
    def agent_type(self) -> str:
        """Agentタイプ."""
        return "sales"

    @property
    def skills(self) -> list[str]:
        """利用可能なスキル."""
        return ["text2sql", "chart", "analysis"]

    @classmethod
    def get_definition(cls) -> dict[str, Any]:
        """Agent定義（Studio用）."""
        return {
            "type": "sales",
            "name": "SalesAgent",
            "label": "売上分析Agent",
            "category": "specialized",
            "icon": "chart-line",
            "description": "売上データ分析、トレンド可視化、予測に特化したAgent",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
                {"name": "period", "type": "number", "label": "分析期間（日）", "required": False},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
                {"name": "insights", "type": "array", "label": "インサイト"},
                {"name": "suggestions", "type": "array", "label": "提案"},
            ],
            "config": [
                {"name": "sql_schema", "type": "json", "label": "DBスキーマ"},
                {"name": "default_period", "type": "number", "label": "デフォルト期間", "default": 30},
                {"name": "compare_yoy", "type": "boolean", "label": "前年比較", "default": True},
            ],
        }


__all__ = [
    "SalesAgent",
    "SalesAgentConfig",
    "SalesInput",
    "SalesOutput",
]
