"""FAQ Agent - Thin Wrapper around Kernel FAQAgent.

業務ロジックの大部分は kernel.agents.specialized.faq_agent.FAQAgent に移動しました。
このファイルは後方互換性のために残されています。
"""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field

from kernel.agents.specialized.faq_agent import (
    FAQAgent as KernelFAQAgent,
    FAQAgentConfig,
    FAQResponse,
)

logger = logging.getLogger(__name__)


# =============================================================================
# 互換スキーマ
# =============================================================================


class FAQInput(BaseModel):
    """FAQAgent 入力スキーマ (互換用)."""

    question: str = Field(..., description="質問文")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class DocumentSchema(BaseModel):
    """ソースドキュメントスキーマ (互換用)."""

    id: str = ""
    content: str = ""
    source: str = ""
    score: float = 0.0


class ChartSchema(BaseModel):
    """チャートデータスキーマ (互換用)."""

    chart_type: str = "bar"
    title: str = ""
    data: dict[str, Any] = Field(default_factory=dict)


class SuggestionSchema(BaseModel):
    """提案スキーマ (互換用)."""

    text: str = ""
    type: str = "followup"


class FAQOutput(BaseModel):
    """FAQAgent 出力スキーマ (互換用)."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"
    documents: list[DocumentSchema] = Field(default_factory=list)
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    chart: ChartSchema | None = None
    rich_response: dict[str, Any] | None = None
    artifacts: list[dict[str, Any]] = Field(default_factory=list)
    execution_report: dict[str, Any] = Field(default_factory=dict)
    suggestions: list[SuggestionSchema] = Field(default_factory=list)
    verification: dict[str, Any] = Field(default_factory=dict)
    error: str = ""


# =============================================================================
# FAQ Agent Wrapper
# =============================================================================


class FAQAgent(KernelFAQAgent):
    """FAQ専門Agent (App Wrapper). 実装は kernel 側に移動済み."""

    def __init__(
        self,
        config: FAQAgentConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        super().__init__(config=config, llm_client=llm_client)

    async def process(self, input_data: FAQInput) -> FAQOutput:
        """Kernel Agent を呼び出し、互換性のある出力を返す."""
        result_dict = await self.run(input_data.model_dump())
        output = FAQOutput(
            question=result_dict.get("question", ""),
            answer=result_dict.get("answer", ""),
            query_type=result_dict.get("query_type", "faq"),
            sql=result_dict.get("sql", ""),
            data=result_dict.get("data", []),
            columns=result_dict.get("columns", []),
            error=result_dict.get("error", ""),
        )
        if "rich_response" in result_dict:
            output.rich_response = result_dict["rich_response"]
        if "chart" in result_dict and isinstance(result_dict["chart"], dict):
            output.chart = ChartSchema(**result_dict["chart"])
        if "suggestions" in result_dict:
            output.suggestions = [
                SuggestionSchema(**s) for s in result_dict["suggestions"] if isinstance(s, dict)
            ]
        if "citations" in result_dict:
            output.documents = [
                DocumentSchema(
                    id=c.get("id", ""), content=c.get("snippet", ""),
                    source=c.get("title", ""), score=c.get("score", 0.0),
                ) for c in result_dict["citations"] if isinstance(c, dict)
            ]
        return output

    # -----------------------------------------------------------------------
    # 互換用内部メソッド (テスト用)
    # -----------------------------------------------------------------------

    @property
    def _llm(self) -> Any:
        return self._llm_client

    @_llm.setter
    def _llm(self, value: Any) -> None:
        self._llm_client = value

    async def _classify_query(self, question: str) -> str:
        """テスト互換用のクエリ分類."""
        from shared.services.query_classifier import classify_query
        return classify_query(question).value

    def _classify_query_heuristic(self, question: str) -> str:
        """テスト互換用のヒューリスティック分類."""
        from shared.services.query_classifier import QueryClassifier
        return QueryClassifier().classify(question).value

    async def _handle_chat_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用: Kernel run() を経由して実行."""
        return await self.process(FAQInput(question=question))

    async def _handle_faq_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_sql_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_weather_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_external_query(self, question: str, query_type: str, context: dict[str, Any] | None = None) -> FAQOutput:
        return await self.process(FAQInput(question=question, context=context or {}))

    async def _handle_hybrid_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))


__all__ = ["FAQAgent", "FAQAgentConfig", "FAQInput", "FAQOutput"]
