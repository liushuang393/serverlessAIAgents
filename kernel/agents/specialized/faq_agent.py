"""FAQ Agent - FAQ 専門Agent (Kernel Specialized)

RAG + Text2SQL + Chart + Suggestion を統合した
FAQ システム専門のAgent。

設計原則：
- ResilientAgent 継承による自動リトライ・タイムアウト制御 (AgentBlock 経由)
- RAGCapableMixin による動的 RAG 接続
- 富文本レスポンス（Markdown、コード、表格、チャート）対応
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from kernel.agents.agent_block import AgentBlock
from kernel.agents.mixins import RAGCapableMixin
from kernel.protocols.a2ui.rich_content import (
    ChartType,
    RichResponse,
)
from shared.services.query_classifier import classify_query


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from kernel.runtime import CapabilityBundle


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


@dataclass
class FAQAgentConfig:
    """FAQAgent 設定."""

    # RAG 設定
    rag_collection: str = "faq_knowledge"
    rag_chunk_strategy: str = "semantic"
    rag_reranker: str = "bm25"
    rag_top_k: int = 5

    # SQL 設定
    sql_schema: dict[str, list[str]] = field(default_factory=dict)
    sql_dialect: str = "postgresql"

    # 機能フラグ
    enable_rag: bool = True
    enable_sql: bool = True
    enable_hybrid: bool = True
    enable_gap_analysis: bool = True
    enable_rich_response: bool = True
    enable_citations: bool = True
    enable_charts: bool = True

    # LLM 設定
    llm_temperature: float = 0.3
    max_tokens: int = 1000


class FAQResponse(BaseModel):
    """FAQ レスポンス."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"  # faq, sql, hybrid
    confidence: float = 0.0

    # 富文本コンポーネント
    rich_response: dict[str, Any] | None = None

    # ソース/引用
    citations: list[dict[str, Any]] = Field(default_factory=list)

    # データ（SQL結果）
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)

    # チャート
    chart: dict[str, Any] | None = None

    # 提案
    suggestions: list[dict[str, Any]] = Field(default_factory=list)

    # ギャップ分析
    gap_info: dict[str, Any] | None = None

    # メタデータ
    execution_time_ms: float = 0
    agent_trace: list[str] = Field(default_factory=list)
    error: str = ""


# =============================================================================
# FAQ Agent
# =============================================================================


class FAQAgent(RAGCapableMixin, AgentBlock):
    """FAQ 専門 Agent (Kernel 実装).

    RAG + Text2SQL + Chart + Suggestion を統合。
    """

    name = "FAQAgent"

    def __init__(
        self,
        config: FAQAgentConfig | None = None,
        llm_client: Any | None = None,
        bundle: CapabilityBundle | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
            llm_client: LLMクライアント
            bundle: CapabilityBundle
        """
        super().__init__()
        self._config = config or FAQAgentConfig()
        self._llm_client = llm_client
        self._logger = logging.getLogger(self.name)

        if bundle is not None:
            self.set_capability_bundle(bundle)

        # Skills (遅延初期化)
        self._retriever = None
        self._answer_generator = None
        self._gap_analyzer = None
        self._text2sql_service = None
        self._suggestion_service = None
        self._initialized = False

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行."""
        start_time = datetime.now()
        question = input_data.get("question", "")

        if not question:
            return FAQResponse(error="質問が指定されていません").model_dump()

        await self._ensure_initialized()

        try:
            # クエリタイプ判定 (Shared Service 使用)
            query_type_enum = classify_query(question)
            query_type = query_type_enum.value
            agent_trace = [f"Query classified as: {query_type}"]

            if query_type == "sql":
                response = await self._handle_sql_query(question, agent_trace)
            elif query_type == "hybrid":
                response = await self._handle_hybrid_query(question, agent_trace)
            else:
                response = await self._handle_faq_query(question, agent_trace)

            if self._config.enable_gap_analysis and response.confidence < 0.7:
                gap_info = await self._analyze_gap(question, response)
                response.gap_info = gap_info

            response.execution_time_ms = (datetime.now() - start_time).total_seconds() * 1000
            return response.model_dump()

        except Exception as e:
            self._logger.exception("FAQ Agent エラー: %s", e)
            return FAQResponse(
                question=question,
                error=str(e),
                execution_time_ms=(datetime.now() - start_time).total_seconds() * 1000,
            ).model_dump()

    async def run_stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        question = input_data.get("question", "")
        execution_id = str(uuid.uuid4())

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 10,
            "message": "クエリを分析中...",
        }

        await self._ensure_initialized()
        query_type = classify_query(question).value

        if query_type == "sql":
            yield {"type": "progress", "progress": 40, "message": "SQL を生成中..."}
            response = await self._handle_sql_query(question, [])
        elif query_type == "hybrid":
            yield {"type": "progress", "progress": 40, "message": "ハイブリッド検索中..."}
            response = await self._handle_hybrid_query(question, [])
        else:
            yield {"type": "progress", "progress": 40, "message": "ナレッジベースを検索中..."}
            response = await self._handle_faq_query(question, [])

        yield {"type": "progress", "progress": 100, "message": "完了"}
        yield {"type": "result", "data": response.model_dump()}

    async def _handle_faq_query(self, question: str, agent_trace: list[str]) -> FAQResponse:
        """FAQ クエリを処理."""
        agent_trace.append("Executing RAG retrieval")
        retrieval_result = await self._retriever.run({"query": question, "top_k": self._config.rag_top_k})
        chunks = retrieval_result.get("chunks", [])

        answer_result = await self._answer_generator.run({"question": question, "context": chunks})
        answer = answer_result.get("answer", "")
        confidence = answer_result.get("confidence", 0.8)
        citations = answer_result.get("citations", [])

        rich_response = None
        if self._config.enable_rich_response:
            rich_response = self._build_rich_response(answer=answer, citations=citations, query_type="faq")

        suggestions = await self._generate_suggestions(question, "faq")

        return FAQResponse(
            question=question,
            answer=answer,
            query_type="faq",
            confidence=confidence,
            rich_response=rich_response.to_dict() if rich_response else None,
            citations=[
                {
                    "id": c.get("doc_id", ""),
                    "title": c.get("title", ""),
                    "snippet": c.get("snippet", ""),
                    "score": c.get("relevance_score", 0),
                }
                for c in citations
            ],
            suggestions=suggestions,
            agent_trace=agent_trace,
        )

    async def _handle_sql_query(self, question: str, agent_trace: list[str]) -> FAQResponse:
        """SQL クエリを処理."""
        if self._text2sql_service is None:
            return FAQResponse(question=question, query_type="sql", answer="SQL サービス不可", error="not_init")

        sql_result = await self._text2sql_service.execute(action="query", question=question)
        if not sql_result.success:
            return FAQResponse(question=question, query_type="sql", error=sql_result.error_message)

        sql = str(sql_result.data.get("sql", ""))
        data = sql_result.data.get("data", [])
        columns = sql_result.data.get("columns", [])
        answer = str(sql_result.data.get("answer", ""))
        chart = sql_result.data.get("chart")

        rich_response = None
        if self._config.enable_rich_response:
            rich_response = self._build_rich_response(
                answer=answer, sql=sql, data=data, columns=columns, query_type="sql"
            )

        suggestions = await self._generate_suggestions(question, "sql")

        return FAQResponse(
            question=question,
            answer=answer,
            query_type="sql",
            confidence=0.9,
            rich_response=rich_response.to_dict() if rich_response else None,
            sql=sql,
            data=data,
            columns=columns,
            chart=chart,
            suggestions=suggestions,
            agent_trace=agent_trace,
        )

    async def _handle_hybrid_query(self, question: str, agent_trace: list[str]) -> FAQResponse:
        """ハイブリッドクエリを処理（RAG + SQL 両方を試行）."""
        agent_trace.append("Executing hybrid query (RAG + SQL)")

        # まず SQL を試行
        sql_response: FAQResponse | None = None
        if self._text2sql_service is not None:
            try:
                sql_result = await self._text2sql_service.execute(
                    action="query",
                    question=question,
                )
                if sql_result.success and sql_result.data.get("data"):
                    sql_response = FAQResponse(
                        question=question,
                        query_type="hybrid",
                        sql=str(sql_result.data.get("sql", "")),
                        data=sql_result.data.get("data", []),
                        columns=sql_result.data.get("columns", []),
                        chart=sql_result.data.get("chart"),
                    )
                    agent_trace.append("SQL query succeeded")
            except Exception as e:
                agent_trace.append(f"SQL query failed: {e}")

        # RAG で回答生成
        retrieval_result = await self._retriever.run(
            {"query": question, "top_k": self._config.rag_top_k},
        )
        chunks = retrieval_result.get("chunks", [])
        answer_result = await self._answer_generator.run(
            {"question": question, "context": chunks},
        )
        answer = answer_result.get("answer", "")
        confidence = answer_result.get("confidence", 0.8)
        citations = answer_result.get("citations", [])

        # SQL 結果があればマージ
        response = FAQResponse(
            question=question,
            answer=answer,
            query_type="hybrid",
            confidence=confidence,
            citations=[
                {
                    "id": c.get("doc_id", ""),
                    "title": c.get("title", ""),
                    "snippet": c.get("snippet", ""),
                    "score": c.get("relevance_score", 0),
                }
                for c in citations
            ],
            suggestions=await self._generate_suggestions(question, "hybrid"),
            agent_trace=agent_trace,
        )

        if sql_response is not None:
            response.sql = sql_response.sql
            response.data = sql_response.data
            response.columns = sql_response.columns
            response.chart = sql_response.chart

        if self._config.enable_rich_response:
            rich = self._build_rich_response(
                answer=answer,
                citations=citations,
                sql=response.sql or None,
                data=response.data or None,
                columns=response.columns or None,
                query_type="hybrid",
            )
            response.rich_response = rich.to_dict()

        return response

    def _build_rich_response(
        self,
        answer: str,
        citations: list[dict[str, Any]] | None = None,
        sql: str | None = None,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
        query_type: str = "faq",
    ) -> RichResponse:
        """富文本レスポンスを構築."""
        response = RichResponse()
        response.add_markdown(answer)
        if sql:
            response.add_code(sql, language="sql", title="SQL")
        if data:
            response.add_table(data=data, title="Results")
            if len(data) > 0 and len(data[0]) >= 2:
                keys = list(data[0].keys())
                response.add_chart_from_data(data=data, x_key=keys[0], y_key=keys[1], chart_type=ChartType.BAR)
        if citations and self._config.enable_citations:
            response.add_citations(citations)
        return response

    async def _generate_suggestions(self, question: str, query_type: str) -> list[dict[str, Any]]:
        """提案生成."""
        if self._suggestion_service:
            try:
                res = await self._suggestion_service.execute(action="suggest", question=question, query_type=query_type)
                if res.success:
                    return res.data.get("suggestions", [])
            except Exception:
                pass
        return [{"text": "詳細を教えて", "type": "followup"}]

    async def _analyze_gap(self, question: str, response: FAQResponse) -> dict[str, Any]:
        """ギャップ分析."""
        if not self._gap_analyzer:
            return {}
        try:
            return await self._gap_analyzer.run(
                {"query_logs": [{"question": question, "confidence": response.confidence}]}
            )
        except Exception:
            return {}

    async def _ensure_initialized(self) -> None:
        """遅延初期化."""
        if self._initialized:
            return

        from kernel.skills.builtin.knowledge_qa import AnswerGenerator, GapAnalyzer, Retriever
        from shared.services import SQLDialect, Text2SQLConfig, Text2SQLService

        self._retriever = Retriever()
        self._answer_generator = AnswerGenerator(llm_client=self._llm_client)
        self._gap_analyzer = GapAnalyzer() if self._config.enable_gap_analysis else None

        try:
            dialect = SQLDialect(self._config.sql_dialect)
        except Exception:
            dialect = SQLDialect.POSTGRESQL

        self._text2sql_service = Text2SQLService(Text2SQLConfig(schema=self._config.sql_schema, dialect=dialect))

        from shared.services import SuggestionConfig, SuggestionService

        self._suggestion_service = SuggestionService(SuggestionConfig())
        self._initialized = True
