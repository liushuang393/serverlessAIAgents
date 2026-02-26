"""強化版 FAQ Agent - Orchestrator + Skills + 富文本対応.

このAgentは以下の改善を含みます：
1. Orchestrator パターンによる複数Agent協調
2. knowledge_qa Skills の活用
3. 富文本レスポンス（Markdown、コード、表格、チャート）
4. リアルタイム進捗状態同期
5. 引用/ソース表示

使用例:
    >>> from apps.faq_system.backend.agents import EnhancedFAQAgent
    >>>
    >>> agent = EnhancedFAQAgent()
    >>> result = await agent.run({"question": "返品ポリシーは？"})
    >>> print(result["rich_response"])  # 富文本コンポーネント
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.agents.mixins import RAGCapableMixin
from agentflow.core.agent_block import AgentBlock
from agentflow.protocols.a2ui.rich_content import (
    ChartType,
    RichResponse,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.bootstrap.capability_bundle import CapabilityBundle


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


@dataclass
class EnhancedFAQConfig:
    """強化版FAQAgent 設定."""

    # RAG 設定
    rag_collection: str = "faq_knowledge"
    rag_top_k: int = 5

    # SQL 設定
    sql_schema: dict[str, list[str]] = field(default_factory=dict)
    sql_dialect: str = "postgresql"

    # 機能フラグ
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
# 強化版 FAQ Agent
# =============================================================================


class EnhancedFAQAgent(RAGCapableMixin, AgentBlock):
    """強化版 FAQ Agent.

    特徴：
    - 複数 Agent 協調（Retriever + AnswerGenerator + GapAnalyzer）
    - 富文本レスポンス
    - リアルタイム進捗
    - 引用表示
    - RAGCapableMixin: CapabilityBundle 経由の動的 RAG 接続
    """

    name = "EnhancedFAQAgent"

    def __init__(
        self,
        config: EnhancedFAQConfig | None = None,
        llm_client: Any | None = None,
        bundle: "CapabilityBundle | None" = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
            llm_client: LLMクライアント
            bundle: AppCapabilityBootstrapper が構築した CapabilityBundle
        """
        super().__init__()
        self._config = config or EnhancedFAQConfig()
        self._llm_client = llm_client
        self._logger = logging.getLogger(self.name)

        # CapabilityBundle を設定（RAGCapableMixin 経由）
        if bundle is not None:
            self.set_capability_bundle(bundle)

        # Skills（遅延初期化）
        self._retriever = None
        self._answer_generator = None
        self._gap_analyzer = None
        self._text2sql_service = None
        self._initialized = False

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行.

        Args:
            input_data: 入力データ {"question": "..."}

        Returns:
            FAQResponse のdict形式
        """
        start_time = datetime.now()
        question = input_data.get("question", "")
        input_data.get("session_id", str(uuid.uuid4()))

        if not question:
            return FAQResponse(error="質問が指定されていません").model_dump()

        await self._ensure_initialized()

        try:
            # クエリタイプ判定
            query_type = self._classify_query(question)
            agent_trace = [f"Query classified as: {query_type}"]

            # 処理実行
            if query_type == "sql":
                response = await self._handle_sql_query(question, agent_trace)
            else:
                response = await self._handle_faq_query(question, agent_trace)

            # ギャップ分析（オプション）
            if self._config.enable_gap_analysis and response.confidence < 0.7:
                gap_info = await self._analyze_gap(question, response)
                response.gap_info = gap_info

            # 実行時間
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
        """ストリーム実行.

        Args:
            input_data: 入力データ

        Yields:
            進捗イベント
        """
        question = input_data.get("question", "")
        execution_id = str(uuid.uuid4())

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 0,
            "message": "処理を開始しています...",
        }

        await self._ensure_initialized()

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 10,
            "message": "クエリを分析中...",
        }

        query_type = self._classify_query(question)

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 20,
            "message": f"クエリタイプ: {query_type}",
        }

        if query_type == "sql":
            yield {
                "type": "progress",
                "execution_id": execution_id,
                "progress": 40,
                "message": "SQL を生成中...",
            }
            response = await self._handle_sql_query(question, [])
        else:
            yield {
                "type": "progress",
                "execution_id": execution_id,
                "progress": 40,
                "message": "ナレッジベースを検索中...",
            }
            response = await self._handle_faq_query(question, [])

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 80,
            "message": "回答を生成中...",
        }

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 100,
            "message": "完了",
        }

        yield {
            "type": "result",
            "execution_id": execution_id,
            "data": response.model_dump(),
        }

    async def _handle_faq_query(self, question: str, agent_trace: list[str]) -> FAQResponse:
        """FAQ クエリを処理."""
        agent_trace.append("Executing RAG retrieval")

        # Retriever で検索
        retrieval_result = await self._retriever.run(
            {
                "query": question,
                "top_k": self._config.rag_top_k,
            }
        )

        chunks = retrieval_result.get("chunks", [])
        agent_trace.append(f"Retrieved {len(chunks)} chunks")

        # AnswerGenerator で回答生成
        answer_result = await self._answer_generator.run(
            {
                "question": question,
                "context": chunks,
            }
        )

        answer = answer_result.get("answer", "")
        confidence = answer_result.get("confidence", 0.8)
        citations = answer_result.get("citations", [])

        # 富文本レスポンス生成
        rich_response = None
        if self._config.enable_rich_response:
            rich_response = self._build_rich_response(
                answer=answer,
                citations=citations,
                query_type="faq",
            )

        # 提案生成
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
            return FAQResponse(
                question=question,
                query_type="sql",
                answer="SQL サービスが利用できません。",
                confidence=0.0,
                error="sql_service_not_initialized",
                agent_trace=agent_trace,
            )

        agent_trace.append("Executing Text2SQL service")
        sql_result = await self._text2sql_service.execute(action="query", question=question)
        if not sql_result.success:
            message = sql_result.error_message or "SQL 実行に失敗しました。"
            return FAQResponse(
                question=question,
                query_type="sql",
                answer=message,
                confidence=0.0,
                error=message,
                agent_trace=agent_trace,
            )

        sql = str(sql_result.data.get("sql", ""))
        data = sql_result.data.get("data", [])
        columns = sql_result.data.get("columns", [])
        answer = str(sql_result.data.get("answer", ""))
        chart = sql_result.data.get("chart")
        agent_trace.append(f"Text2SQL succeeded: rows={len(data) if isinstance(data, list) else 0}")

        # 富文本レスポンス生成
        rich_response = None
        if self._config.enable_rich_response:
            rich_response = self._build_rich_response(
                answer=answer,
                sql=sql,
                data=data if isinstance(data, list) else [],
                columns=columns if isinstance(columns, list) else [],
                query_type="sql",
            )

        suggestions = await self._generate_suggestions(question, "sql")

        return FAQResponse(
            question=question,
            answer=answer,
            query_type="sql",
            confidence=0.9,
            rich_response=rich_response.to_dict() if rich_response else None,
            sql=sql,
            data=data if isinstance(data, list) else [],
            columns=columns if isinstance(columns, list) else [],
            chart=chart if isinstance(chart, dict) else None,
            suggestions=suggestions,
            agent_trace=agent_trace,
        )

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

        # 回答（Markdown）
        response.add_markdown(answer)

        # SQL（コードブロック）
        if sql:
            response.add_code(sql, language="sql", title="生成されたSQL")

        # データテーブル
        if data:
            response.add_table(
                data=data,
                title="クエリ結果",
                sortable=True,
                paginated=len(data) > 10,
            )

            # チャート（データから自動生成）
            if len(data) > 0 and len(data[0]) >= 2:
                keys = list(data[0].keys())
                # 数値カラムを探す
                numeric_key = None
                for k in keys[1:]:
                    if isinstance(data[0].get(k), (int, float)):
                        numeric_key = k
                        break

                if numeric_key:
                    response.add_chart_from_data(
                        data=data,
                        x_key=keys[0],
                        y_key=numeric_key,
                        chart_type=ChartType.BAR,
                        title="データ可視化",
                    )

        # 引用
        if citations and self._config.enable_citations:
            response.add_markdown("\n\n### 参照ソース")
            response.add_citations(citations)

        return response

    async def _generate_suggestions(self, question: str, query_type: str) -> list[dict[str, Any]]:
        """フォローアップ提案を生成."""
        # 簡易実装
        if query_type == "sql":
            return [
                {"text": "前月との比較を見せて", "type": "followup"},
                {"text": "カテゴリ別の内訳は？", "type": "followup"},
                {"text": "トップ10を表示", "type": "followup"},
            ]
        return [
            {"text": "もう少し詳しく教えて", "type": "followup"},
            {"text": "関連する情報は？", "type": "followup"},
            {"text": "例を見せて", "type": "followup"},
        ]

    async def _analyze_gap(self, question: str, response: FAQResponse) -> dict[str, Any]:
        """ギャップ分析を実行."""
        if not self._gap_analyzer:
            return {}

        try:
            return await self._gap_analyzer.run(
                {
                    "query_logs": [
                        {
                            "question": question,
                            "confidence": response.confidence,
                            "answered": bool(response.answer),
                        }
                    ]
                }
            )
        except Exception as e:
            self._logger.warning("ギャップ分析エラー: %s", e)
            return {}

    def _classify_query(self, question: str) -> str:
        """クエリタイプを判定."""
        sql_keywords = [
            "売上",
            "収入",
            "数量",
            "統計",
            "レポート",
            "top",
            "ランキング",
            "トレンド",
            "比較",
            "金額",
            "注文",
            "顧客数",
            "件数",
            "合計",
            "平均",
            "月別",
            "年別",
            "日別",
        ]

        question_lower = question.lower()
        sql_score = sum(1 for k in sql_keywords if k in question_lower)

        return "sql" if sql_score >= 2 else "faq"

    async def _ensure_initialized(self) -> None:
        """Skills の遅延初期化."""
        if self._initialized:
            return

        from agentflow.services import SQLDialect, Text2SQLConfig, Text2SQLService
        from agentflow.skills.builtin.knowledge_qa import (
            AnswerGenerator,
            GapAnalyzer,
            Retriever,
        )

        self._retriever = Retriever()
        self._answer_generator = AnswerGenerator(llm_client=self._llm_client)
        self._gap_analyzer = GapAnalyzer() if self._config.enable_gap_analysis else None
        try:
            dialect = SQLDialect(self._config.sql_dialect)
        except ValueError:
            dialect = SQLDialect.POSTGRESQL
        self._text2sql_service = Text2SQLService(
            Text2SQLConfig(
                schema=self._config.sql_schema,
                dialect=dialect,
                auto_chart=self._config.enable_charts,
            )
        )

        self._initialized = True
        self._logger.info("EnhancedFAQAgent initialized")


__all__ = [
    "EnhancedFAQAgent",
    "EnhancedFAQConfig",
    "FAQResponse",
]
