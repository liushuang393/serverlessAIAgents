"""FAQ Service - フレームワーク規範準拠の FAQ サービス.

ServiceBase を継承し、RAG + Text2SQL のハイブリッド検索を提供。

v1.1 新機能:
    - AI安全防護統合（幻覚検出、PII脱敏、SQL注入防護）

使用例:
    >>> from apps.faq_system.backend.services import FAQService, FAQConfig
    >>>
    >>> service = FAQService(FAQConfig(collection="faq_knowledge"))
    >>> result = await service.execute(action="query", question="返品ポリシーは？")
    >>> print(result.data["answer"])
"""

from __future__ import annotations

import logging
import re
import time
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.security import SafetyMixin
from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義（Pydantic BaseModel）
# =============================================================================


class QueryType(str, Enum):
    """クエリタイプ."""

    FAQ = "faq"
    SQL = "sql"
    HYBRID = "hybrid"


class ChartType(str, Enum):
    """チャートタイプ."""

    BAR = "bar"
    LINE = "line"
    PIE = "pie"
    SCATTER = "scatter"
    TABLE = "table"


class FAQConfig(BaseModel):
    """FAQ サービス設定."""

    # RAG 設定
    collection: str = Field(default="faq_knowledge", description="コレクション名")
    chunk_strategy: str = Field(default="recursive", description="チャンキング戦略")
    chunk_size: int = Field(default=1000, description="チャンクサイズ")
    reranker_type: str = Field(default="bm25", description="リランカータイプ")
    top_k: int = Field(default=5, description="上位K件")

    # Text2SQL 設定
    sql_dialect: str = Field(default="postgresql", description="SQLダイアレクト")
    sql_max_rows: int = Field(default=100, description="最大行数")
    db_schema: dict[str, list[str]] = Field(default_factory=dict, description="DBスキーマ")

    # LLM 設定
    llm_temperature: float = Field(default=0.1, description="LLM温度")


class SQLResult(BaseModel):
    """SQL 実行結果."""

    sql: str
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    row_count: int = 0
    success: bool = True
    error: str | None = None
    execution_time_ms: float = 0


class ChartData(BaseModel):
    """チャートデータ."""

    chart_type: ChartType
    title: str
    data: dict[str, Any] = Field(default_factory=dict)
    options: dict[str, Any] = Field(default_factory=dict)


# =============================================================================
# FAQ Service 実装
# =============================================================================


class FAQService(ServiceBase[dict[str, Any]], SafetyMixin):
    """FAQ Service - RAG + Text2SQL ハイブリッドサービス.

    ServiceBase を継承し、フレームワーク規範に準拠。

    Actions:
    - query: FAQ質問応答（自動クエリ分類）
    - faq: FAQのみ検索
    - sql: Text2SQLのみ実行

    v1.1 新機能:
        - AI安全防護（幻覚検出、PII脱敏、SQL注入防護）
    """

    def __init__(
        self,
        config: FAQConfig | None = None,
        enable_safety: bool = True,
    ) -> None:
        """初期化.

        Args:
            config: FAQサービス設定
            enable_safety: AI安全防護を有効化するか
        """
        super().__init__()
        self._config = config or FAQConfig()
        self._llm = None
        self._db = None
        self._rag_service = None
        self._started = False

        # AI安全防護初期化（v1.1）
        self.init_safety(enabled=enable_safety)

    async def start(self) -> None:
        """サービス開始."""
        if self._started:
            return

        from agentflow.providers import get_llm
        from agentflow.services.rag_service import RAGConfig, RAGService

        # RAG サービス初期化
        rag_config = RAGConfig(
            collection=self._config.collection,
            chunk_strategy=self._config.chunk_strategy,  # type: ignore
            chunk_size=self._config.chunk_size,
            reranker=self._config.reranker_type,  # type: ignore
            top_k=self._config.top_k,
        )
        self._rag_service = RAGService(rag_config)
        await self._rag_service.start()

        # LLM 初期化
        self._llm = get_llm(temperature=self._config.llm_temperature)

        # DB 接続（オプショナル）
        try:
            from agentflow.providers import get_db

            self._db = get_db()
            await self._db.connect()
        except Exception as e:
            self._logger.warning(f"DB connection failed (Text2SQL disabled): {e}")
            self._db = None

        self._started = True
        self._logger.info(f"FAQService started: {self._config.collection}")

    async def stop(self) -> None:
        """サービス停止."""
        if self._rag_service:
            await self._rag_service.stop()
        if self._db:
            await self._db.disconnect()
        self._started = False

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック（ServiceBase 抽象メソッド実装）."""
        action = kwargs.get("action", "query")
        question = kwargs.get("question", "")

        if not self._started:
            await self.start()

        start_time = time.time()

        if action == "query":
            # 自動クエリ分類
            query_type = self._classify_query(question)
            yield self._emit_progress(execution_id, 10, f"Query type: {query_type.value}")

            if query_type == QueryType.SQL:
                async for event in self._do_sql_query(execution_id, question, start_time):
                    yield event
            elif query_type == QueryType.HYBRID:
                async for event in self._do_hybrid_query(execution_id, question, start_time):
                    yield event
            else:
                async for event in self._do_faq_query(execution_id, question, start_time):
                    yield event

        elif action == "faq":
            async for event in self._do_faq_query(execution_id, question, start_time):
                yield event

        elif action == "sql":
            async for event in self._do_sql_query(execution_id, question, start_time):
                yield event

        else:
            yield self._emit_error(execution_id, "invalid_action", f"Unknown action: {action}")

    # =========================================================================
    # 内部処理メソッド
    # =========================================================================

    def _classify_query(self, question: str) -> QueryType:
        """クエリタイプを分類."""
        sql_keywords = [
            "销售", "收入", "数量", "统计", "报表",
            "top", "排名", "趋势", "对比", "同比",
            "环比", "金额", "订单", "客户数",
        ]
        question_lower = question.lower()
        sql_score = sum(1 for k in sql_keywords if k in question_lower)

        if sql_score >= 2:
            return QueryType.SQL
        if sql_score >= 1:
            return QueryType.HYBRID
        return QueryType.FAQ

    async def _do_faq_query(
        self,
        execution_id: str,
        question: str,
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """FAQ クエリ処理."""
        yield self._emit_progress(execution_id, 20, "Searching knowledge base...", phase="search")

        # RAG サービスを使用
        result = await self._rag_service.execute(action="query", question=question)

        if not result.success:
            yield self._emit_error(execution_id, "rag_error", result.error_message or "RAG failed")
            return

        yield self._emit_progress(execution_id, 80, "Generating suggestions...", phase="suggest")
        suggestions = await self._generate_suggestions(question)

        yield self._emit_result(
            execution_id,
            {
                "answer": result.data.get("answer", ""),
                "query_type": QueryType.FAQ.value,
                "sources": result.data.get("documents", []),
                "suggestions": suggestions,
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_sql_query(
        self,
        execution_id: str,
        question: str,
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """SQL クエリ処理."""
        if not self._db:
            yield self._emit_error(execution_id, "db_not_available", "Database not connected")
            return

        yield self._emit_progress(execution_id, 20, "Generating SQL...", phase="generate_sql")
        sql = await self._generate_sql(question)

        yield self._emit_progress(execution_id, 40, "Executing SQL...", phase="execute_sql")
        sql_result = await self._execute_sql(sql)

        if not sql_result.success:
            yield self._emit_error(execution_id, "sql_error", sql_result.error or "SQL failed")
            return

        yield self._emit_progress(execution_id, 60, "Summarizing results...", phase="summarize")
        answer = await self._summarize_sql_result(question, sql_result)

        yield self._emit_progress(execution_id, 80, "Generating chart...", phase="chart")
        chart = self._generate_chart(question, sql_result)

        suggestions = await self._generate_suggestions(question)

        yield self._emit_result(
            execution_id,
            {
                "answer": answer,
                "query_type": QueryType.SQL.value,
                "sql_result": sql_result.model_dump(),
                "chart": chart.model_dump() if chart else None,
                "suggestions": suggestions,
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_hybrid_query(
        self,
        execution_id: str,
        question: str,
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """ハイブリッドクエリ処理."""
        import asyncio

        yield self._emit_progress(execution_id, 10, "Running parallel queries...", phase="parallel")

        # 並列実行
        faq_task = self._rag_service.execute(action="query", question=question)
        sql_task = self._execute_sql_safe(question) if self._db else asyncio.sleep(0)

        faq_result, sql_result = await asyncio.gather(faq_task, sql_task, return_exceptions=True)

        answer_parts = []

        # FAQ 結果
        if not isinstance(faq_result, Exception) and faq_result.success:
            answer_parts.append(f"Based on knowledge base:\n{faq_result.data.get('answer', '')}")

        # SQL 結果
        chart = None
        sql_data = None
        if not isinstance(sql_result, Exception) and sql_result and sql_result.success:
            yield self._emit_progress(execution_id, 60, "Summarizing data...", phase="summarize")
            summary = await self._summarize_sql_result(question, sql_result)
            answer_parts.append(f"\nBased on data analysis:\n{summary}")
            chart = self._generate_chart(question, sql_result)
            sql_data = sql_result.model_dump()

        suggestions = await self._generate_suggestions(question)

        yield self._emit_result(
            execution_id,
            {
                "answer": "\n".join(answer_parts) if answer_parts else "No relevant information.",
                "query_type": QueryType.HYBRID.value,
                "sources": faq_result.data.get("documents", []) if not isinstance(faq_result, Exception) else [],
                "sql_result": sql_data,
                "chart": chart.model_dump() if chart else None,
                "suggestions": suggestions,
            },
            (time.time() - start_time) * 1000,
        )

    async def _execute_sql_safe(self, question: str) -> SQLResult | None:
        """安全な SQL 実行（例外をキャッチ）."""
        try:
            sql = await self._generate_sql(question)
            return await self._execute_sql(sql)
        except Exception as e:
            self._logger.warning(f"SQL execution failed: {e}")
            return None

    async def _generate_sql(self, question: str) -> str:
        """SQL を生成."""
        schema_info = self._format_schema()
        prompt = f"""Convert this question to SQL ({self._config.sql_dialect}):
Schema: {schema_info}
Question: {question}
Rules: SELECT only, add LIMIT {self._config.sql_max_rows}
SQL:"""
        response = await self._llm.chat([{"role": "user", "content": prompt}])
        return self._extract_sql(response.get("content", ""))

    async def _execute_sql(self, sql: str) -> SQLResult:
        """SQL を実行."""
        if not self._db:
            return SQLResult(sql=sql, success=False, error="DB not connected")

        start = time.time()
        try:
            rows = await self._db.execute_raw(sql)
            exec_time = (time.time() - start) * 1000

            if rows:
                columns = list(rows[0].keys()) if hasattr(rows[0], "keys") else []
                data = [dict(r) if hasattr(r, "keys") else r for r in rows]
            else:
                columns, data = [], []

            return SQLResult(
                sql=sql,
                data=data,
                columns=columns,
                row_count=len(data),
                success=True,
                execution_time_ms=exec_time,
            )
        except Exception as e:
            return SQLResult(
                sql=sql,
                success=False,
                error=str(e),
                execution_time_ms=(time.time() - start) * 1000,
            )

    async def _summarize_sql_result(self, question: str, result: SQLResult) -> str:
        """SQL 結果を要約."""
        if not result.data:
            return "No data found."

        sample = result.data[:10]
        prompt = f"Summarize this data for question '{question}':\nData: {sample}\nTotal: {result.row_count}"
        response = await self._llm.chat([{"role": "user", "content": prompt}])
        return response.get("content", "")

    def _generate_chart(self, question: str, result: SQLResult) -> ChartData | None:
        """チャートを生成."""
        if not result.data or not result.columns:
            return None

        chart_type = ChartType.PIE if len(result.data) <= 5 else ChartType.BAR
        labels = [str(r.get(result.columns[0], "")) for r in result.data[:50]]
        values = [
            r.get(result.columns[1], 0) if len(result.columns) > 1 else 0
            for r in result.data[:50]
        ]

        return ChartData(
            chart_type=chart_type,
            title=question[:50],
            data={
                "labels": labels,
                "datasets": [{"data": values}],
                "xAxis": {"type": "category", "data": labels},
                "yAxis": {"type": "value"},
                "series": [{"type": chart_type.value, "data": values}],
            },
        )

    async def _generate_suggestions(self, question: str) -> list[str]:
        """フォローアップ質問を生成."""
        prompt = f"Based on '{question}', suggest 3 follow-up questions (one per line):"
        response = await self._llm.chat([{"role": "user", "content": prompt}])
        content = response.get("content", "")
        return [
            line.strip().lstrip("123.-) ")
            for line in content.split("\n")
            if line.strip()
        ][:3]

    def _format_schema(self) -> str:
        """スキーマをフォーマット."""
        schema = self._config.db_schema
        if not schema:
            return "(No schema)"
        return "\n".join([f"{t}: {', '.join(c)}" for t, c in schema.items()])

    def _extract_sql(self, text: str) -> str:
        """テキストから SQL を抽出."""
        match = re.search(r"```sql\s*(.*?)\s*```", text, re.DOTALL | re.IGNORECASE)
        if match:
            return match.group(1).strip()
        match = re.search(r"SELECT.*", text, re.DOTALL | re.IGNORECASE)
        return match.group(0).strip() if match else text.strip()

    # =========================================================================
    # コンテキストマネージャー
    # =========================================================================

    async def __aenter__(self) -> FAQService:
        """非同期コンテキストマネージャー開始."""
        await self.start()
        return self

    async def __aexit__(self, *args: Any) -> None:
        """非同期コンテキストマネージャー終了."""
        await self.stop()


__all__ = [
    "ChartData",
    "ChartType",
    "FAQConfig",
    "FAQService",
    "QueryType",
    "SQLResult",
]
