# -*- coding: utf-8 -*-
"""FAQ Agent - FAQ 専門Agent（ResilientAgent 準拠）.

RAG + Text2SQL + Chart + Suggestion を統合した
FAQ システム専門のAgent。

設計原則：
- ResilientAgent 継承による自動リトライ・タイムアウト制御
- Pydantic による型安全な入出力
- 松耦合：LLM プロバイダーを意識しない

使用例:
    >>> from agentflow.agents import FAQAgent, FAQInput
    >>>
    >>> agent = FAQAgent(config=FAQAgentConfig(
    ...     rag_collection="faq_knowledge",
    ...     sql_schema={"sales": ["id", "amount", "date"]},
    ... ))
    >>>
    >>> result = await agent.run({"question": "今月の売上TOP10は？"})
    >>> print(result["answer"])
    >>> print(result["chart"])  # EChartsデータ
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
# Pydantic スキーマ（型安全な入出力）
# =============================================================================


class FAQInput(BaseModel):
    """FAQAgent 入力スキーマ."""

    question: str = Field(..., description="質問文")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class DocumentSchema(BaseModel):
    """ソースドキュメントスキーマ."""

    id: str = ""
    content: str = ""
    source: str = ""
    score: float = 0.0


class ChartSchema(BaseModel):
    """チャートデータスキーマ."""

    chart_type: str = "bar"
    title: str = ""
    data: dict[str, Any] = Field(default_factory=dict)


class SuggestionSchema(BaseModel):
    """提案スキーマ."""

    text: str = ""
    type: str = "followup"


class FAQOutput(BaseModel):
    """FAQAgent 出力スキーマ."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"  # "faq" or "sql"
    documents: list[DocumentSchema] = Field(default_factory=list)
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    chart: ChartSchema | None = None
    suggestions: list[SuggestionSchema] = Field(default_factory=list)
    error: str = ""


# =============================================================================
# 設定
# =============================================================================


@dataclass
class FAQAgentConfig:
    """FAQAgent 設定.

    Attributes:
        rag_collection: RAGコレクション名
        rag_chunk_strategy: チャンキング戦略
        rag_reranker: リランカータイプ
        sql_schema: DBスキーマ
        sql_dialect: SQLダイアレクト
        auto_chart: チャート自動生成
        max_suggestions: 最大提案数
    """

    rag_collection: str = "faq_knowledge"
    rag_chunk_strategy: str = "semantic"
    rag_reranker: str = "bm25"
    rag_top_k: int = 5
    sql_schema: dict[str, list[str]] = field(default_factory=dict)
    sql_dialect: str = "postgresql"
    auto_chart: bool = True
    max_suggestions: int = 5


# =============================================================================
# FAQ Agent（ResilientAgent 継承）
# =============================================================================


class FAQAgent(ResilientAgent[FAQInput, FAQOutput]):
    """FAQ専門Agent（ResilientAgent 継承・型安全）.

    RAG + Text2SQL + Chart + Suggestion を統合。

    Note:
        ResilientAgent により自動リトライ・タイムアウトが制御されます。
    """

    # ResilientAgent 設定
    name = "FAQAgent"
    temperature = 0.3

    # システムプロンプト
    SYSTEM_PROMPT = """あなたは企業内部のFAQ専門アシスタントです。

主な職責:
1. ナレッジベースから関連情報を検索して回答する
2. 売上・顧客データに関する質問にはSQLクエリを生成して回答する
3. データを視覚化してわかりやすく説明する
4. 次のステップを提案してユーザーをサポートする

回答ルール:
- 簡潔で正確な回答を心がける
- ソースを明示する（[1]、[2]等）
- 不明な点は正直に伝える
- 追加の質問を促す提案を行う"""

    def __init__(
        self,
        config: FAQAgentConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初期化.

        Args:
            config: FAQAgent設定
            llm_client: LLMクライアント（オプション、松耦合）
        """
        super().__init__(llm_client)
        self._config = config or FAQAgentConfig()
        self._logger = logging.getLogger(self.name)
        self._services_initialized = False

        # サービスインスタンス（遅延初期化・私有化）
        self.__rag_service = None
        self.__sql_service = None
        self.__chart_service = None
        self.__suggestion_service = None

    def _parse_input(self, input_data: dict[str, Any]) -> FAQInput:
        """入力データを Pydantic モデルに変換."""
        return FAQInput(**input_data)

    async def process(self, input_data: FAQInput) -> FAQOutput:
        """FAQ 処理を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き出力データ
        """
        await self._ensure_services()

        question = input_data.question
        if not question:
            return FAQOutput(error="質問が指定されていません")

        # クエリタイプを判定
        query_type = self._classify_query(question)

        try:
            if query_type == "sql":
                return await self._handle_sql_query(question, query_type)
            else:
                return await self._handle_faq_query(question, query_type)

        except Exception as e:
            self._logger.error(f"FAQAgent実行エラー: {e}")
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=f"申し訳ありません。エラーが発生しました: {e}",
                error=str(e),
            )

    async def _handle_faq_query(self, question: str, query_type: str) -> FAQOutput:
        """FAQ クエリを処理."""
        rag_result = await self.__rag_service.execute(
            action="query",
            question=question,
        )

        documents = [
            DocumentSchema(
                id=d.get("id", ""),
                content=d.get("content", ""),
                source=d.get("source", ""),
                score=d.get("score", 0.0),
            )
            for d in rag_result.data.get("documents", [])
        ]

        suggestions = await self._generate_suggestions(question, query_type, bool(documents))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=rag_result.data.get("answer", ""),
            documents=documents,
            suggestions=suggestions,
        )

    async def _handle_sql_query(self, question: str, query_type: str) -> FAQOutput:
        """SQL クエリを処理."""
        sql_result = await self.__sql_service.execute(
            action="query",
            question=question,
        )

        chart_data = sql_result.data.get("chart")
        chart = None
        if chart_data:
            chart = ChartSchema(
                chart_type=chart_data.get("type", "bar"),
                title=chart_data.get("title", ""),
                data=chart_data.get("data", {}),
            )

        suggestions = await self._generate_suggestions(
            question, query_type, bool(sql_result.data.get("data"))
        )

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=sql_result.data.get("answer", ""),
            sql=sql_result.data.get("sql", ""),
            data=sql_result.data.get("data", []),
            columns=sql_result.data.get("columns", []),
            chart=chart,
            suggestions=suggestions,
        )

    async def _generate_suggestions(
        self, question: str, query_type: str, data_found: bool
    ) -> list[SuggestionSchema]:
        """フォローアップ提案を生成."""
        try:
            suggestion_result = await self.__suggestion_service.execute(
                question=question,
                context={"query_type": query_type, "data_found": data_found},
            )
            return [
                SuggestionSchema(text=s.get("text", ""), type=s.get("type", "followup"))
                for s in suggestion_result.data.get("suggestions", [])
            ]
        except Exception as e:
            self._logger.warning(f"提案生成エラー: {e}")
            return []

    async def _ensure_services(self) -> None:
        """サービスの遅延初期化（私有メソッド）."""
        if self._services_initialized:
            return

        from agentflow.services import (
            ChartConfig,
            ChartService,
            RAGConfig,
            RAGService,
            SuggestionConfig,
            SuggestionService,
            Text2SQLConfig,
            Text2SQLService,
        )

        # RAGサービス
        self.__rag_service = RAGService(
            RAGConfig(
                collection=self._config.rag_collection,
                chunk_strategy=self._config.rag_chunk_strategy,
                reranker_type=self._config.rag_reranker,
                top_k=self._config.rag_top_k,
            )
        )

        # Text2SQLサービス
        self.__sql_service = Text2SQLService(
            Text2SQLConfig(
                schema=self._config.sql_schema,
                dialect=self._config.sql_dialect,
                auto_chart=self._config.auto_chart,
            )
        )

        # Chartサービス
        self.__chart_service = ChartService(ChartConfig())

        # Suggestionサービス
        self.__suggestion_service = SuggestionService(
            SuggestionConfig(max_suggestions=self._config.max_suggestions)
        )

        self._services_initialized = True

    def _classify_query(self, question: str) -> str:
        """クエリタイプを判定（私有メソッド）.

        Args:
            question: 質問文

        Returns:
            "sql" または "faq"
        """
        sql_keywords = [
            "売上", "収入", "数量", "統計", "報表", "レポート",
            "top", "ランキング", "トレンド", "比較", "同比", "前年比",
            "金額", "注文", "顧客数", "件数", "合計", "平均",
            "月別", "年別", "日別", "カテゴリ別",
        ]

        question_lower = question.lower()
        sql_score = sum(1 for k in sql_keywords if k in question_lower)

        return "sql" if sql_score >= 2 else "faq"

    def validate_output(self, output: FAQOutput) -> bool:
        """出力検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True = 有効）
        """
        if output.error and not output.answer:
            self._logger.warning(f"Validation warning: error without answer: {output.error}")
        return True

    # =========================================================================
    # AgentPool 統合用（公開プロパティ）
    # =========================================================================

    @property
    def agent_type(self) -> str:
        """Agentタイプ."""
        return "faq"

    @property
    def skills(self) -> list[str]:
        """利用可能なスキル."""
        return ["rag", "text2sql", "chart", "suggestion"]

    @classmethod
    def get_definition(cls) -> dict[str, Any]:
        """Agent定義（Studio用）."""
        return {
            "type": "faq",
            "name": "FAQAgent",
            "label": "FAQ専門Agent",
            "category": "specialized",
            "icon": "question-circle",
            "description": "RAG + SQL + Chart + Suggestion を統合したFAQ専門Agent",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "query_type", "type": "string", "label": "クエリタイプ"},
                {"name": "documents", "type": "array", "label": "ソースドキュメント"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
                {"name": "suggestions", "type": "array", "label": "提案"},
            ],
            "config": [
                {
                    "name": "rag_collection",
                    "type": "string",
                    "label": "RAGコレクション",
                    "default": "faq_knowledge",
                },
                {"name": "sql_schema", "type": "json", "label": "DBスキーマ"},
                {"name": "auto_chart", "type": "boolean", "label": "チャート自動生成", "default": True},
            ],
        }


__all__ = [
    "FAQAgent",
    "FAQAgentConfig",
    "FAQInput",
    "FAQOutput",
    "DocumentSchema",
    "ChartSchema",
    "SuggestionSchema",
]
