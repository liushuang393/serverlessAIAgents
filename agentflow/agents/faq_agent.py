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

import json
import logging
import os
import re
import time
import uuid
from contextlib import suppress
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field

# 循環インポート回避: agentflow.core から直接インポート
from agentflow.core import ResilientAgent
from agentflow.protocols.a2a_card import AgentCard, AgentSkill
from agentflow.protocols.a2ui.rich_content import (
    AlertType,
    ChartType,
    RichResponse,
)


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


class IntentRouteDecision(BaseModel):
    """LLM 意图路由决策."""

    route: str = "unclear"
    confidence: float = 0.0
    reason: str = ""


class FAQOutput(BaseModel):
    """FAQAgent 出力スキーマ."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"  # "chat" | "faq" | "sql" | "hybrid" | "sales_material" | "weather" | "external" | "blocked"
    documents: list[DocumentSchema] = Field(default_factory=list)
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    chart: ChartSchema | None = None
    rich_response: dict[str, Any] | None = None
    artifacts: list[dict[str, Any]] = Field(default_factory=list)
    execution_report: dict[str, Any] = Field(default_factory=dict)
    a2a: dict[str, Any] = Field(default_factory=dict)
    suggestions: list[SuggestionSchema] = Field(default_factory=list)
    verification: dict[str, Any] = Field(default_factory=dict)
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
    enable_rag: bool = True
    enable_sql: bool = True
    enable_hybrid: bool = True
    sales_material_output_root: str = field(
        default_factory=lambda: os.getenv("FAQ_SALES_MATERIAL_DIR", "/tmp/faq_sales_material")
    )
    sales_material_default_num_images: int = 4
    sales_material_max_images: int = 12
    enable_sales_material: bool = True
    enable_weather: bool = True
    weather_default_days: int = 3
    enable_external_tools: bool = True
    external_max_results: int = 5
    external_fetch_timeout_seconds: float = 10.0
    enable_browser_tools: bool = False
    browser_allowed_domains: list[str] = field(
        default_factory=lambda: [
            item.strip()
            for item in os.getenv("FAQ_BROWSER_ALLOWED_DOMAINS", "").split(",")
            if item.strip()
        ]
    )
    enable_safety_guard: bool = True


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
        self.__rag_service: Any = None
        self.__sql_service: Any = None
        self.__chart_service: Any = None
        self.__suggestion_service: Any = None
        self.__weather_service: Any = None
        self.__web_search_skill: Any = None
        self.__browser_skill: Any = None
        self.__safety_guard: Any = None
        self.__design_intent_agent: Any = None
        self.__design_prompt_agent: Any = None
        self.__design_workflow_agent: Any = None

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
        raw_context = input_data.context if isinstance(input_data.context, dict) else {}
        effective_question = self._resolve_question_with_conversation_context(question, raw_context)
        execution_report = self._new_execution_report(question)
        self._append_report_phase(execution_report, "load", "ok", {"agent": self.name})

        if not question:
            self._append_report_phase(execution_report, "start", "error", {"reason": "empty_question"})
            return FAQOutput(
                error="質問が指定されていません",
                execution_report=self._finish_execution_report(
                    execution_report,
                    status="error",
                    error_message="質問が指定されていません",
                ),
                a2a=self._build_a2a_metadata("faq"),
            )

        violation = await self._check_safety_violation(question)
        if violation:
            blocked_response = self._build_blocked_response(question, violation)
            self._append_report_phase(
                execution_report,
                "safety",
                "blocked",
                {"reason": violation},
            )
            blocked_response.execution_report = self._finish_execution_report(
                execution_report,
                status="blocked",
                error_message=blocked_response.error,
            )
            blocked_response.a2a = self._build_a2a_metadata("blocked")
            return blocked_response

        # 意図ルーターでクエリタイプを判定
        query_type = await self._route_query(effective_question, context=raw_context)
        self._append_report_phase(execution_report, "start", "ok", {"query_type": query_type})

        try:
            if query_type == "chat":
                self._append_report_phase(execution_report, "call", "ok", {"service": "llm_direct"})
                response = await self._handle_chat_query(effective_question, query_type)
            elif query_type == "weather":
                self._append_report_phase(execution_report, "call", "ok", {"service": "weather_api"})
                response = await self._handle_weather_query(effective_question, query_type)
            elif query_type == "external":
                self._append_report_phase(execution_report, "call", "ok", {"service": "external_tools"})
                response = await self._handle_external_query(effective_question, query_type, raw_context)
            elif query_type == "hybrid":
                self._append_report_phase(execution_report, "call", "ok", {"service": "rag+text2sql"})
                response = await self._handle_hybrid_query(effective_question, query_type)
            elif query_type == "sql":
                self._append_report_phase(execution_report, "call", "ok", {"service": "text2sql"})
                response = await self._handle_sql_query(effective_question, query_type)
            elif query_type == "sales_material":
                self._append_report_phase(execution_report, "call", "ok", {"service": "design_skills"})
                response = await self._handle_sales_material_query(effective_question, query_type)
            else:
                self._append_report_phase(execution_report, "call", "ok", {"service": "rag"})
                response = await self._handle_faq_query(effective_question, query_type)

            self._append_report_phase(
                execution_report,
                "execute",
                "ok",
                {
                    "query_type": query_type,
                    "artifacts": len(response.artifacts),
                    "has_chart": response.chart is not None,
                },
            )
            # API 出力上の question は常に最新ユーザー入力を返す
            response.question = question
            response.execution_report = self._finish_execution_report(
                execution_report,
                status="success",
            )
            response.a2a = self._build_a2a_metadata(query_type)
            return response

        except Exception as e:
            self._logger.exception(f"FAQAgent実行エラー: {e}")
            self._append_report_phase(
                execution_report,
                "execute",
                "error",
                {"error": str(e)},
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=f"申し訳ありません。エラーが発生しました: {e}",
                error=str(e),
                execution_report=self._finish_execution_report(
                    execution_report,
                    status="error",
                    error_message=str(e),
                ),
                a2a=self._build_a2a_metadata(query_type),
            )

    async def _handle_chat_query(self, question: str, query_type: str) -> FAQOutput:
        """チャット・日常会話クエリを処理（LLM直接回答、RAG不要）."""
        chat_prompt = (
            f"{self.SYSTEM_PROMPT}\n\n"
            "ユーザーからの挨拶や日常的な質問には、共感を示しつつ客観的に回答してください。\n"
            "事実と推測を明確に分け、断定できない内容は『不確実』と明示してください。\n"
            "回答言語は必ずユーザーの入力言語に合わせてください。\n"
            "ナレッジベース検索は不要です。あなた自身の知識で簡潔に回答してください。\n\n"
            f"ユーザー: {question}"
        )
        answer = await self._call_llm(chat_prompt)
        if not answer:
            answer = self._localized_message(
                question,
                ja="こんにちは。何をお手伝いしましょうか？",
                zh="你好，我可以帮你做什么？",
                en="Hello, how can I help you?",
            )

        suggestions = await self._generate_suggestions(question, query_type, True)

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=answer,
            documents=[],
            rich_response=self._build_chat_rich_response(answer).to_dict(),
            suggestions=suggestions,
        )

    async def _handle_faq_query(self, question: str, query_type: str) -> FAQOutput:
        """FAQ クエリを処理."""
        if self.__rag_service is None:
            return await self._handle_chat_query(question, "chat")
        expanded_question = self._expand_faq_query_multilingual(question)
        rag_result = await self.__rag_service.execute(
            action="query",
            question=expanded_question,
        )
        if not rag_result.success:
            fallback_error = rag_result.error_message or self._localized_message(
                question,
                ja="RAG 検索に失敗しました。",
                zh="RAG 检索失败。",
                en="RAG search failed.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=fallback_error,
                documents=[],
                rich_response=self._build_chat_rich_response(fallback_error).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error=rag_result.error_message or "rag_error",
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
        verification = self._verify_faq_answer(
            answer=str(rag_result.data.get("answer", "")),
            documents=documents,
        )
        answer_text = str(rag_result.data.get("answer", "")).strip()
        if verification.get("status") != "supported":
            warning_text = self._localized_message(
                question,
                ja="注意: 回答の根拠が弱いため、質問を具体化するか DB 分析ルートの利用を推奨します。",
                zh="注意: 当前回答证据较弱，建议补充更具体的问题，或改走数据库分析。",
                en="Note: Evidence is weak. Please provide a more specific question or use DB analytics.",
            )
            answer_text = (
                f"{answer_text}\n\n"
                f"{warning_text}"
            ).strip()

        suggestions = await self._generate_suggestions(question, query_type, bool(documents))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=answer_text,
            documents=documents,
            rich_response=self._build_faq_rich_response(
                answer=answer_text,
                documents=documents,
            ).to_dict(),
            suggestions=suggestions,
            verification=verification,
            error="" if verification.get("status") == "supported" else "faq_evidence_weak",
        )

    async def _handle_weather_query(self, question: str, query_type: str) -> FAQOutput:
        """天気クエリを処理（Open-Meteo API 経由）."""
        city = self._extract_city_for_weather(question)
        if not city:
            ask_city = self._localized_message(
                question,
                ja="都市名を教えてください。例: 東京の天気 / weather in Tokyo",
                zh="请告诉我城市名，例如：北京天气、东京の天気、weather in Tokyo。",
                en="Please tell me the city name, e.g. weather in Tokyo.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=ask_city,
                data=[],
                rich_response=self._build_chat_rich_response(ask_city).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error="city_required",
            )

        if self.__weather_service is None:
            fallback = self._localized_message(
                question,
                ja="天気サービスは現在利用できません。しばらくして再試行してください。",
                zh="天气服务未启用，请稍后再试。",
                en="Weather service is not available right now. Please try again later.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=fallback,
                data=[],
                rich_response=self._build_chat_rich_response(fallback).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error="weather_api_unavailable",
            )

        weather_result = await self.__weather_service.execute(
            action="forecast",
            city=city,
            days=self._config.weather_default_days,
        )
        if not weather_result.success:
            error_code = weather_result.error_code or "weather_api_unavailable"
            if error_code == "city_not_found":
                answer = self._localized_message(
                    question,
                    ja=f"都市「{city}」が見つかりません。別の表記で試してください。",
                    zh=f"未找到城市「{city}」，请换个城市名再试。",
                    en=f"City '{city}' was not found. Please try another spelling.",
                )
            elif error_code == "city_required":
                answer = self._localized_message(
                    question,
                    ja="都市名を指定してから再度問い合わせてください。",
                    zh="请告诉我城市名后再查询天气。",
                    en="Please provide a city name and try again.",
                )
            elif error_code == "weather_parse_error":
                answer = self._localized_message(
                    question,
                    ja="天気データの解析に失敗しました。しばらくして再試行してください。",
                    zh="天气数据解析失败，请稍后再试。",
                    en="Failed to parse weather data. Please try again later.",
                )
            else:
                answer = weather_result.error_message or self._localized_message(
                    question,
                    ja="天気サービスが一時的に利用できません。しばらくして再試行してください。",
                    zh="天气服务暂时不可用，请稍后再试。",
                    en="Weather API is temporarily unavailable. Please try again later.",
                )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=answer,
                data=[],
                rich_response=self._build_chat_rich_response(answer).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error=error_code,
            )

        weather_data = weather_result.data if isinstance(weather_result.data, dict) else {}
        answer = self._format_weather_answer(weather_data, question)
        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=answer,
            data=[weather_data] if weather_data else [],
            rich_response=self._build_chat_rich_response(answer).to_dict(),
            suggestions=await self._generate_suggestions(question, query_type, True),
        )

    async def _handle_external_query(
        self,
        question: str,
        query_type: str,
        context: dict[str, Any],
    ) -> FAQOutput:
        """外部 API / 検索 / ブラウザ取得を処理."""
        user_lang = self._detect_language(question)
        use_browser = self._should_use_browser(question)
        url = self._extract_first_url(question)

        if use_browser and url:
            browser_result = await self._try_browser_fetch(url)
            if browser_result is not None:
                answer_text = browser_result.get("answer", "")
                docs = [
                    DocumentSchema(
                        id=str(browser_result.get("url", "")),
                        content=str(browser_result.get("content_preview", "")),
                        source=str(browser_result.get("url", "")),
                        score=1.0,
                    )
                ]
                return FAQOutput(
                    question=question,
                    query_type=query_type,
                    answer=answer_text,
                    documents=docs,
                    data=[browser_result],
                    rich_response=self._build_faq_rich_response(answer_text, docs).to_dict(),
                    suggestions=await self._generate_suggestions(question, query_type, True),
                    verification={"status": "supported", "source_count": 1, "mode": "browser"},
                )

        if self.__web_search_skill is None:
            message = self._localized_message(
                question,
                ja="外部検索機能が無効化されています。",
                zh="外部搜索功能未启用。",
                en="External search is disabled.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=message,
                rich_response=self._build_chat_rich_response(message).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error="external_tools_disabled",
            )

        search_query = self._extract_search_query(question, context)
        try:
            summary = await self.__web_search_skill.search_and_summarize(
                query=search_query,
                question=question,
                llm_client=None,
            )
        except Exception as exc:
            self._logger.warning("External search failed: %s", exc)
            message = self._localized_message(
                question,
                ja="外部検索に失敗しました。しばらくしてから再試行してください。",
                zh="外部搜索失败，请稍后重试。",
                en="External search failed. Please retry later.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=message,
                rich_response=self._build_chat_rich_response(message).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error="external_search_error",
            )
        sources = summary.sources[: self._config.external_max_results]
        docs: list[DocumentSchema] = []
        source_links: list[str] = []
        for index, result in enumerate(sources):
            if isinstance(result, dict):
                url = str(result.get("url", ""))
                snippet = str(result.get("snippet", ""))
            else:
                url = str(getattr(result, "url", ""))
                snippet = str(getattr(result, "snippet", ""))
            if not url:
                continue
            docs.append(
                DocumentSchema(
                    id=url,
                    content=snippet,
                    source=url,
                    score=max(0.1, 1.0 - (index * 0.1)),
                )
            )
            source_links.append(url)
        answer_text = summary.answer.strip() or self._localized_message(
            question,
            ja="信頼できる外部情報を取得できませんでした。",
            zh="未找到可靠的外部信息。",
            en="No reliable external information was found.",
        )
        if source_links:
            source_title = "参照元" if user_lang == "ja" else ("来源" if user_lang == "zh" else "Sources")
            answer_text += f"\n\n{source_title}:\n" + "\n".join(f"- {item}" for item in source_links[:3])

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=answer_text,
            documents=docs,
            rich_response=self._build_faq_rich_response(answer_text, docs).to_dict(),
            suggestions=await self._generate_suggestions(question, query_type, bool(source_links)),
            verification={"status": "supported" if source_links else "insufficient", "source_count": len(source_links)},
            error="" if source_links else "external_not_found",
        )

    async def _handle_sql_query(self, question: str, query_type: str) -> FAQOutput:
        """SQL クエリを処理."""
        if self.__sql_service is None:
            return await self._handle_faq_query(question, "faq")
        sql_result = await self.__sql_service.execute(
            action="query",
            question=question,
        )
        if not sql_result.success:
            sql_error_message = sql_result.error_message or self._localized_message(
                question,
                ja="データ分析に失敗しました。",
                zh="数据分析失败。",
                en="Data analytics failed.",
            )
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=sql_error_message,
                rich_response=self._build_chat_rich_response(sql_error_message).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error=sql_result.error_message or "sql_error",
            )

        sql_payload = sql_result.data if isinstance(sql_result.data, dict) else {}
        data_rows = sql_payload.get("data")
        rows = data_rows if isinstance(data_rows, list) else []
        columns_raw = sql_payload.get("columns")
        columns = [str(col) for col in columns_raw] if isinstance(columns_raw, list) else []
        chart = await self._resolve_chart_from_sql_payload(question, sql_payload, rows, columns)
        methodology = self._build_db_methodology(question, rows, columns)
        base_answer = str(sql_payload.get("answer", "")).strip() or self._localized_message(
            question,
            ja="データベースクエリを実行しました。",
            zh="已完成数据库查询。",
            en="Database query has been executed.",
        )
        final_answer = f"{base_answer}\n\n{methodology}".strip()
        suggestions = await self._generate_suggestions(question, query_type, bool(rows))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=final_answer,
            sql=str(sql_payload.get("sql", "")),
            data=rows,
            columns=columns,
            chart=chart,
            rich_response=self._build_sql_rich_response(
                answer=final_answer,
                sql=str(sql_payload.get("sql", "")),
                data=rows,
            ).to_dict(),
            suggestions=suggestions,
            verification={"status": "supported" if rows else "insufficient", "source_count": len(rows)},
        )

    async def _handle_hybrid_query(self, question: str, query_type: str) -> FAQOutput:
        """RAG + SQL の双通道協調処理."""
        import asyncio

        rag_available = self.__rag_service is not None
        sql_available = self.__sql_service is not None

        if not rag_available and not sql_available:
            return await self._handle_chat_query(question, "chat")
        if not rag_available:
            return await self._handle_sql_query(question, "sql")
        if not sql_available:
            return await self._handle_faq_query(question, "faq")

        expanded_question = self._expand_faq_query_multilingual(question)
        rag_task = self.__rag_service.execute(action="query", question=expanded_question)
        sql_task = self.__sql_service.execute(action="query", question=question)
        rag_result, sql_result = await asyncio.gather(rag_task, sql_task, return_exceptions=True)

        rag_error = isinstance(rag_result, Exception) or (hasattr(rag_result, "success") and not rag_result.success)
        sql_error = isinstance(sql_result, Exception) or (hasattr(sql_result, "success") and not sql_result.success)

        if rag_error and sql_error:
            return await self._handle_chat_query(question, "chat")
        if rag_error and not sql_error:
            return await self._handle_sql_query(question, "sql")
        if sql_error and not rag_error:
            return await self._handle_faq_query(question, "faq")

        rag_service_result = rag_result
        sql_service_result = sql_result

        documents = [
            DocumentSchema(
                id=d.get("id", ""),
                content=d.get("content", ""),
                source=d.get("source", ""),
                score=d.get("score", 0.0),
            )
            for d in rag_service_result.data.get("documents", [])
            if isinstance(d, dict)
        ]

        sql_data = sql_service_result.data.get("data", [])
        sql_columns = sql_service_result.data.get("columns", [])
        sql_text = sql_service_result.data.get("sql", "")
        sql_answer = str(sql_service_result.data.get("answer", "")).strip()
        rag_answer = str(rag_service_result.data.get("answer", "")).strip()
        combined_answer_parts = [part for part in [rag_answer, sql_answer] if part]
        combined_answer = "\n\n".join(combined_answer_parts) if combined_answer_parts else "回答を生成できませんでした。"
        sql_rows = sql_data if isinstance(sql_data, list) else []
        sql_columns_safe = [str(col) for col in sql_columns] if isinstance(sql_columns, list) else []
        chart = await self._resolve_chart_from_sql_payload(
            question=question,
            sql_payload=sql_service_result.data if isinstance(sql_service_result.data, dict) else {},
            rows=sql_rows,
            columns=sql_columns_safe,
        )
        methodology = self._build_db_methodology(question, sql_rows, sql_columns_safe)
        combined_answer = f"{combined_answer}\n\n{methodology}".strip()

        rich_response = self._build_sql_rich_response(
            answer=combined_answer,
            sql=sql_text if isinstance(sql_text, str) else "",
            data=sql_rows,
        ).to_dict()
        suggestions = await self._generate_suggestions(question, query_type, bool(documents or sql_data))
        verification = self._verify_faq_answer(
            answer=rag_answer,
            documents=documents,
        )

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=combined_answer,
            documents=documents,
            sql=sql_text if isinstance(sql_text, str) else "",
            data=sql_rows,
            columns=sql_columns_safe,
            chart=chart,
            rich_response=rich_response,
            suggestions=suggestions,
            verification=verification,
        )

    async def _handle_sales_material_query(self, question: str, query_type: str) -> FAQOutput:
        """営業資料向け画像生成を処理."""
        if not self._config.enable_sales_material:
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer="営業資料画像生成機能は無効化されています。",
                error="sales_material_disabled",
            )

        parsed = self._parse_sales_material_request(question)
        request_id = f"mat-{uuid.uuid4().hex[:10]}"
        output_dir = Path(self._config.sales_material_output_root) / request_id
        output_dir.mkdir(parents=True, exist_ok=True)

        intent_result = await self.__design_intent_agent.run(
            {
                "brief": parsed["brief"],
                "num_images": parsed["num_images"],
                "target_platform": parsed["target_platform"],
                "style_preferences": parsed["style_preferences"],
            }
        )
        prompt_plan_result = await self.__design_prompt_agent.run(
            {
                "intent": intent_result,
                "brand_colors": parsed["brand_colors"],
                "aspect_ratio": parsed["aspect_ratio"],
                "reference_image_paths": [],
            }
        )
        workflow_result = await self.__design_workflow_agent.run(
            {
                "prompt_plan": prompt_plan_result,
                "output_directory": str(output_dir),
                "save_locally": True,
            }
        )

        artifacts = self._extract_material_artifacts(workflow_result, request_id)
        errors = workflow_result.get("errors", [])
        generated_count = len(artifacts)

        if generated_count == 0 and errors:
            answer = "営業資料画像の生成に失敗しました。 ComfyUI起動状態または OPENAI_API_KEY を確認してください。"
        else:
            answer = f"営業資料向け画像を {generated_count} 枚生成しました。"
            if errors:
                answer += f" 一部失敗: {len(errors)} 件。"

        suggestions = await self._generate_suggestions(question, query_type, generated_count > 0)

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=answer,
            data=artifacts,
            artifacts=artifacts,
            rich_response=self._build_sales_material_rich_response(
                brief=parsed["brief"],
                target_platform=parsed["target_platform"],
                artifacts=artifacts,
                errors=errors,
                output_directory=str(output_dir),
            ).to_dict(),
            suggestions=suggestions,
            error="" if generated_count > 0 else ("; ".join(errors) if errors else ""),
        )

    def _build_chat_rich_response(self, answer: str) -> RichResponse:
        """チャット向け A2UI レスポンスを構築（引用なし）."""
        response = RichResponse()
        response.add_markdown(answer or "こんにちは！何かお手伝いできることはありますか？")
        return response

    def _build_faq_rich_response(
        self,
        answer: str,
        documents: list[DocumentSchema],
    ) -> RichResponse:
        """FAQ向け A2UI レスポンスを構築."""
        response = RichResponse()
        response.add_markdown(answer or "該当情報は見つかりませんでした。")

        if documents:
            citations = [
                {
                    "id": doc.id,
                    "title": doc.source or f"doc-{index + 1}",
                    "snippet": doc.content[:180],
                    "score": doc.score,
                }
                for index, doc in enumerate(documents[:5])
            ]
            response.add_markdown("### 参照ソース")
            response.add_citations(citations)
        else:
            response.add_alert(
                "ナレッジベースで関連ドキュメントを検出できませんでした。",
                AlertType.WARNING,
                title="参照情報",
            )
        return response

    def _build_sql_rich_response(
        self,
        answer: str,
        sql: str,
        data: list[dict[str, Any]],
    ) -> RichResponse:
        """SQL向け A2UI レスポンスを構築."""
        response = RichResponse()
        response.add_markdown(answer or "SQL実行結果を要約できませんでした。")

        if sql:
            response.add_code(sql, language="sql", title="生成SQL")
        if data:
            response.add_table(data, title="クエリ結果")
            keys = list(data[0].keys())
            numeric_key = next(
                (key for key in keys if isinstance(data[0].get(key), (int, float))),
                None,
            )
            x_key = next((key for key in keys if key != numeric_key), keys[0])
            if numeric_key:
                response.add_chart_from_data(
                    data=data,
                    x_key=x_key,
                    y_key=numeric_key,
                    chart_type=ChartType.BAR,
                    title="集計チャート",
                )
        return response

    def _build_sales_material_rich_response(
        self,
        brief: str,
        target_platform: str,
        artifacts: list[dict[str, Any]],
        errors: list[str],
        output_directory: str,
    ) -> RichResponse:
        """営業資料画像向け A2UI レスポンスを構築."""
        response = RichResponse()
        response.add_markdown(
            "## 営業資料画像生成結果\n"
            f"- ブリーフ: {brief}\n"
            f"- ターゲット: {target_platform or '汎用'}\n"
            f"- 生成枚数: {len(artifacts)}\n"
            f"- 出力先: `{output_directory}`"
        )

        if artifacts:
            response.add_table(
                artifacts,
                title="生成アセット一覧",
                columns=[
                    {"key": "artifact_id", "label": "artifact_id"},
                    {"key": "role", "label": "role"},
                    {"key": "file_name", "label": "file_name"},
                    {"key": "size_bytes", "label": "size_bytes"},
                    {"key": "download_url", "label": "download_url"},
                ],
            )
            for artifact in artifacts:
                response.add_link(
                    text=f"{artifact['file_name']} をダウンロード",
                    url=str(artifact["download_url"]),
                    external=False,
                )
        else:
            response.add_alert(
                "画像を生成できませんでした。ComfyUI もしくは OpenAI フォールバックを確認してください。",
                AlertType.ERROR,
                title="画像生成失敗",
            )

        if errors:
            response.add_markdown("### エラー詳細")
            for error in errors[:5]:
                response.add_alert(error, AlertType.WARNING)
        return response

    async def _generate_suggestions(self, question: str, query_type: str, data_found: bool) -> list[SuggestionSchema]:
        """フォローアップ提案を生成."""
        if query_type == "sales_material":
            return [
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="16:9スライド比率で再生成",
                        zh="按 16:9 幻灯片比例重新生成",
                        en="Regenerate in 16:9 slide ratio",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="ブランドカラー指定で再生成",
                        zh="指定品牌色重新生成",
                        en="Regenerate with brand colors",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="生成画像から提案文を作成",
                        zh="基于生成图片写销售提案文案",
                        en="Create sales copy from generated images",
                    ),
                    type="followup",
                ),
            ]
        if query_type == "weather":
            return [
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="東京の明日の天気",
                        zh="东京明天的天气",
                        en="Tomorrow's weather in Tokyo",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="大阪の3日間予報",
                        zh="大阪未来三天天气",
                        en="3-day forecast for Osaka",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="名古屋は今日雨ですか",
                        zh="名古屋今天会下雨吗",
                        en="Will it rain in Nagoya today?",
                    ),
                    type="followup",
                ),
            ]
        if query_type == "blocked":
            return []
        if query_type == "external":
            return [
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="引用したソースURLを提示",
                        zh="给出引用来源链接",
                        en="Show the cited source URLs",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="直近24時間の更新を再検索",
                        zh="继续搜索最近24小时更新",
                        en="Search for updates in the last 24 hours",
                    ),
                    type="followup",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="複数ソースの差分比較",
                        zh="对比多个来源的差异",
                        en="Compare differences across sources",
                    ),
                    type="followup",
                ),
            ]
        if query_type in {"sql", "hybrid"} and data_found:
            base_suggestions = [
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="地域別にドリルダウン",
                        zh="按地区维度继续下钻",
                        en="Drill down by region",
                    ),
                    type="drill_down",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="前年比・前月比を追加",
                        zh="补充同比/环比变化",
                        en="Add YoY and MoM comparison",
                    ),
                    type="comparison",
                ),
                SuggestionSchema(
                    text=self._localized_message(
                        question,
                        ja="実行可能な改善計画を作成",
                        zh="输出可执行改善计划",
                        en="Generate an actionable improvement plan",
                    ),
                    type="action_plan",
                ),
            ]
        else:
            base_suggestions = []

        try:
            suggestion_result = await self.__suggestion_service.execute(
                question=question,
                context={"query_type": query_type, "data_found": data_found},
            )
            generated = [
                SuggestionSchema(text=s.get("text", ""), type=s.get("type", "followup"))
                for s in suggestion_result.data.get("suggestions", [])
            ]
            return (base_suggestions + generated)[: self._config.max_suggestions]
        except Exception as e:
            self._logger.warning(f"提案生成エラー: {e}")
            return base_suggestions

    async def _ensure_services(self) -> None:
        """サービスの遅延初期化（私有メソッド）."""
        if self._services_initialized:
            return

        from agentflow.security.ai_safety_guard import AISafetyGuard, GuardConfig
        from agentflow.services import (
            ChartConfig,
            ChartService,
            ChunkStrategy,
            RAGConfig,
            RAGService,
            RerankerType,
            SQLDialect,
            SuggestionConfig,
            SuggestionService,
            Text2SQLConfig,
            Text2SQLService,
            WeatherConfig,
            WeatherService,
        )
        from agentflow.skills.browser import BrowserSkill, BrowserSkillConfig
        from agentflow.skills.builtin.design_skills.agents.intent_analyzer_agent import (
            IntentAnalyzerAgent,
        )
        from agentflow.skills.builtin.design_skills.agents.prompt_planner_agent import (
            PromptPlannerAgent,
        )
        from agentflow.skills.builtin.design_skills.agents.workflow_executor_agent import (
            WorkflowExecutorAgent,
        )
        from agentflow.skills.builtin.web_search import SearchConfig, WebSearchSkill

        # RAGサービス
        if self._config.enable_rag:
            try:
                chunk_strategy = ChunkStrategy(self._config.rag_chunk_strategy)
            except ValueError:
                chunk_strategy = ChunkStrategy.RECURSIVE

            try:
                reranker = RerankerType(self._config.rag_reranker)
            except ValueError:
                reranker = RerankerType.BM25

            self.__rag_service = RAGService(
                RAGConfig(
                    collection=self._config.rag_collection,
                    chunk_strategy=chunk_strategy,
                    reranker=reranker,
                    top_k=self._config.rag_top_k,
                )
            )
        else:
            self.__rag_service = None

        try:
            dialect = SQLDialect(self._config.sql_dialect)
        except ValueError:
            dialect = SQLDialect.POSTGRESQL

        # Text2SQLサービス
        if self._config.enable_sql:
            self.__sql_service = Text2SQLService(
                Text2SQLConfig(
                    schema=self._config.sql_schema,
                    dialect=dialect,
                    auto_chart=self._config.auto_chart,
                )
            )
        else:
            self.__sql_service = None

        # Chartサービス
        self.__chart_service = ChartService(ChartConfig())

        # Suggestionサービス
        self.__suggestion_service = SuggestionService(SuggestionConfig(max_suggestions=self._config.max_suggestions))
        if self._config.enable_weather:
            self.__weather_service = WeatherService(
                WeatherConfig(default_days=self._config.weather_default_days),
            )
        else:
            self.__weather_service = None

        if self._config.enable_external_tools:
            self.__web_search_skill = WebSearchSkill(
                SearchConfig(
                    max_results=self._config.external_max_results,
                    timeout=self._config.external_fetch_timeout_seconds,
                )
            )
        else:
            self.__web_search_skill = None

        if self._config.enable_browser_tools:
            self.__browser_skill = BrowserSkill(
                BrowserSkillConfig(
                    domain_whitelist=self._config.browser_allowed_domains,
                    headless=True,
                )
            )
        else:
            self.__browser_skill = None

        if self._config.enable_safety_guard:
            self.__safety_guard = AISafetyGuard(config=GuardConfig(strict_mode=True))
        else:
            self.__safety_guard = None

        self.__design_intent_agent = IntentAnalyzerAgent(llm_client=self._llm)
        self.__design_prompt_agent = PromptPlannerAgent(llm_client=self._llm)
        self.__design_workflow_agent = WorkflowExecutorAgent(llm_client=self._llm)

        self._services_initialized = True

    # LLM 意図路由器提示词（语义判断优先，不依赖固定关键词）
    _INTENT_PROMPT_TEMPLATE = (
        "You are IntentRouterAgent for an enterprise assistant.\n"
        "Select exactly one route for downstream sub-agent execution based on user intent and required data source.\n"
        "Do not classify by shallow keyword matching; infer the real task objective.\n\n"
        "Route definitions:\n"
        "- chat: general conversation, no enterprise KB/DB lookup needed\n"
        "- faq: enterprise policy/procedure knowledge lookup via RAG knowledge base\n"
        "- sql: structured analytics/aggregation from business database only\n"
        "- hybrid: needs BOTH KB context and DB analytics in one answer\n"
        "- sales_material: generate sales materials/images/deck assets\n"
        "- weather: weather/forecast query for a city\n"
        "- external: web search/external API/url browsing outside internal data\n"
        "- blocked: harmful/illegal request that should be refused\n"
        "- unclear: intent is ambiguous or insufficient for confident routing\n\n"
        "Capability availability:\n"
        "- rag_enabled: {rag_enabled}\n"
        "- sql_enabled: {sql_enabled}\n"
        "- hybrid_enabled: {hybrid_enabled}\n"
        "- sales_material_enabled: {sales_material_enabled}\n"
        "- weather_enabled: {weather_enabled}\n"
        "- external_enabled: {external_enabled}\n\n"
        "Conversation context (latest turns):\n"
        "{history_block}\n\n"
        "Optional route hint from caller: {route_hint}\n\n"
        'Return JSON only: {{"route":"chat|faq|sql|hybrid|sales_material|weather|external|blocked|unclear","confidence":0.0-1.0,"reason":"short"}}\n'
        "User message:\n"
        "{question}"
    )
    _INTENT_CONFIDENCE_THRESHOLD = 0.45

    _VALID_QUERY_TYPES = frozenset(
        {"chat", "sales_material", "sql", "faq", "hybrid", "weather", "external", "blocked", "unclear"}
    )

    async def _classify_query(
        self,
        question: str,
        context: dict[str, Any] | None = None,
    ) -> str:
        """互換API: 意図分類を実行してルートを返す.

        既存テスト/呼び出し互換のため残し、内部は _route_query を利用する。
        """
        return await self._route_query(question, context=context)

    def _resolve_question_with_conversation_context(self, question: str, context: dict[str, Any]) -> str:
        """会話履歴をもとに省略質問を補完する.

        特に天気問い合わせで「東京」のような追加入力を受けた場合に、
        前ターン文脈を利用して「東京の天気」へ補完する。
        """
        current = re.sub(r"\s+", " ", question).strip()
        if not current:
            return question

        history = self._extract_conversation_history(context)
        if not history:
            return question

        if self._is_weather_followup(current, history):
            return f"{current}の天気"

        # 一般フォローアップ（短文）には直前ユーザー文を添えて解釈を安定化
        if len(current) <= 24:
            previous_user = self._get_latest_message_by_role(history, role="user")
            if previous_user:
                return f"前の質問: {previous_user}\n追加入力: {current}"

        return question

    def _extract_conversation_history(self, context: dict[str, Any]) -> list[dict[str, str]]:
        """context から正規化済み会話履歴を抽出する."""
        raw = context.get("conversation_history")
        if not isinstance(raw, list):
            return []

        normalized: list[dict[str, str]] = []
        for item in raw:
            if not isinstance(item, dict):
                continue
            role_raw = item.get("role")
            content_raw = item.get("content")
            if role_raw not in {"user", "assistant"}:
                continue
            if not isinstance(content_raw, str):
                continue
            content = re.sub(r"\s+", " ", content_raw).strip()
            if not content:
                continue
            normalized.append(
                {
                    "role": str(role_raw),
                    "content": content,
                }
            )

        return normalized[-8:]

    @staticmethod
    def _get_latest_message_by_role(history: list[dict[str, str]], *, role: str) -> str:
        """指定ロールの最新メッセージ本文を返す."""
        for item in reversed(history):
            if item.get("role") == role:
                content = item.get("content", "").strip()
                if content:
                    return content
        return ""

    def _is_weather_followup(self, current: str, history: list[dict[str, str]]) -> bool:
        """天気問い合わせの追加入力か判定する."""
        if self._contains_weather_keyword(current):
            return False
        if not self._looks_like_city_phrase(current):
            return False

        previous_user = self._get_latest_message_by_role(history, role="user")
        previous_assistant = self._get_latest_message_by_role(history, role="assistant")
        return self._contains_weather_keyword(previous_user) or self._contains_weather_keyword(previous_assistant)

    @staticmethod
    def _contains_weather_keyword(text: str) -> bool:
        """文字列に天気意図キーワードが含まれるか判定."""
        value = text.lower()
        weather_keywords = (
            "weather",
            "forecast",
            "temperature",
            "天气",
            "气温",
            "预报",
            "天気",
            "気温",
            "予報",
        )
        return any(keyword in value for keyword in weather_keywords)

    @staticmethod
    def _looks_like_city_phrase(text: str) -> bool:
        """都市名らしい短文かを判定する."""
        normalized = text.strip(" \t\r\n。．，、！？!?：:;；")
        if not normalized or len(normalized) > 40:
            return False
        if not re.fullmatch(r"[A-Za-z\u3040-\u30ff\u4e00-\u9fff\s\-'・ー]+", normalized):
            return False
        disallowed = {
            "ありがとう",
            "お願いします",
            "おねがいします",
            "こたえて",
            "回答して",
            "はい",
            "いいえ",
            "yes",
            "no",
            "ok",
            "okay",
        }
        return normalized.lower() not in disallowed

    async def _route_query(
        self,
        question: str,
        *,
        context: dict[str, Any] | None = None,
    ) -> str:
        """意図ルーターでクエリルートを決定.

        Args:
            question: 質問文

        Returns:
            "chat" | "sales_material" | "sql" | "faq" | "hybrid" | "weather" | "external" | "blocked"
        """
        normalized_context = context if isinstance(context, dict) else {}
        llm_decision = await self._route_query_with_intent_llm(question, normalized_context)
        if llm_decision is not None:
            return self._apply_route_policy(llm_decision.route)

        # LLM が使えない場合のみ非キーワード型の保守的デフォルトへフォールバック
        fallback_route = self._default_route_without_keyword_rules(normalized_context)
        return self._apply_route_policy(fallback_route)

    async def _route_query_with_intent_llm(
        self,
        question: str,
        context: dict[str, Any],
    ) -> IntentRouteDecision | None:
        """LLM 意図路由器でルートを決定する."""
        if self._llm is None:
            self._logger.warning("Intent routing fallback: LLM client is unavailable")
            return None

        try:
            prompt = self._build_intent_router_prompt(question, context)
            raw = await self._call_llm(prompt)
            decision = self._parse_intent_router_response(raw)
            if decision.route not in self._VALID_QUERY_TYPES:
                self._logger.warning("Intent routing received invalid route: %r", raw)
                return None
            if decision.confidence < self._INTENT_CONFIDENCE_THRESHOLD:
                self._logger.info(
                    "Intent routing confidence too low: route=%s confidence=%.2f reason=%s",
                    decision.route,
                    decision.confidence,
                    decision.reason,
                )
                return IntentRouteDecision(route="unclear", confidence=decision.confidence, reason=decision.reason)
            return decision
        except Exception as exc:
            self._logger.warning("Intent routing via LLM failed: %s", exc)
            return None

    def _build_intent_router_prompt(self, question: str, context: dict[str, Any]) -> str:
        """意図ルーター用の LLM プロンプトを構築する."""
        history = self._extract_conversation_history(context)
        if history:
            history_lines = [f"- {item['role']}: {item['content'][:140]}" for item in history[-6:]]
            history_block = "\n".join(history_lines)
        else:
            history_block = "- none"

        route_hint = self._extract_route_hint(context) or "none"
        hybrid_enabled = self._config.enable_hybrid and self._config.enable_rag and self._config.enable_sql

        return self._INTENT_PROMPT_TEMPLATE.format(
            rag_enabled=str(self._config.enable_rag).lower(),
            sql_enabled=str(self._config.enable_sql).lower(),
            hybrid_enabled=str(hybrid_enabled).lower(),
            sales_material_enabled=str(self._config.enable_sales_material).lower(),
            weather_enabled=str(self._config.enable_weather).lower(),
            external_enabled=str(self._config.enable_external_tools).lower(),
            history_block=history_block,
            route_hint=route_hint,
            question=question,
        )

    def _parse_intent_router_response(self, response: str) -> IntentRouteDecision:
        """LLM 応答を IntentRouteDecision に変換する."""
        text = response.strip()
        if not text:
            return IntentRouteDecision()

        payload: dict[str, Any] | None = None
        try:
            parsed = json.loads(text)
            if isinstance(parsed, dict):
                payload = parsed
        except json.JSONDecodeError:
            payload = None

        if payload is None:
            json_match = re.search(r"\{.*\}", text, re.DOTALL)
            if json_match:
                with suppress(json.JSONDecodeError):
                    parsed = json.loads(json_match.group(0))
                    if isinstance(parsed, dict):
                        payload = parsed

        if payload is not None:
            route = str(payload.get("route", "")).strip().lower()
            confidence_raw = payload.get("confidence", 0.0)
            reason = str(payload.get("reason", "")).strip()
            if isinstance(confidence_raw, (int, float)):
                confidence = float(confidence_raw)
            elif isinstance(confidence_raw, str):
                try:
                    confidence = float(confidence_raw)
                except ValueError:
                    confidence = 0.0
            else:
                confidence = 0.0
            confidence = min(max(confidence, 0.0), 1.0)
            return IntentRouteDecision(route=route or "unclear", confidence=confidence, reason=reason)

        # 非JSONレスポンス対策
        route = self._extract_route_from_intent_response(text)
        return IntentRouteDecision(route=route or "unclear", confidence=0.0, reason="unstructured_response")

    @staticmethod
    def _extract_route_hint(context: dict[str, Any]) -> str:
        """リクエスト options から route hint を抽出する."""
        options_raw = context.get("options")
        if not isinstance(options_raw, dict):
            return ""
        hint = options_raw.get("route_hint")
        if not isinstance(hint, str):
            return ""
        return hint.strip()

    def _default_route_without_keyword_rules(self, context: dict[str, Any]) -> str:
        """非キーワード型フォールバックルートを返す."""
        hinted = self._extract_route_hint(context).lower()
        if hinted in self._VALID_QUERY_TYPES:
            return hinted

        # 既定は FAQ 優先（LLM 不可時の安全側）
        if self._config.enable_rag:
            return "faq"
        if self._config.enable_sql:
            return "sql"
        return "chat"

    def _extract_route_from_intent_response(self, response: str) -> str:
        """意図ルーター応答から route を抽出."""
        text = response.strip()
        if not text:
            return ""

        try:
            payload = json.loads(text)
            if isinstance(payload, dict):
                route = str(payload.get("route", "")).strip().lower()
                if route:
                    return route
        except json.JSONDecodeError:
            pass

        json_match = re.search(r"\{.*\}", text, re.DOTALL)
        if json_match:
            try:
                payload = json.loads(json_match.group(0))
                if isinstance(payload, dict):
                    route = str(payload.get("route", "")).strip().lower()
                    if route:
                        return route
            except json.JSONDecodeError:
                pass

        first_token = text.lower().split()[0]
        return re.sub(r"[^a-z_]", "", first_token)

    def _classify_query_heuristic(self, question: str) -> str:
        """キーワードベースのフォールバック分類(LLM不可時のみ使用)."""
        question_lower = question.lower().strip()
        normalized_text = self._expand_faq_query_multilingual(question).lower()

        if self._contains_hard_block_content(question):
            return "blocked"

        sales_material_keywords = [
            "pitch deck",
            "sales deck",
            "poster",
            "flyer",
            "banner",
        ]
        if any(k in question_lower for k in sales_material_keywords):
            return "sales_material"

        weather_keywords = [
            "weather",
            "forecast",
            "temperature",
            "天气",
            "气温",
            "预报",
            "天気",
            "気温",
            "予報",
        ]
        if any(k in question_lower for k in weather_keywords):
            return "weather"

        external_keywords = [
            "联网",
            "上网",
            "外部api",
            "external api",
            "web search",
            "search web",
            "internet",
            "crawl",
            "crawler",
            "爬虫",
            "抓取",
            "browse",
            "open url",
        ]
        has_url = bool(self._extract_first_url(question))
        if any(k in question_lower for k in external_keywords) or has_url:
            return "external"

        chat_keywords = [
            "hello",
            "hi",
            "こんにちは",
            "你好",
            "thanks",
            "ありがとう",
            "你是谁",
            "who are you",
        ]
        if any(k in question_lower for k in chat_keywords):
            return "chat"

        sql_indicators = [
            "top",
            "ranking",
            "trend",
            "total",
            "average",
            "count",
            "同比",
            "环比",
            "売上",
            "顧客",
            "revenue",
            "customer",
        ]
        sql_score = sum(1 for k in sql_indicators if k in normalized_text)
        if sql_score >= 2:
            return "sql"
        if sql_score == 1:
            return "unclear"

        # LLMが無い環境ではFAQにルーティング
        return "faq"

    def _apply_route_policy(self, route: str) -> str:
        """機能有効状態を考慮して最終ルートを決定."""
        rag_available = self._config.enable_rag
        sql_available = self._config.enable_sql
        hybrid_available = self._config.enable_hybrid and rag_available and sql_available

        normalized = route.strip().lower()
        if normalized == "sales_material":
            return "sales_material" if self._config.enable_sales_material else "chat"

        if normalized == "weather":
            return "weather" if self._config.enable_weather else "chat"

        if normalized == "external":
            return "external" if self._config.enable_external_tools else "chat"

        if normalized == "blocked":
            return "blocked"

        if normalized == "chat":
            return "chat"

        if normalized in {"unclear", ""}:
            if hybrid_available:
                return "hybrid"
            if rag_available and sql_available:
                return "hybrid"
            if rag_available:
                return "faq"
            if sql_available:
                return "sql"
            return "chat"

        if normalized == "hybrid":
            if hybrid_available:
                return "hybrid"
            if rag_available and sql_available:
                return "hybrid"
            if rag_available:
                return "faq"
            if sql_available:
                return "sql"
            return "chat"

        if normalized == "faq":
            if rag_available:
                return "faq"
            return "sql" if sql_available else "chat"

        if normalized == "sql":
            if sql_available:
                return "sql"
            return "faq" if rag_available else "chat"

        return "faq" if rag_available else ("sql" if sql_available else "chat")

    def _extract_city_for_weather(self, question: str) -> str:
        """天気クエリから都市名を抽出."""
        text = question.strip()
        if not text:
            return ""

        patterns = [
            r"(?P<city>[\u4e00-\u9fffA-Za-z\u3040-\u30ff\-\s]{1,40})\s*(?:的)?天气(?:预报)?",
            r"(?P<city>[\u4e00-\u9fffA-Za-z\u3040-\u30ff\-\s]{1,40})\s*天気(?:予報)?",
            r"(?P<city>[\u4e00-\u9fffA-Za-z\u3040-\u30ff\-\s]{1,40})\s*の天気",
            r"(?:weather|forecast)\s+(?:in|for|at)\s+(?P<city>[A-Za-z][A-Za-z\s\-']{1,60})",
            r"(?P<city>[A-Za-z][A-Za-z\s\-']{1,60})\s+(?:weather|forecast)",
        ]
        for pattern in patterns:
            matched = re.search(pattern, text, flags=re.IGNORECASE)
            if not matched:
                continue
            city = self._normalize_city_candidate(matched.group("city"))
            if city:
                return city

        return self._extract_city_by_llm(text)

    def _extract_city_by_llm(self, question: str) -> str:
        """LLM 補助抽出のプレースホルダ（現段階は非有効）."""
        _ = question
        return ""

    def _normalize_city_candidate(self, candidate: str) -> str:
        """抽出候補を都市名として正規化."""
        city = candidate.strip(" \t\r\n,，。！？!?：:;；")
        city = re.sub(r"^(请问|請問|請|请|告诉我|告訴我|查一下|查询|查下)\s*", "", city, flags=re.IGNORECASE)
        city = re.sub(
            r"(今天|今日|明天|后天|現在|现在|本周|这周|這週|未来|未來|future|today|tomorrow|now|right now)$",
            "",
            city,
            flags=re.IGNORECASE,
        )
        city = re.sub(r"(的|の)$", "", city)
        city = re.sub(r"\s+", " ", city).strip()
        if not city:
            return ""

        blocked_tokens = {
            "天气",
            "天気",
            "weather",
            "forecast",
            "today",
            "tomorrow",
            "today weather",
            "今天天",
            "今天",
            "今日",
            "明天",
            "后天",
        }
        if city.lower() in blocked_tokens:
            return ""
        return city

    def _format_weather_answer(self, payload: dict[str, Any], question: str) -> str:
        """天気結果をユーザー向けテキストに整形."""
        lang = self._detect_language(question)
        city = str(payload.get("city", "")).strip()
        country = str(payload.get("country", "")).strip()
        location = ", ".join(part for part in [city, country] if part)

        current = payload.get("current")
        current_block = current if isinstance(current, dict) else {}
        current_text = str(current_block.get("weather_text", "不明")).strip() or "不明"
        temp = current_block.get("temperature_c")
        wind = current_block.get("wind_speed_kmh")

        if lang == "zh":
            header = f"{location} 当前天气：{current_text}"
            wind_label = "风速"
            forecast_title = "未来3天预报："
            line_template = "- {date}: {desc}，{t_min:.1f}°C ~ {t_max:.1f}°C"
            line_fallback = "- {date}: {desc}"
        elif lang == "en":
            header = f"{location} current weather: {current_text}"
            wind_label = "wind"
            forecast_title = "Next 3-day forecast:"
            line_template = "- {date}: {desc}, {t_min:.1f}°C ~ {t_max:.1f}°C"
            line_fallback = "- {date}: {desc}"
        else:
            header = f"{location} の現在の天気: {current_text}"
            wind_label = "風速"
            forecast_title = "今後3日間の予報:"
            line_template = "- {date}: {desc}、{t_min:.1f}°C ~ {t_max:.1f}°C"
            line_fallback = "- {date}: {desc}"
        if isinstance(temp, (int, float)):
            header += f", {temp:.1f}°C"
        if isinstance(wind, (int, float)):
            header += f", {wind_label} {wind:.1f} km/h"

        daily = payload.get("daily")
        forecasts = daily if isinstance(daily, list) else []
        if not forecasts:
            return header + ("。" if lang != "en" else ".")

        lines = [header, forecast_title]
        for day in forecasts[:3]:
            if not isinstance(day, dict):
                continue
            date = str(day.get("date", "")).strip()
            desc = str(day.get("weather_text", "不明")).strip() or "不明"
            t_max = day.get("temperature_max_c")
            t_min = day.get("temperature_min_c")
            if isinstance(t_max, (int, float)) and isinstance(t_min, (int, float)):
                lines.append(line_template.format(date=date, desc=desc, t_min=t_min, t_max=t_max))
            else:
                lines.append(line_fallback.format(date=date, desc=desc))
        return "\n".join(lines)

    def _detect_language(self, text: str) -> str:
        """入力テキストの主要言語を推定."""
        if re.search(r"[\u3040-\u30ff]", text):
            return "ja"
        if re.search(r"(请|嗎|吗|天气|预报|怎么|如何|客户|数据|分析)", text):
            return "zh"
        if re.search(r"[A-Za-z]{3,}", text) and not re.search(r"[\u4e00-\u9fff]", text):
            return "en"
        return "ja"

    def _localized_message(self, question: str, *, ja: str, zh: str, en: str) -> str:
        """質問言語に合わせた文面を返す."""
        lang = self._detect_language(question)
        if lang == "zh":
            return zh
        if lang == "en":
            return en
        return ja

    def _expand_faq_query_multilingual(self, question: str) -> str:
        """FAQ検索向けに語句分解と多言語展開を行う."""
        tokens = re.findall(r"[A-Za-z]{2,}|[\u4e00-\u9fff]{2,}|[\u3040-\u30ff]{2,}", question.lower())
        if not tokens:
            return question

        term_map: dict[str, list[str]] = {
            "有給": ["年假", "paid leave", "annual leave"],
            "年假": ["有給休暇", "paid leave", "annual leave"],
            "休暇": ["leave", "假期", "vacation policy"],
            "パスワード": ["password", "密码"],
            "password": ["パスワード", "密码"],
            "vpn": ["vpn接続", "远程接入", "remote access"],
            "返品": ["return policy", "退货", "退款"],
            "退货": ["返品", "return policy"],
            "経費": ["expense", "报销", "精算"],
            "報销": ["経費精算", "expense reimbursement"],
        }

        expanded_terms: list[str] = [question]
        seen = {question}
        for token in tokens:
            for key, values in term_map.items():
                if token in key or key in token:
                    for value in values:
                        if value not in seen:
                            expanded_terms.append(value)
                            seen.add(value)

        return " ".join(expanded_terms)

    async def _check_safety_violation(self, question: str) -> str:
        """安全ポリシー違反を判定."""
        if self._contains_hard_block_content(question):
            return "policy_violence_or_sexual"
        if self.__safety_guard is None:
            return ""
        try:
            result = await self.__safety_guard.check_input(question)
            if not result.is_safe:
                return f"safety_guard:{result.safety_level.value}"
        except Exception as exc:
            self._logger.warning("Safety guard check failed: %s", exc)
        return ""

    def _contains_hard_block_content(self, question: str) -> bool:
        """暴力・性的な禁止内容をキーワードで判定."""
        q = question.lower()
        blocked_patterns = [
            r"(?:how to|如何|怎么|怎样|教我).{0,24}(?:kill|murder|bomb|袭击|枪击|杀人|炸弹|爆炸物)",
            r"(?:制作|制造).{0,12}(?:炸弹|枪|爆炸物)",
            r"(?:how to|如何|怎么).{0,24}(?:rape|sexual assault|强奸|性侵|迷奸)",
            r"(?:未成年|儿童|child|minor).{0,10}(?:色情|性|porn|sex|nude)",
            r"(?:成人视频|色情片|pornhub|裸照|nude video)",
        ]
        return any(re.search(pattern, q, re.IGNORECASE) for pattern in blocked_patterns)

    def _build_blocked_response(self, question: str, reason: str) -> FAQOutput:
        """安全拒否レスポンスを生成."""
        message = self._localized_message(
            question,
            ja="この依頼は暴力・性的な安全ポリシー違反に該当するため対応できません。",
            zh="该请求涉及暴力或性违规内容，无法提供帮助。请更换为合规问题。",
            en="This request violates violence/sexual safety policy, so I cannot help with it.",
        )
        return FAQOutput(
            question=question,
            query_type="blocked",
            answer=message,
            rich_response=self._build_chat_rich_response(message).to_dict(),
            suggestions=[],
            error=reason,
            verification={"status": "blocked", "reason": reason},
        )

    def _verify_faq_answer(
        self,
        answer: str,
        documents: list[DocumentSchema],
    ) -> dict[str, Any]:
        """FAQ回答の根拠を自己検証."""
        if not answer.strip():
            return {"status": "insufficient", "confidence": 0.0, "reason": "empty_answer", "source_count": 0}
        if not documents:
            return {"status": "insufficient", "confidence": 0.0, "reason": "no_sources", "source_count": 0}

        scores = [max(0.0, float(doc.score)) for doc in documents]
        avg_score = sum(scores) / len(scores) if scores else 0.0
        if avg_score >= 0.45:
            status = "supported"
        elif avg_score >= 0.25:
            status = "weak"
        else:
            status = "insufficient"

        evidence = [
            {
                "source": doc.source or doc.id,
                "score": doc.score,
                "snippet": doc.content[:120],
            }
            for doc in documents[:3]
        ]
        return {
            "status": status,
            "confidence": round(min(1.0, avg_score), 3),
            "source_count": len(documents),
            "evidence": evidence,
        }

    async def _resolve_chart_from_sql_payload(
        self,
        question: str,
        sql_payload: dict[str, Any],
        rows: list[dict[str, Any]],
        columns: list[str],
    ) -> ChartSchema | None:
        """SQL結果からチャートを解決（欠損時は自動生成）."""
        chart_data = sql_payload.get("chart")
        if isinstance(chart_data, dict):
            if "type" in chart_data:
                return ChartSchema(
                    chart_type=str(chart_data.get("type", "bar")),
                    title=str(chart_data.get("title", "")),
                    data=chart_data.get("data", {}) if isinstance(chart_data.get("data"), dict) else {},
                )
            if "chart_type" in chart_data:
                body = chart_data.get("echarts")
                if not isinstance(body, dict):
                    body = chart_data.get("data")
                return ChartSchema(
                    chart_type=str(chart_data.get("chart_type", "bar")),
                    title=str(chart_data.get("title", "")),
                    data=body if isinstance(body, dict) else {},
                )

        if not rows or self.__chart_service is None:
            return None

        try:
            generated = await self.__chart_service.execute(
                action="generate",
                data=rows,
                columns=columns,
                title=question[:50],
            )
            if not generated.success or not isinstance(generated.data, dict):
                return None
            chart_body = generated.data.get("echarts")
            if not isinstance(chart_body, dict):
                chart_body = generated.data.get("data")
            return ChartSchema(
                chart_type=str(generated.data.get("chart_type", "bar")),
                title=str(generated.data.get("title", question[:30])),
                data=chart_body if isinstance(chart_body, dict) else {},
            )
        except Exception as exc:
            self._logger.warning("Chart auto-generation failed: %s", exc)
            return None

    def _build_db_methodology(
        self,
        question: str,
        rows: list[dict[str, Any]],
        columns: list[str],
    ) -> str:
        """DB分析向けの方法論（道/法/術/器）を構築."""
        lang = self._detect_language(question)
        if not rows:
            if lang == "zh":
                return (
                    "方法论（道/法/术/器）\n"
                    "- 道: 先明确业务目标与决策场景。\n"
                    "- 法: 补齐时间、地区、客户分层后再分析。\n"
                    "- 术: 增加同比/环比与异常点检测。\n"
                    "- 器: 建议建设固定 SQL 看板与预警规则。"
                )
            if lang == "en":
                return (
                    "Methodology (Principle/Framework/Tactics/Tools)\n"
                    "- Principle: Clarify the business goal and decision context.\n"
                    "- Framework: Add time/region/customer segmentation before analysis.\n"
                    "- Tactics: Add YoY/MoM and anomaly detection.\n"
                    "- Tools: Build recurring SQL dashboards and alert rules."
                )
            return (
                "方法論（道/法/術/器）\n"
                "- 道: 意思決定に直結する業務目標を先に定義します。\n"
                "- 法: 時間・地域・顧客層の軸で比較構造を作ります。\n"
                "- 術: 前年比/前月比と異常値検知を組み合わせます。\n"
                "- 器: 定常SQL・ダッシュボード・閾値アラートを整備します。"
            )

        sample = rows[0]
        numeric_cols = [col for col in columns if isinstance(sample.get(col), (int, float))]
        primary_metric = numeric_cols[0] if numeric_cols else ("core_metric" if lang == "en" else "主要指標")
        metric_values = [float(row.get(primary_metric, 0)) for row in rows if isinstance(row.get(primary_metric), (int, float))]
        metric_avg = sum(metric_values) / len(metric_values) if metric_values else 0.0
        growth_focus = any(k in question.lower() for k in ["增长", "同比", "环比", "trend", "increase"])

        if lang == "zh":
            focus = "增长" if growth_focus else "效率"
            return (
                "方法论（道/法/术/器）\n"
                f"- 道: 以“{focus}与可执行改进”为目标，围绕问题“{question[:28]}”落地。\n"
                f"- 法: 统一口径（指标={primary_metric}），再做时间/地区/客户分层对比；当前均值约 {metric_avg:.2f}。\n"
                "- 术: 先找 Top/Bottom 群组，再做异常归因，输出 2-3 条可执行实验方案。\n"
                "- 器: 固化为周报 SQL + 图表看板 + 阈值告警。"
            )
        if lang == "en":
            focus = "growth" if growth_focus else "efficiency"
            return (
                "Methodology (Principle/Framework/Tactics/Tools)\n"
                f"- Principle: Target {focus} and actionable improvement for '{question[:28]}'.\n"
                f"- Framework: Normalize metric definition ({primary_metric}) and compare by time/region/customer; current mean is {metric_avg:.2f}.\n"
                "- Tactics: Identify top/bottom segments, then run anomaly attribution and define 2-3 experiments.\n"
                "- Tools: Operationalize with recurring SQL, dashboards, and threshold alerts."
            )

        focus = "成長" if growth_focus else "効率"
        return (
            "方法論（道/法/術/器）\n"
            f"- 道: 「{focus}と実行可能な改善」を目的に、問い「{question[:28]}」に接続します。\n"
            f"- 法: 指標定義（{primary_metric}）を統一し、時間・地域・顧客層で比較します。平均値は {metric_avg:.2f} です。\n"
            "- 術: Top/Bottom セグメント特定 → 異常要因分解 → 2〜3件の実験計画に落とし込みます。\n"
            "- 器: 週次SQL・ダッシュボード・閾値アラートで継続改善を回します。"
        )

    def _should_use_browser(self, question: str) -> bool:
        """ブラウザ取得を使用すべきか判定."""
        lowered = question.lower()
        browser_keywords = ["browse", "open url", "crawl", "爬虫", "抓取网页", "打开网页", "浏览器"]
        return any(keyword in lowered for keyword in browser_keywords)

    def _extract_first_url(self, text: str) -> str:
        """テキストから最初のURLを抽出."""
        match = re.search(r"https?://[^\s)\]>\"]+", text, re.IGNORECASE)
        if not match:
            return ""
        return match.group(0).strip()

    def _extract_search_query(self, question: str, context: dict[str, Any]) -> str:
        """外部検索用クエリを抽出."""
        options = context.get("options")
        if isinstance(options, dict):
            raw_query = options.get("external_query")
            if isinstance(raw_query, str) and raw_query.strip():
                return raw_query.strip()

        normalized = re.sub(r"https?://[^\s)\]>\"]+", " ", question, flags=re.IGNORECASE)
        normalized = re.sub(
            r"(请|請|帮我|幫我|联网|上网|搜索|查一下|查查|external api|web search|search web|browse|crawl|爬虫)",
            " ",
            normalized,
            flags=re.IGNORECASE,
        )
        normalized = re.sub(r"\s+", " ", normalized).strip()
        return normalized or question.strip()

    async def _try_browser_fetch(self, url: str) -> dict[str, Any] | None:
        """BrowserSkill でページ本文を取得."""
        if self.__browser_skill is None:
            return None
        lang = "ja"
        try:
            page_info = await self.__browser_skill.navigate(url)
            body_text = await self.__browser_skill.get_text("body")
            preview = re.sub(r"\s+", " ", body_text).strip()[:1200]
            lang = self._detect_language(body_text[:200])
            if lang == "zh":
                answer = (
                    f"已通过浏览器抓取页面：{page_info.title}\n"
                    f"URL: {page_info.url}\n"
                    f"摘要: {preview[:400]}"
                )
            elif lang == "en":
                answer = (
                    f"Fetched by browser: {page_info.title}\n"
                    f"URL: {page_info.url}\n"
                    f"Summary: {preview[:400]}"
                )
            else:
                answer = (
                    f"ブラウザでページを取得しました: {page_info.title}\n"
                    f"URL: {page_info.url}\n"
                    f"要約: {preview[:400]}"
                )
            return {
                "url": page_info.url,
                "title": page_info.title,
                "content_preview": preview,
                "mode": "browser",
                "answer": answer,
            }
        except Exception as exc:
            self._logger.warning("Browser fetch failed, fallback to search: %s", exc)
            return None
        finally:
            with suppress(Exception):
                await self.__browser_skill.stop()

    def _parse_sales_material_request(self, question: str) -> dict[str, Any]:
        """営業資料画像生成の入力を抽出."""
        num_images = self._config.sales_material_default_num_images
        count_match = re.search(r"(\d{1,2})\s*(枚|张|個|个|images?)", question, re.IGNORECASE)
        if count_match:
            requested = int(count_match.group(1))
            num_images = max(1, min(self._config.sales_material_max_images, requested))

        brief = re.sub(
            r"(请|請|帮我|幫我|做成|制作|作成|生成|ください|して|営業資料|营业资料|資料図|资料图)",
            " ",
            question,
            flags=re.IGNORECASE,
        )
        brief = re.sub(r"\d{1,2}\s*(枚|张|個|个|images?)", " ", brief, flags=re.IGNORECASE)
        brief = re.sub(r"(1:1|16:9|9:16|4:3|3:4|4:5)", " ", brief)
        brief = re.sub(r"#[0-9a-fA-F]{6}", " ", brief)
        brief = re.sub(r"(ブランドカラー|品牌色|颜色|色指定)", " ", brief, flags=re.IGNORECASE)
        brief = re.sub(r"[，,。]", " ", brief)
        brief = re.sub(r"\s+", " ", brief).strip() or question.strip()
        if len(brief) < 4:
            brief = question.strip()

        target_platform = ""
        question_lower = question.lower()
        if "instagram" in question_lower:
            target_platform = "instagram"
        elif "amazon" in question_lower:
            target_platform = "amazon"
        elif "linkedin" in question_lower:
            target_platform = "linkedin"

        aspect_ratio = "1:1"
        ratio_match = re.search(r"(1:1|16:9|9:16|4:3|3:4|4:5)", question)
        if ratio_match:
            aspect_ratio = ratio_match.group(1)

        brand_colors = re.findall(r"#[0-9a-fA-F]{6}", question)

        return {
            "brief": brief,
            "num_images": num_images,
            "target_platform": target_platform,
            "aspect_ratio": aspect_ratio,
            "brand_colors": brand_colors,
            "style_preferences": [],
        }

    def _extract_material_artifacts(
        self,
        workflow_result: dict[str, Any],
        request_id: str,
    ) -> list[dict[str, Any]]:
        """生成結果からアーティファクト一覧を抽出."""
        images = workflow_result.get("images", [])
        artifacts: list[dict[str, Any]] = []

        for index, image in enumerate(images):
            file_path = str(image.get("file_path", ""))
            file_name = Path(file_path).name if file_path else f"image_{index + 1:03d}.png"
            size_bytes = 0
            if file_path and Path(file_path).exists():
                size_bytes = Path(file_path).stat().st_size

            artifact_id = f"{request_id}-{index + 1:03d}"
            artifacts.append(
                {
                    "artifact_id": artifact_id,
                    "role": str(image.get("role", "")),
                    "file_path": file_path,
                    "file_name": file_name,
                    "size_bytes": size_bytes,
                    "download_url": f"artifact://{artifact_id}",
                }
            )
        return artifacts

    def _new_execution_report(self, question: str) -> dict[str, Any]:
        """実行レポートを初期化."""
        return {
            "report_id": f"exec-{uuid.uuid4().hex[:10]}",
            "question_preview": question[:120],
            "status": "running",
            "started_at": datetime.now().isoformat(),
            "phases": [],
            "_start_perf": time.perf_counter(),
        }

    def _append_report_phase(
        self,
        report: dict[str, Any],
        phase: str,
        status: str,
        details: dict[str, Any] | None = None,
    ) -> None:
        """実行レポートにフェーズを追加."""
        report["phases"].append(
            {
                "phase": phase,
                "status": status,
                "timestamp": datetime.now().isoformat(),
                "details": details or {},
            }
        )

    def _finish_execution_report(
        self,
        report: dict[str, Any],
        status: str,
        error_message: str = "",
    ) -> dict[str, Any]:
        """実行レポートを完了."""
        start_perf = float(report.pop("_start_perf", time.perf_counter()))
        report["finished_at"] = datetime.now().isoformat()
        report["duration_ms"] = int((time.perf_counter() - start_perf) * 1000)
        report["status"] = status
        if error_message:
            report["error"] = error_message
        return report

    def _build_a2a_metadata(self, query_type: str) -> dict[str, Any]:
        """A2A 連携向けメタデータを生成."""
        skill_map = {
            "faq": "knowledge_search",
            "sql": "sql_analytics",
            "hybrid": "knowledge_data_fusion",
            "chat": "general_assistant",
            "sales_material": "design_skills",
            "weather": "weather_info",
            "external": "external_research",
            "blocked": "safety_guard",
        }
        selected_skill = skill_map.get(query_type, "knowledge_search")
        card = AgentCard(
            name="faq-system-maq-router",
            description="社内FAQ/SQL分析/営業資料画像生成/外部調査/安全拒否を振り分けるマルチ機能Agent",
            version="1.1.0",
            skills=[
                AgentSkill(
                    name="knowledge_search",
                    description="社内知識検索と回答生成",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="sql_analytics",
                    description="自然言語からSQL生成し、表とチャートを返却",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="knowledge_data_fusion",
                    description="ナレッジ検索とデータ分析を協調実行して回答",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="general_assistant",
                    description="FAQ外の一般質問へ直接回答",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="design_skills",
                    description="営業資料向け画像セットを生成",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="weather_info",
                    description="都市名に基づく現在天気と短期予報を返却",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="external_research",
                    description="外部 API / Web 検索 / URL 取得を実行して根拠付きで返却",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
                AgentSkill(
                    name="safety_guard",
                    description="暴力・性・危険要求を検知し回答を拒否",
                    input_schema={"type": "object", "properties": {"question": {"type": "string"}}},
                    output_schema={"type": "object"},
                ),
            ],
            metadata={"selected_skill": selected_skill, "query_type": query_type},
        )
        return {
            "selected_skill": selected_skill,
            "card": card.to_a2a_format(),
        }

    # クエリタイプ別の実行メッセージ
    _QUERY_TYPE_LABELS: dict[str, str] = {
        "chat": "General Assistant が回答を生成中...",
        "faq": "Knowledge Search が RAG 検索を実行中...",
        "sql": "SQL Analytics が SQL を生成・実行中...",
        "hybrid": "Fusion Agent が RAG と SQL を並列実行中...",
        "sales_material": "Design Skills が画像ワークフローを実行中...",
        "weather": "Weather Info が天気 API を照会中...",
        "external": "External Research が外部 API / Web を調査中...",
        "blocked": "Safety Guard が安全ポリシーを適用中...",
    }

    _QUERY_TYPE_AGENT_LABELS: dict[str, str] = {
        "chat": "general_assistant",
        "faq": "knowledge_search",
        "sql": "sql_analytics",
        "hybrid": "knowledge_data_fusion",
        "sales_material": "design_skills",
        "weather": "weather_info",
        "external": "external_research",
        "blocked": "safety_guard",
    }

    def _estimate_stream_total_steps(self, query_type: str) -> int:
        """クエリタイプ別に SSE の想定ステップ数を算出."""
        total_steps = 6  # 受信 / 安全確認 / ルーティング / ルート確定 / 実行 / 整形
        if query_type in {"faq", "hybrid"}:
            total_steps += 1
        if query_type in {"sql", "hybrid"}:
            total_steps += 1
        return total_steps

    def _build_stream_progress_event(
        self,
        *,
        execution_id: str,
        step: int,
        total_steps: int,
        message: str,
        phase: str,
        agent: str,
        query_type: str = "",
        skill: str = "",
        force_progress: int | None = None,
    ) -> dict[str, Any]:
        """SSE 進捗イベントを構築."""
        safe_total = max(total_steps, 1)
        calculated = int((min(step, safe_total) / safe_total) * 100)
        progress = force_progress if force_progress is not None else min(calculated, 99)
        event: dict[str, Any] = {
            "type": "progress",
            "execution_id": execution_id,
            "progress": progress,
            "message": message,
            "phase": phase,
            "agent": agent,
            "step": min(step, safe_total),
            "total_steps": safe_total,
        }
        if query_type:
            event["query_type"] = query_type
        if skill:
            event["skill"] = skill
        return event

    async def run_stream(self, inputs: dict[str, Any]) -> Any:
        """ストリーム実行.

        既存 app の SSE 実装互換を維持しつつ、
        実際に実行している Agent / フェーズを進捗イベントで返す。
        """
        question = str(inputs.get("question", ""))
        raw_context = inputs.get("context", {})
        context = raw_context if isinstance(raw_context, dict) else {}
        effective_question = self._resolve_question_with_conversation_context(question, context)
        execution_id = f"stream-{uuid.uuid4().hex[:12]}"
        if not question:
            yield {
                "type": "error",
                "execution_id": execution_id,
                "message": "質問が指定されていません",
            }
            return

        try:
            execution_report = self._new_execution_report(question)
            self._append_report_phase(execution_report, "load", "ok", {"agent": self.name})
            step = 1
            total_steps = 6
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message="入力を受信し、初期化を開始しました。",
                phase="load",
                agent=self.name,
            )

            await self._ensure_services()

            step += 1
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message="Safety Guard で安全性を確認中...",
                phase="safety_check",
                agent="safety_guard",
            )
            safety_reason = await self._check_safety_violation(question)
            if safety_reason:
                blocked = self._build_blocked_response(question, safety_reason)
                self._append_report_phase(execution_report, "safety", "blocked", {"reason": safety_reason})
                blocked.execution_report = self._finish_execution_report(
                    execution_report,
                    status="blocked",
                    error_message=blocked.error,
                )
                blocked.a2a = self._build_a2a_metadata("blocked")

                step += 1
                yield self._build_stream_progress_event(
                    execution_id=execution_id,
                    step=step,
                    total_steps=total_steps,
                    message="安全ポリシー違反を検出したため回答を拒否しました。",
                    phase="blocked",
                    agent="safety_guard",
                    query_type="blocked",
                    skill=self._QUERY_TYPE_AGENT_LABELS["blocked"],
                    force_progress=95,
                )
                result = blocked.model_dump() if hasattr(blocked, "model_dump") else blocked.dict()
                yield self._build_stream_progress_event(
                    execution_id=execution_id,
                    step=total_steps,
                    total_steps=total_steps,
                    message="処理を終了しました。",
                    phase="done",
                    agent="safety_guard",
                    query_type="blocked",
                    skill=self._QUERY_TYPE_AGENT_LABELS["blocked"],
                    force_progress=100,
                )
                yield {
                    "type": "result",
                    "execution_id": execution_id,
                    "data": result,
                }
                return

            step += 1
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message="Intent Router で問い合わせ種別を判定中...",
                phase="routing",
                agent="intent_router",
            )
            query_type = await self._route_query(effective_question, context=context)
            total_steps = self._estimate_stream_total_steps(query_type)
            skill = self._QUERY_TYPE_AGENT_LABELS.get(query_type, "knowledge_search")
            self._append_report_phase(execution_report, "start", "ok", {"query_type": query_type})

            step += 1
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message=f"ルーティング完了: {query_type} ({skill})",
                phase="routing_done",
                agent="intent_router",
                query_type=query_type,
                skill=skill,
            )

            self._append_report_phase(execution_report, "call", "ok", {"service": skill})
            service_label = self._QUERY_TYPE_LABELS.get(query_type, "処理中...")
            step += 1
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message=service_label,
                phase="execute_start",
                agent=skill,
                query_type=query_type,
                skill=skill,
            )

            response: FAQOutput
            if query_type == "chat":
                response = await self._handle_chat_query(effective_question, query_type)
            elif query_type == "weather":
                response = await self._handle_weather_query(effective_question, query_type)
            elif query_type == "external":
                response = await self._handle_external_query(effective_question, query_type, context)
            elif query_type == "hybrid":
                response = await self._handle_hybrid_query(effective_question, query_type)
            elif query_type == "sql":
                response = await self._handle_sql_query(effective_question, query_type)
            elif query_type == "sales_material":
                response = await self._handle_sales_material_query(effective_question, query_type)
            else:
                response = await self._handle_faq_query(effective_question, query_type)

            if query_type in {"faq", "hybrid"}:
                step += 1
                yield self._build_stream_progress_event(
                    execution_id=execution_id,
                    step=step,
                    total_steps=total_steps,
                    message="回答根拠を自己検証中...",
                    phase="verification",
                    agent=skill,
                    query_type=query_type,
                    skill=skill,
                )

            if query_type in {"sql", "hybrid"}:
                step += 1
                yield self._build_stream_progress_event(
                    execution_id=execution_id,
                    step=step,
                    total_steps=total_steps,
                    message="分析結果と改善提案を整形中...",
                    phase="analytics_summary",
                    agent=skill,
                    query_type=query_type,
                    skill=skill,
                )

            step += 1
            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=step,
                total_steps=total_steps,
                message="回答データを整形中...",
                phase="compose",
                agent=skill,
                query_type=query_type,
                skill=skill,
            )

            self._append_report_phase(
                execution_report,
                "execute",
                "ok",
                {
                    "query_type": query_type,
                    "artifacts": len(response.artifacts),
                    "has_chart": response.chart is not None,
                },
            )
            response.execution_report = self._finish_execution_report(execution_report, status="success")
            response.a2a = self._build_a2a_metadata(query_type)
            response.question = question
            result = response.model_dump() if hasattr(response, "model_dump") else response.dict()

            yield self._build_stream_progress_event(
                execution_id=execution_id,
                step=total_steps,
                total_steps=total_steps,
                message="処理が完了しました。",
                phase="done",
                agent=skill,
                query_type=query_type,
                skill=skill,
                force_progress=100,
            )
            yield {
                "type": "result",
                "execution_id": execution_id,
                "data": result,
            }
        except Exception as exc:
            self._logger.exception("run_stream failed: %s", exc)
            yield {
                "type": "error",
                "execution_id": execution_id,
                "message": str(exc),
            }

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
        return ["rag", "text2sql", "chart", "suggestion", "design_skills", "weather", "external", "safety_guard"]

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
                {"name": "artifacts", "type": "array", "label": "生成アセット"},
                {"name": "execution_report", "type": "object", "label": "実行レポート"},
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
                {
                    "name": "auto_chart",
                    "type": "boolean",
                    "label": "チャート自動生成",
                    "default": True,
                },
                {
                    "name": "sales_material_output_root",
                    "type": "string",
                    "label": "営業資料画像の出力先",
                    "default": "/tmp/faq_sales_material",
                },
            ],
        }


__all__ = [
    "ChartSchema",
    "DocumentSchema",
    "FAQAgent",
    "FAQAgentConfig",
    "FAQInput",
    "FAQOutput",
    "SuggestionSchema",
]
