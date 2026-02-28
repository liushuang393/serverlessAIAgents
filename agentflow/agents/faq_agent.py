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


class FAQOutput(BaseModel):
    """FAQAgent 出力スキーマ."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"  # "chat" | "faq" | "sql" | "hybrid" | "sales_material"
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

        # 意図ルーターでクエリタイプを判定
        query_type = await self._route_query(question)
        self._append_report_phase(execution_report, "start", "ok", {"query_type": query_type})

        try:
            if query_type == "chat":
                self._append_report_phase(execution_report, "call", "ok", {"service": "llm_direct"})
                response = await self._handle_chat_query(question, query_type)
            elif query_type == "hybrid":
                self._append_report_phase(execution_report, "call", "ok", {"service": "rag+text2sql"})
                response = await self._handle_hybrid_query(question, query_type)
            elif query_type == "sql":
                self._append_report_phase(execution_report, "call", "ok", {"service": "text2sql"})
                response = await self._handle_sql_query(question, query_type)
            elif query_type == "sales_material":
                self._append_report_phase(execution_report, "call", "ok", {"service": "design_skills"})
                response = await self._handle_sales_material_query(question, query_type)
            else:
                self._append_report_phase(execution_report, "call", "ok", {"service": "rag"})
                response = await self._handle_faq_query(question, query_type)

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
            "ユーザーからの挨拶や日常的な質問には、親しみやすく簡潔に回答してください。\n"
            "ナレッジベース検索は不要です。あなた自身の知識で直接回答してください。\n\n"
            f"ユーザー: {question}"
        )
        answer = await self._call_llm(chat_prompt)
        if not answer:
            answer = "こんにちは！何かお手伝いできることはありますか？"

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
        rag_result = await self.__rag_service.execute(
            action="query",
            question=question,
        )
        if not rag_result.success:
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=rag_result.error_message or "RAG 検索に失敗しました。",
                documents=[],
                rich_response=self._build_chat_rich_response(
                    rag_result.error_message or "RAG 検索に失敗しました。"
                ).to_dict(),
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

        suggestions = await self._generate_suggestions(question, query_type, bool(documents))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=rag_result.data.get("answer", ""),
            documents=documents,
            rich_response=self._build_faq_rich_response(
                answer=rag_result.data.get("answer", ""),
                documents=documents,
            ).to_dict(),
            suggestions=suggestions,
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
            return FAQOutput(
                question=question,
                query_type=query_type,
                answer=sql_result.error_message or "データ分析に失敗しました。",
                rich_response=self._build_chat_rich_response(
                    sql_result.error_message or "データ分析に失敗しました。"
                ).to_dict(),
                suggestions=await self._generate_suggestions(question, query_type, False),
                error=sql_result.error_message or "sql_error",
            )

        chart_data = sql_result.data.get("chart")
        chart = None
        if chart_data:
            chart = ChartSchema(
                chart_type=chart_data.get("type", "bar"),
                title=chart_data.get("title", ""),
                data=chart_data.get("data", {}),
            )

        suggestions = await self._generate_suggestions(question, query_type, bool(sql_result.data.get("data")))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=sql_result.data.get("answer", ""),
            sql=sql_result.data.get("sql", ""),
            data=sql_result.data.get("data", []),
            columns=sql_result.data.get("columns", []),
            chart=chart,
            rich_response=self._build_sql_rich_response(
                answer=sql_result.data.get("answer", ""),
                sql=sql_result.data.get("sql", ""),
                data=sql_result.data.get("data", []),
            ).to_dict(),
            suggestions=suggestions,
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

        rag_task = self.__rag_service.execute(action="query", question=question)
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

        chart_data = sql_service_result.data.get("chart")
        chart = None
        if isinstance(chart_data, dict):
            chart = ChartSchema(
                chart_type=str(chart_data.get("type", "bar")),
                title=str(chart_data.get("title", "")),
                data=chart_data.get("data", {}) if isinstance(chart_data.get("data"), dict) else {},
            )

        rich_response = self._build_sql_rich_response(
            answer=combined_answer,
            sql=sql_text if isinstance(sql_text, str) else "",
            data=sql_data if isinstance(sql_data, list) else [],
        ).to_dict()
        suggestions = await self._generate_suggestions(question, query_type, bool(documents or sql_data))

        return FAQOutput(
            question=question,
            query_type=query_type,
            answer=combined_answer,
            documents=documents,
            sql=sql_text if isinstance(sql_text, str) else "",
            data=sql_data if isinstance(sql_data, list) else [],
            columns=sql_columns if isinstance(sql_columns, list) else [],
            chart=chart,
            rich_response=rich_response,
            suggestions=suggestions,
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
                SuggestionSchema(text="16:9のスライド用サイズでもう一度生成", type="followup"),
                SuggestionSchema(text="ブランドカラーを指定して再生成", type="followup"),
                SuggestionSchema(text="生成画像から営業提案文を作成", type="followup"),
            ]

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
            ChunkStrategy,
            RAGConfig,
            RAGService,
            RerankerType,
            SQLDialect,
            SuggestionConfig,
            SuggestionService,
            Text2SQLConfig,
            Text2SQLService,
        )
        from agentflow.skills.builtin.design_skills.agents.intent_analyzer_agent import (
            IntentAnalyzerAgent,
        )
        from agentflow.skills.builtin.design_skills.agents.prompt_planner_agent import (
            PromptPlannerAgent,
        )
        from agentflow.skills.builtin.design_skills.agents.workflow_executor_agent import (
            WorkflowExecutorAgent,
        )

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
        self.__design_intent_agent = IntentAnalyzerAgent(llm_client=self._llm)
        self.__design_prompt_agent = PromptPlannerAgent(llm_client=self._llm)
        self.__design_workflow_agent = WorkflowExecutorAgent(llm_client=self._llm)

        self._services_initialized = True

    # LLM意図分類プロンプト
    _INTENT_PROMPT = (
        "You are the intent routing agent for an enterprise FAQ system.\n"
        "Route the user message into exactly one route.\n\n"
        "Routes:\n"
        "- chat: general chat, unrelated topics, greetings, pure LLM answer only.\n"
        "- faq: knowledge-base lookup only.\n"
        "- sql: database analytics only.\n"
        "- hybrid: both KB context and DB analytics are needed.\n"
        "- sales_material: generate sales materials/images.\n"
        "- unclear: cannot confidently decide faq/sql/chat.\n\n"
        "Rules:\n"
        "- If unrelated to FAQ/enterprise knowledge, choose chat.\n"
        "- If unclear between faq/sql/hybrid, choose unclear.\n"
        '- Return JSON only: {{"route":"<one_of_routes>","confidence":0.0-1.0,"reason":"short"}}.\n\n'
        "User message: {question}"
    )

    _VALID_QUERY_TYPES = frozenset({"chat", "sales_material", "sql", "faq", "hybrid", "unclear"})

    async def _classify_query(self, question: str) -> str:
        """互換API: 意図分類を実行してルートを返す.

        既存テスト/呼び出し互換のため残し、内部は _route_query を利用する。
        """
        return await self._route_query(question)

    async def _route_query(self, question: str) -> str:
        """意図ルーターでクエリルートを決定.

        Args:
            question: 質問文

        Returns:
            "chat" | "sales_material" | "sql" | "faq" | "hybrid"
        """
        route = ""
        if self._llm is not None:
            try:
                prompt = self._INTENT_PROMPT.format(question=question)
                raw = await self._call_llm(prompt)
                route = self._extract_route_from_intent_response(raw)
                if route in self._VALID_QUERY_TYPES:
                    return self._apply_route_policy(route)
                self._logger.warning(
                    "LLM intent classification returned unexpected value: %r, falling back to heuristic",
                    raw,
                )
            except Exception as e:
                self._logger.warning("LLM intent classification failed: %s", e)

        route = self._classify_query_heuristic(question)
        return self._apply_route_policy(route)

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

        sales_material_keywords = [
            "pitch deck",
            "sales deck",
            "poster",
            "flyer",
            "banner",
        ]
        if any(k in question_lower for k in sales_material_keywords):
            return "sales_material"

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
        ]
        sql_score = sum(1 for k in sql_indicators if k in question_lower)
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
        }
        selected_skill = skill_map.get(query_type, "knowledge_search")
        card = AgentCard(
            name="faq-system-maq-router",
            description="社内FAQ/SQL分析/営業資料画像生成を振り分けるマルチ機能Agent",
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
            ],
            metadata={"selected_skill": selected_skill, "query_type": query_type},
        )
        return {
            "selected_skill": selected_skill,
            "card": card.to_a2a_format(),
        }

    # クエリタイプ別の進捗メッセージ
    _QUERY_TYPE_LABELS: dict[str, str] = {
        "chat": "LLM で回答を生成中...",
        "faq": "ナレッジベースを検索中...",
        "sql": "SQL クエリを生成中...",
        "hybrid": "RAG と SQL を協調処理中...",
        "sales_material": "営業資料を作成中...",
    }

    async def run_stream(self, inputs: dict[str, Any]) -> Any:
        """ストリーム実行.

        既存 app の SSE 実装互換のため、軽量な進捗イベントを返す。
        進捗ステップ: 10→20→40→60→90→100
        """
        question = str(inputs.get("question", ""))
        execution_id = f"stream-{uuid.uuid4().hex[:12]}"
        if not question:
            yield {
                "type": "error",
                "execution_id": execution_id,
                "message": "質問が指定されていません",
            }
            return

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 10,
            "message": "ロード完了",
        }

        # クエリタイプ判定
        query_type = await self._route_query(question)
        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 20,
            "message": f"ルーティング: {query_type}",
        }

        # サービス呼び出し開始を通知
        service_label = self._QUERY_TYPE_LABELS.get(query_type, "処理中...")
        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 40,
            "message": service_label,
        }

        # メイン処理（LLM呼び出し含む）
        result = await self.run(inputs)

        yield {
            "type": "progress",
            "execution_id": execution_id,
            "progress": 90,
            "message": "回答を整形中...",
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
            "data": result,
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
        return ["rag", "text2sql", "chart", "suggestion", "design_skills"]

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
