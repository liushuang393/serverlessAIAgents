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
    """FAQ専門Agent (App Wrapper).

    実装は kernel 側に移動しました。
    """

    def __init__(
        self,
        config: FAQAgentConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        super().__init__(config=config, llm_client=llm_client)

    async def process(self, input_data: FAQInput) -> FAQOutput:
        """Kernel Agent を呼び出し、互換性のある出力を返す."""
        # Kernel Agent の実行
        result_dict = await self.run(input_data.model_dump())
        
        # FAQResponse から FAQOutput への変換
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
            output.suggestions = [SuggestionSchema(**s) for s in result_dict["suggestions"] if isinstance(s, dict)]
            
        if "citations" in result_dict:
            output.documents = [
                DocumentSchema(
                    id=c.get("id", ""),
                    content=c.get("snippet", ""),
                    source=c.get("title", ""),
                    score=c.get("score", 0.0)
                ) for c in result_dict["citations"] if isinstance(c, dict)
            ]
            
        return output

    # -----------------------------------------------------------------------
    # 互換用内部メソッド (テスト用)
    # -----------------------------------------------------------------------

    @property
    def _llm(self) -> Any:
        return self.llm_client

    @_llm.setter
    def _llm(self, value: Any) -> None:
        self.llm_client = value

<<<<<<< HEAD
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

        from harness.guardrails.ai_safety_guard import AISafetyGuard, GuardConfig
        from shared.services import (
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
        # オプション依存: 存在しない場合は None で代替
        _browser_cls: Any = None
        _browser_cfg_cls: Any = None
        if self._config.enable_browser_tools:
            try:
                from kernel.skills.browser import BrowserSkill as _BrowserSkill, BrowserSkillConfig as _BrowserSkillConfig  # type: ignore[import-not-found]
                _browser_cls = _BrowserSkill
                _browser_cfg_cls = _BrowserSkillConfig
            except ImportError:
                self._logger.warning("kernel.skills.browser not found; browser tools disabled")

        _intent_agent_cls: Any = None
        _prompt_agent_cls: Any = None
        _workflow_agent_cls: Any = None
        try:
            from kernel.skills.builtin.design_skills.agents.intent_analyzer_agent import (
                IntentAnalyzerAgent as _IntentAnalyzerAgent,
            )
            from kernel.skills.builtin.design_skills.agents.prompt_planner_agent import (
                PromptPlannerAgent as _PromptPlannerAgent,
            )
            from kernel.skills.builtin.design_skills.agents.workflow_executor_agent import (
                WorkflowExecutorAgent as _WorkflowExecutorAgent,
            )
            _intent_agent_cls = _IntentAnalyzerAgent
            _prompt_agent_cls = _PromptPlannerAgent
            _workflow_agent_cls = _WorkflowExecutorAgent
        except ImportError:
            self._logger.warning("design_skills agents not found; sales material disabled")

        _web_search_cls: Any = None
        _search_cfg_cls: Any = None
        if self._config.enable_external_tools:
            try:
                from kernel.skills.builtin.web_search import SearchConfig as _SearchConfig, WebSearchSkill as _WebSearchSkill  # type: ignore[import-not-found]
                _web_search_cls = _WebSearchSkill
                _search_cfg_cls = _SearchConfig
            except ImportError:
                self._logger.warning("kernel.skills.builtin.web_search not found; external search disabled")

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

        if self._config.enable_external_tools and _web_search_cls is not None and _search_cfg_cls is not None:
            self.__web_search_skill = _web_search_cls(
                _search_cfg_cls(
                    max_results=self._config.external_max_results,
                    timeout=self._config.external_fetch_timeout_seconds,
                )
            )
        else:
            self.__web_search_skill = None

        if self._config.enable_browser_tools and _browser_cls is not None and _browser_cfg_cls is not None:
            self.__browser_skill = _browser_cls(
                _browser_cfg_cls(
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

        self.__design_intent_agent = _intent_agent_cls(llm_client=self._llm) if _intent_agent_cls is not None else None
        self.__design_prompt_agent = _prompt_agent_cls(llm_client=self._llm) if _prompt_agent_cls is not None else None
        self.__design_workflow_agent = _workflow_agent_cls(llm_client=self._llm) if _workflow_agent_cls is not None else None

        self._services_initialized = True

    # LLM 意図路由器提示词（语义判断优先，不依赖固定关键词）
    _INTENT_PROMPT_TEMPLATE = (
        "You are IntentRouterAgent for an enterprise assistant.\n"
        "Select exactly one route for downstream sub-agent execution based on user intent and required data source.\n"
        "Do not classify by shallow keyword matching; infer the real task objective.\n\n"
        "Policy:\n"
        "1) Enterprise knowledge, internal policy/process, MCP integration, or business DB analysis => use faq/sql/hybrid/external.\n"
        "2) General world knowledge (math, history, physics, common-sense Q&A) => use chat.\n"
        "3) If the request is outside both categories or not answerable as Q&A => use unclear.\n\n"
        "Route definitions:\n"
        "- chat: general conversation, no enterprise KB/DB lookup needed\n"
        "- faq: enterprise policy/procedure knowledge lookup via RAG knowledge base\n"
        "- sql: structured analytics/aggregation from business database only\n"
        "- hybrid: needs BOTH KB context and DB analytics in one answer\n"
        "- sales_material: generate sales materials/images/deck assets\n"
        "- weather: weather/forecast query for a city\n"
        "- external: web search/external API/url browsing outside internal data\n"
        "- blocked: harmful/illegal request that should be refused\n"
        "- unclear: out-of-scope request or not answerable within enterprise/general Q&A scope\n\n"
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
            if llm_decision.route in {"unclear", ""}:
                if llm_decision.confidence >= self._INTENT_CONFIDENCE_THRESHOLD:
                    return self._apply_route_policy("unclear")
                ambiguous_route = self._default_route_without_keyword_rules(question, normalized_context)
                return self._apply_route_policy(ambiguous_route)
            return self._apply_route_policy(llm_decision.route)

        # LLM が使えない場合のみ非キーワード型の保守的デフォルトへフォールバック
        fallback_route = self._default_route_without_keyword_rules(question, normalized_context)
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
        confidence = 0.65 if route in self._VALID_QUERY_TYPES and route not in {"", "unclear"} else 0.0
        return IntentRouteDecision(route=route or "unclear", confidence=confidence, reason="unstructured_response")

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

    def _default_route_without_keyword_rules(self, question: str, context: dict[str, Any]) -> str:
        """意図不明時のフォールバックルートを返す."""
        hinted = self._extract_route_hint(context).lower()
        if hinted in self._VALID_QUERY_TYPES:
            return hinted

        strong_route = self._infer_strong_fallback_route(question)
        if strong_route:
            return strong_route

        if self._looks_like_enterprise_faq_question(question) and self._config.enable_rag:
            return "faq"

        if self._looks_like_information_request(question):
            return "chat"

        return "unclear"

    def _infer_strong_fallback_route(self, question: str) -> str:
        """曖昧時でも強く推定できるルートを返す."""
        heuristic_route = self._classify_query_heuristic(question)
        if heuristic_route in {"blocked", "weather", "external", "sales_material", "chat", "sql"}:
            return heuristic_route
        if heuristic_route == "faq" and self._looks_like_enterprise_faq_question(question):
            return "faq"
        return ""

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
=======
    async def _classify_query(self, question: str) -> str:
        """テスト互換用のクエリ分類."""
        from shared.services.query_classifier import classify_query
        return classify_query(question).value
>>>>>>> 3cd2683 (refactor: centralize FAQ agent logic into kernel and introduce shared query classifier service.)

    def _classify_query_heuristic(self, question: str) -> str:
        """テスト互換用のヒューリスティック分類."""
        from shared.services.query_classifier import QueryClassifier
        return QueryClassifier().classify(question).value

    async def _handle_chat_query(self, question: str, query_type: str) -> FAQOutput:
        # Kernel 側には個別のハンドラメソッドがないため、run() を経由して疑似的に実行
        return await self.process(FAQInput(question=question))

    async def _handle_faq_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_sql_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_weather_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))

    async def _handle_external_query(self, question: str, query_type: str, context: dict) -> FAQOutput:
        return await self.process(FAQInput(question=question, context=context))

    async def _handle_hybrid_query(self, question: str, query_type: str) -> FAQOutput:
        return await self.process(FAQInput(question=question))


__all__ = ["FAQAgent", "FAQAgentConfig", "FAQInput", "FAQOutput"]
