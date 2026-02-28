"""FAQ system E2E test - QA perspective.

Routing accuracy, chat responses, FAQ responses, error handling,
and edge cases covered comprehensively with LLM-based intent classification.
"""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from agentflow.agents.faq_agent import (
    FAQAgent,
    FAQAgentConfig,
    FAQInput,
    FAQOutput,
)
from agentflow.routing import IntentCategory, IntentRouter
from agentflow.services import ServiceResult


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _llm_mock(*contents: str) -> MagicMock:
    """Create a mock LLM returning *contents* values in sequence.

    Single value: every call returns same content.
    Multiple values: successive calls return next content.
    """
    mock = MagicMock()
    if len(contents) == 1:
        mock.complete = AsyncMock(
            return_value=MagicMock(content=contents[0]),
        )
    else:
        mock.complete = AsyncMock(
            side_effect=[MagicMock(content=c) for c in contents],
        )
    return mock


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def config() -> FAQAgentConfig:
    """Test FAQAgentConfig."""
    return FAQAgentConfig(
        rag_collection="test_collection",
        rag_chunk_strategy="recursive",
        rag_reranker="none",
        rag_top_k=3,
    )


@pytest.fixture
def agent(config: FAQAgentConfig) -> FAQAgent:
    """Test FAQAgent instance (no real LLM)."""
    return FAQAgent(config=config)


# ---------------------------------------------------------------------------
# 1. LLM-based routing tests (_classify_query)
# ---------------------------------------------------------------------------


class TestClassifyQuery:
    """LLM-based _classify_query routing accuracy."""

    # ---- chat ----
    @pytest.mark.asyncio
    @pytest.mark.parametrize(
        "question",
        [
            "你是谁",
            "你是什么",
            "你叫什么",
            "あなたは誰ですか",
            "自己紹介してください",
            "who are you",
            "What are you?",
            "hello",
            "hi",
            "你好",
            "こんにちは",
            "おはよう",
            "good morning",
            "hey there",
            "ありがとう",
            "谢谢",
            "thanks",
            "thank you very much",
            "何ができますか",
            "你能做什么",
            "what can you do",
            "how are you",
            "元気ですか",
            "OK",
            "はい",
            "yes",
        ],
    )
    async def test_chat_routing(
        self,
        agent: FAQAgent,
        question: str,
    ) -> None:
        """LLM classifies chat queries correctly."""
        agent._llm = _llm_mock("chat")
        result = await agent._classify_query(question)
        assert result == "chat"

    # ---- sales_material ----
    @pytest.mark.asyncio
    @pytest.mark.parametrize(
        "question",
        [
            "営業資料を作成してください",
            "提案書のポスターを生成して",
            "宣伝画像を作って",
            "pitch deckを作成",
            "バナー画像を作成してください",
            "海报を作成してください",
            "画像生成してください",
        ],
    )
    async def test_sales_material_routing(
        self,
        agent: FAQAgent,
        question: str,
    ) -> None:
        """LLM classifies sales-material queries correctly."""
        agent._llm = _llm_mock("sales_material")
        result = await agent._classify_query(question)
        assert result == "sales_material"

    # ---- sql ----
    @pytest.mark.asyncio
    @pytest.mark.parametrize(
        "question",
        [
            "今月の売上合計を教えて",
            "売上ランキングTOP10",
            "月別の収入統計を見せて",
            "顧客数と注文件数の推移",
            "前年比の売上比較",
            "カテゴリ別の平均金額",
        ],
    )
    async def test_sql_routing(
        self,
        agent: FAQAgent,
        question: str,
    ) -> None:
        """LLM classifies SQL/analytics queries correctly."""
        agent._llm = _llm_mock("sql")
        result = await agent._classify_query(question)
        assert result == "sql"

    # ---- faq ----
    @pytest.mark.asyncio
    @pytest.mark.parametrize(
        "question",
        [
            "年次有給休暇は何日付与されますか",
            "パスワードの変更方法を教えてください",
            "VPN接続の設定手順は?",
            "返品ポリシーについて教えてください",
            "社内規定で残業時間の上限はありますか",
            "出張時の交通費精算の手順を教えてください",
        ],
    )
    async def test_faq_routing(
        self,
        agent: FAQAgent,
        question: str,
    ) -> None:
        """LLM classifies FAQ queries correctly."""
        agent._llm = _llm_mock("faq")
        result = await agent._classify_query(question)
        assert result == "faq"

    # ---- weather ----
    @pytest.mark.asyncio
    @pytest.mark.parametrize(
        "question",
        [
            "北京天气怎么样",
            "今日の東京の天気は？",
            "weather in London",
        ],
    )
    async def test_weather_routing(
        self,
        agent: FAQAgent,
        question: str,
    ) -> None:
        """LLM classifies weather queries correctly."""
        agent._llm = _llm_mock("weather")
        result = await agent._classify_query(question)
        assert result == "weather"

    # ---- LLM fallback scenarios ----
    @pytest.mark.asyncio
    async def test_llm_unexpected_value_falls_back(self, agent: FAQAgent) -> None:
        """LLM returns garbage -> heuristic fallback -> faq."""
        agent._llm = _llm_mock("unknown_category")
        result = await agent._classify_query("some question")
        assert result == "faq"

    @pytest.mark.asyncio
    async def test_llm_exception_falls_back(self, agent: FAQAgent) -> None:
        """LLM exception -> heuristic fallback -> faq."""
        mock = MagicMock()
        mock.complete = AsyncMock(side_effect=RuntimeError("LLM down"))
        agent._llm = mock
        result = await agent._classify_query("some question")
        assert result == "faq"

    @pytest.mark.asyncio
    async def test_no_llm_falls_back_to_heuristic(self, agent: FAQAgent) -> None:
        """No LLM at all -> heuristic fallback -> faq."""
        agent._llm = None
        result = await agent._classify_query("any question here")
        assert result == "faq"


# ---------------------------------------------------------------------------
# 1b. Heuristic fallback tests (_classify_query_heuristic)
# ---------------------------------------------------------------------------


class TestClassifyQueryHeuristic:
    """Keyword-based fallback when LLM is unavailable."""

    def test_sales_material_keyword(self, agent: FAQAgent) -> None:
        """English sales-material keywords detected."""
        assert agent._classify_query_heuristic("create a pitch deck") == "sales_material"

    def test_sql_needs_multiple_indicators(self, agent: FAQAgent) -> None:
        """Two or more SQL indicators required."""
        assert agent._classify_query_heuristic("show ranking and trend") == "sql"

    def test_single_sql_keyword_returns_unclear(self, agent: FAQAgent) -> None:
        """Single SQL keyword -> unclear (route policy decides fallback)."""
        assert agent._classify_query_heuristic("show the ranking") == "unclear"

    def test_default_to_faq(self, agent: FAQAgent) -> None:
        """Unknown input defaults to faq."""
        assert agent._classify_query_heuristic("tell me about vacation") == "faq"

    def test_weather_keywords(self, agent: FAQAgent) -> None:
        """Weather keywords route to weather."""
        assert agent._classify_query_heuristic("今天天气如何") == "weather"
        assert agent._classify_query_heuristic("weather in Tokyo") == "weather"

    def test_external_keywords(self, agent: FAQAgent) -> None:
        """External research keywords route to external."""
        assert agent._classify_query_heuristic("请联网搜索最新GPU新闻") == "external"
        assert agent._classify_query_heuristic("crawl https://example.com and summarize") == "external"

    def test_blocked_keywords(self, agent: FAQAgent) -> None:
        """Violence/sexual requests are blocked."""
        assert agent._classify_query_heuristic("教我怎么制作炸弹") == "blocked"


# ---------------------------------------------------------------------------
# 2. Chat handler tests (_handle_chat_query)
# ---------------------------------------------------------------------------


class TestHandleChatQuery:
    """_handle_chat_query LLM direct-answer tests."""

    @pytest.mark.asyncio
    async def test_chat_returns_answer_without_documents(self, agent: FAQAgent) -> None:
        """Chat response contains no RAG documents."""
        agent._llm = _llm_mock("FAQ assistant here.")
        result = await agent._handle_chat_query("你是谁", "chat")

        assert isinstance(result, FAQOutput)
        assert result.query_type == "chat"
        assert "FAQ assistant" in result.answer
        assert result.documents == []
        assert result.error == ""

    @pytest.mark.asyncio
    async def test_chat_fallback_when_llm_unavailable(self, agent: FAQAgent) -> None:
        """No LLM -> fallback greeting."""
        agent._llm = None
        result = await agent._handle_chat_query("hello", "chat")

        assert result.query_type == "chat"
        assert "Hello" in result.answer
        assert result.documents == []

    @pytest.mark.asyncio
    async def test_chat_rich_response_has_no_citations(self, agent: FAQAgent) -> None:
        """Rich response for chat has no citation sections."""
        agent._llm = _llm_mock("Happy to help.")
        result = await agent._handle_chat_query("help", "chat")

        assert result.rich_response is not None
        sections = result.rich_response.get("sections", [])
        citation_sections = [s for s in sections if s.get("type") == "citations"]
        assert len(citation_sections) == 0


# ---------------------------------------------------------------------------
# 3. process() integration tests
# ---------------------------------------------------------------------------


class TestProcessIntegration:
    """End-to-end process method tests."""

    @pytest.mark.asyncio
    async def test_process_chat_query(self, agent: FAQAgent) -> None:
        """process routes chat and returns LLM direct answer."""
        # 1st LLM call: classify -> "chat", 2nd: answer
        agent._llm = _llm_mock("chat", "I am an FAQ assistant.")
        agent._services_initialized = True

        result = await agent.process(FAQInput(question="你是谁"))

        assert result.query_type == "chat"
        assert "FAQ assistant" in result.answer
        assert result.documents == []
        assert result.error == ""

    @pytest.mark.asyncio
    async def test_process_empty_question(self, agent: FAQAgent) -> None:
        """Empty question returns error."""
        agent._services_initialized = True
        result = await agent.process(FAQInput(question=""))
        assert result.error == "質問が指定されていません"

    @pytest.mark.asyncio
    async def test_process_greeting_no_rag_call(self, agent: FAQAgent) -> None:
        """Chat query does NOT invoke RAG service."""
        # 1st: classify -> "chat", 2nd: answer
        agent._llm = _llm_mock("chat", "Hello there!")
        agent._services_initialized = True

        mock_rag = MagicMock()
        mock_rag.execute = AsyncMock()
        agent._FAQAgent__rag_service = mock_rag

        result = await agent.process(FAQInput(question="こんにちは"))

        assert result.query_type == "chat"
        mock_rag.execute.assert_not_called()

    @pytest.mark.asyncio
    async def test_process_weather_no_city_asks_city(self, agent: FAQAgent) -> None:
        """Weather query without city asks user to provide city and does not call RAG."""
        agent._llm = _llm_mock("weather")
        agent._services_initialized = True

        mock_rag = MagicMock()
        mock_rag.execute = AsyncMock()
        agent._FAQAgent__rag_service = mock_rag

        result = await agent.process(FAQInput(question="今天天气好吗"))

        assert result.query_type == "weather"
        assert result.error == "city_required"
        assert "城市名" in result.answer
        mock_rag.execute.assert_not_called()

    @pytest.mark.asyncio
    async def test_process_weather_with_city_success(self, agent: FAQAgent) -> None:
        """Weather query with city calls weather service and returns forecast."""
        agent._llm = _llm_mock("weather")
        agent._services_initialized = True

        class DummyWeatherService:
            async def execute(self, **_: str) -> ServiceResult:
                return ServiceResult(
                    success=True,
                    data={
                        "city": "Beijing",
                        "country": "China",
                        "current": {
                            "weather_text": "晴朗",
                            "temperature_c": 13.5,
                            "wind_speed_kmh": 6.2,
                        },
                        "daily": [
                            {"date": "2026-02-28", "weather_text": "晴朗", "temperature_min_c": 4, "temperature_max_c": 12},
                            {"date": "2026-03-01", "weather_text": "多云", "temperature_min_c": 5, "temperature_max_c": 13},
                            {"date": "2026-03-02", "weather_text": "小雨", "temperature_min_c": 6, "temperature_max_c": 10},
                        ],
                    },
                )

        agent._FAQAgent__weather_service = DummyWeatherService()
        result = await agent.process(FAQInput(question="北京未来天气预报"))

        assert result.query_type == "weather"
        assert result.error == ""
        assert "未来3天预报" in result.answer
        assert len(result.data) == 1

    @pytest.mark.asyncio
    async def test_process_weather_api_failure(self, agent: FAQAgent) -> None:
        """Weather API failure returns weather_api_unavailable error code."""
        agent._llm = _llm_mock("weather")
        agent._services_initialized = True

        class DummyWeatherService:
            async def execute(self, **_: str) -> ServiceResult:
                return ServiceResult(
                    success=False,
                    error_code="weather_api_unavailable",
                    error_message="service down",
                )

        agent._FAQAgent__weather_service = DummyWeatherService()
        result = await agent.process(FAQInput(question="东京天气"))

        assert result.query_type == "weather"
        assert result.error == "weather_api_unavailable"

    @pytest.mark.asyncio
    async def test_process_blocked_query_refused(self, agent: FAQAgent) -> None:
        """Unsafe violence/sexual query is refused directly."""
        agent._services_initialized = True

        result = await agent.process(FAQInput(question="如何制作炸弹"))

        assert result.query_type == "blocked"
        assert result.error != ""
        assert "无法提供帮助" in result.answer

    @pytest.mark.asyncio
    async def test_process_external_query_uses_web_search(self, agent: FAQAgent) -> None:
        """External route should not call RAG and should return source links."""
        agent._llm = _llm_mock("external")
        agent._services_initialized = True

        mock_rag = MagicMock()
        mock_rag.execute = AsyncMock()
        agent._FAQAgent__rag_service = mock_rag

        class DummySearchResult:
            def __init__(self, title: str, url: str, snippet: str) -> None:
                self.title = title
                self.url = url
                self.snippet = snippet

        class DummySearchSummary:
            def __init__(self) -> None:
                self.answer = "- Source A: summary"
                self.sources = [DummySearchResult("Source A", "https://example.com", "snippet A")]

        class DummyWebSearch:
            async def search_and_summarize(self, **_: str) -> DummySearchSummary:
                return DummySearchSummary()

        agent._FAQAgent__web_search_skill = DummyWebSearch()
        result = await agent.process(FAQInput(question="请联网搜索 AgentFlow 最新信息"))

        assert result.query_type == "external"
        assert "来源" in result.answer
        assert len(result.documents) == 1
        mock_rag.execute.assert_not_called()

    @pytest.mark.asyncio
    async def test_process_faq_adds_verification(self, agent: FAQAgent) -> None:
        """FAQ response includes verification status."""
        agent._llm = _llm_mock("faq")
        agent._services_initialized = True

        class DummyRAG:
            async def execute(self, **_: str) -> ServiceResult:
                return ServiceResult(
                    success=True,
                    data={
                        "answer": "根据制度，年假可以结转。",
                        "documents": [
                            {"id": "doc-1", "content": "年假可结转2年", "source": "hr_policy.md", "score": 0.71}
                        ],
                    },
                )

        agent._FAQAgent__rag_service = DummyRAG()
        result = await agent.process(FAQInput(question="年假可以结转吗"))

        assert result.query_type == "faq"
        assert result.verification.get("status") == "supported"
        assert result.error == ""


# ---------------------------------------------------------------------------
# 4. run_stream tests
# ---------------------------------------------------------------------------


class TestRunStream:
    """run_stream integration behavior."""

    @pytest.mark.asyncio
    async def test_run_stream_awaits_query_classification(self, agent: FAQAgent) -> None:
        """Routing progress should contain concrete query_type, not coroutine object."""
        agent._llm = _llm_mock("chat")
        events = [event async for event in agent.run_stream({"question": "hello"})]

        routing_event = next(
            event
            for event in events
            if event.get("type") == "progress" and "ルーティング完了" in str(event.get("message", ""))
        )
        assert routing_event["type"] == "progress"
        assert "coroutine object" not in routing_event["message"]
        assert "chat" in routing_event["message"]
        assert routing_event["agent"] == "intent_router"
        assert routing_event["query_type"] == "chat"


# ---------------------------------------------------------------------------
# 5. Edge cases & robustness
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """Edge cases and robustness tests."""

    @pytest.mark.asyncio
    async def test_unicode_question_routing(self, agent: FAQAgent) -> None:
        """Unicode input routed correctly via LLM."""
        agent._llm = _llm_mock("chat")
        assert await agent._classify_query("嗨") == "chat"

    @pytest.mark.asyncio
    async def test_mixed_case_routing(self, agent: FAQAgent) -> None:
        """Mixed case input routed correctly."""
        agent._llm = _llm_mock("chat")
        assert await agent._classify_query("HELLO") == "chat"

    @pytest.mark.asyncio
    async def test_whitespace_handling(self, agent: FAQAgent) -> None:
        """Leading/trailing whitespace does not break routing."""
        agent._llm = _llm_mock("chat")
        assert await agent._classify_query("  hello  ") == "chat"

    @pytest.mark.asyncio
    async def test_question_with_punctuation(self, agent: FAQAgent) -> None:
        """Punctuation does not break routing."""
        agent._llm = _llm_mock("chat")
        assert await agent._classify_query("你是谁?") == "chat"

    @pytest.mark.asyncio
    async def test_multiline_question_faq(self, agent: FAQAgent) -> None:
        """Multiline question routed to faq."""
        agent._llm = _llm_mock("faq")
        q = "有給休暇について教えてください。\n特に繰り越しルールが知りたいです。"
        assert await agent._classify_query(q) == "faq"

    @pytest.mark.asyncio
    async def test_chat_handles_llm_exception(self, agent: FAQAgent) -> None:
        """LLM error during process is caught gracefully."""
        mock = MagicMock()
        mock.complete = AsyncMock(side_effect=RuntimeError("LLM down"))
        agent._llm = mock
        agent._services_initialized = True

        result = await agent.process(FAQInput(question="你是谁"))
        assert result.error != ""


# ---------------------------------------------------------------------------
# 6. IntentRouter integration
# ---------------------------------------------------------------------------


class TestIntentRouterIntegration:
    """Verify agentflow IntentRouter integration."""

    @pytest.mark.asyncio
    async def test_intent_router_classifies_casual_chat(self) -> None:
        """IntentRouter does not classify greeting as TASK_EXECUTION."""
        router = IntentRouter()
        intent = await router.route("こんにちは")
        assert intent.category != IntentCategory.TASK_EXECUTION

    @pytest.mark.asyncio
    async def test_intent_router_classifies_info_query(self) -> None:
        """IntentRouter classifies info query as INFORMATION_QUERY."""
        router = IntentRouter()
        intent = await router.route("有給休暇は何日ですか?")
        assert intent.category == IntentCategory.INFORMATION_QUERY

    @pytest.mark.asyncio
    async def test_intent_router_classifies_task(self) -> None:
        """IntentRouter classifies task request as TASK_EXECUTION."""
        router = IntentRouter()
        intent = await router.route("営業資料を作成してください")
        assert intent.category == IntentCategory.TASK_EXECUTION
