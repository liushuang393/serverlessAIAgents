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
        # source uses fullwidth punctuation in the fallback string
        assert "こんにちは" in result.answer
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


# ---------------------------------------------------------------------------
# 4. run_stream tests
# ---------------------------------------------------------------------------


class TestRunStream:
    """run_stream integration behavior."""

    @pytest.mark.asyncio
    async def test_run_stream_awaits_query_classification(self, agent: FAQAgent) -> None:
        """Routing progress should contain concrete query_type, not coroutine object."""
        agent._llm = _llm_mock("chat")
        agent.run = AsyncMock(return_value={"answer": "ok"})  # type: ignore[method-assign]

        events = [event async for event in agent.run_stream({"question": "hello"})]

        routing_event = events[1]
        assert routing_event["type"] == "progress"
        assert "coroutine object" not in routing_event["message"]
        assert "chat" in routing_event["message"]


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
