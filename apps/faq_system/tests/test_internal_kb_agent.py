# apps/faq_system/tests/test_internal_kb_agent.py
"""Tests for InternalKBAgent LLM integration and bug fixes."""

from __future__ import annotations

from typing import Any
from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.faq_system.backend.agents.internal_kb_agent import InternalKBAgent, InternalKBConfig

from agentflow.integrations.ticket_generator import TicketGenerator
from agentflow.security.policy_engine import PolicyEngine


# ---------------------------------------------------------------------------
# Concrete subclass to satisfy abstract methods
# ---------------------------------------------------------------------------


class _ConcreteInternalKBAgent(InternalKBAgent):
    """Concrete subclass that satisfies ResilientAgent abstract requirements for testing."""

    # Abstract methods required by ResilientAgent. Signatures differ intentionally:
    # this stub is only used for direct unit-testing of private methods (_generate_answer etc.)
    # and never goes through the base class run() path that calls output.model_dump().
    def _parse_input(self, input_data: dict[str, Any]) -> Any:  # type: ignore[override]
        return input_data

    async def process(self, input_data: Any) -> Any:  # type: ignore[override]
        return input_data


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_agent() -> _ConcreteInternalKBAgent:
    """Create an InternalKBAgent instance with initialization bypassed."""
    agent = _ConcreteInternalKBAgent(
        config=InternalKBConfig(),
        kb_manager=None,
        ticket_generator=TicketGenerator(),
        policy_engine=PolicyEngine(),
    )
    agent._initialized = True  # skip _ensure_initialized network calls
    agent._llm = None  # default: no LLM
    return agent


def _make_search_results(n: int = 2) -> list[dict]:
    results = []
    for i in range(n):
        results.append(
            {
                "document_id": f"doc_{i}",
                "content": f"テストコンテンツ {i} " + "A" * 100,
                "score": 0.8 - i * 0.1,
                "metadata": {},
                "citation": {
                    "title": f"ドキュメント {i}",
                    "source": f"source_{i}",
                    "version": "1.0",
                    "update_date": "2024-01-01",
                },
            }
        )
    return results


# ---------------------------------------------------------------------------
# Test 1: _generate_answer calls LLM when available
# ---------------------------------------------------------------------------


class TestGenerateAnswerCallsLLM:
    @pytest.mark.asyncio
    async def test_generate_answer_calls_llm(self) -> None:
        """_generate_answer should call self._llm.chat and use its content as answer."""
        agent = _make_agent()

        mock_llm = MagicMock()
        mock_llm.chat = AsyncMock(return_value={"content": "LLMが生成した回答です。", "model": "test", "usage": {}})
        agent._llm = mock_llm

        search_results = _make_search_results(2)
        response = await agent._generate_answer("年次有給休暇は？", search_results)

        # chat must have been called once
        mock_llm.chat.assert_called_once()
        call_args = mock_llm.chat.call_args

        # First positional arg is the messages list
        messages = call_args[0][0]
        assert isinstance(messages, list)
        assert len(messages) == 2

        # System message uses SYSTEM_PROMPT
        assert messages[0]["role"] == "system"
        assert messages[0]["content"] == InternalKBAgent.SYSTEM_PROMPT

        # User message contains the question
        assert messages[1]["role"] == "user"
        assert "年次有給休暇は？" in messages[1]["content"]

        # Answer should come from LLM response
        assert response.answer == "LLMが生成した回答です。"


# ---------------------------------------------------------------------------
# Test 2: _generate_answer falls back when LLM is None
# ---------------------------------------------------------------------------


class TestGenerateAnswerFallback:
    @pytest.mark.asyncio
    async def test_generate_answer_fallback_when_no_llm(self) -> None:
        """When self._llm is None, _generate_answer should use snippet fallback."""
        agent = _make_agent()
        agent._llm = None

        search_results = _make_search_results(1)
        response = await agent._generate_answer("質問", search_results)

        # Answer should be the snippet-based fallback (not an LLM response)
        assert "参考情報 [1]" in response.answer
        assert search_results[0]["content"][:100] in response.answer


# ---------------------------------------------------------------------------
# Test 3: _generate_conservative_answer uses CONSERVATIVE_SYSTEM_PROMPT
# ---------------------------------------------------------------------------


class TestGenerateConservativeAnswerCallsLLM:
    @pytest.mark.asyncio
    async def test_generate_conservative_answer_calls_llm(self) -> None:
        """_generate_conservative_answer should use CONSERVATIVE_SYSTEM_PROMPT when LLM available."""
        agent = _make_agent()

        mock_llm = MagicMock()
        mock_llm.chat = AsyncMock(return_value={"content": "保守モードLLM回答", "model": "test", "usage": {}})
        agent._llm = mock_llm

        search_results = _make_search_results(1)
        response = await agent._generate_conservative_answer("就業規則について", search_results)

        mock_llm.chat.assert_called_once()
        call_args = mock_llm.chat.call_args
        messages = call_args[0][0]

        # System message must be CONSERVATIVE_SYSTEM_PROMPT
        assert messages[0]["role"] == "system"
        assert messages[0]["content"] == InternalKBAgent.CONSERVATIVE_SYSTEM_PROMPT

        # Answer from LLM
        assert response.answer == "保守モードLLM回答"


# ---------------------------------------------------------------------------
# Test 4: run_stream uses question in first progress message
# ---------------------------------------------------------------------------


class TestRunStreamUsesQuestionInMessage:
    @pytest.mark.asyncio
    async def test_run_stream_uses_question_in_message(self) -> None:
        """The first progress event (progress=0) should contain the question text."""
        agent = _make_agent()

        # Mock _ensure_initialized and run to avoid real I/O
        agent._ensure_initialized = AsyncMock()
        agent.run = AsyncMock(
            return_value={
                "question": "テスト質問です",
                "answer": "回答",
                "citations": [],
                "confidence": 0.9,
                "needs_confirmation": False,
                "suggested_contacts": [],
                "ticket_id": None,
                "query_type": "faq",
                "execution_time_ms": 10.0,
                "conservative_mode_used": False,
                "error": "",
            }
        )

        question = "テスト質問です"
        events = []
        async for event in agent.run_stream({"question": question}):
            events.append(event)

        # The first event should be a progress=0 event
        first_event = events[0]
        assert first_event["type"] == "progress"
        assert first_event["progress"] == 0

        # The message should contain the question (up to 20 chars)
        assert question[:20] in first_event["message"]
