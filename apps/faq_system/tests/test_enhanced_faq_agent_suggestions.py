"""Tests for EnhancedFAQAgent._generate_suggestions delegating to SuggestionService."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.faq_system.backend.agents.enhanced_faq_agent import (
    EnhancedFAQAgent,
    EnhancedFAQConfig,
)


@pytest.fixture
def agent() -> EnhancedFAQAgent:
    """EnhancedFAQAgent instance with initialization skipped."""
    a = EnhancedFAQAgent(config=EnhancedFAQConfig())
    a._initialized = True  # skip _ensure_initialized
    return a


@pytest.mark.asyncio
async def test_generate_suggestions_uses_suggestion_service(agent: EnhancedFAQAgent) -> None:
    """SuggestionService.execute が呼ばれ、結果が返されることを確認."""
    mock_result = MagicMock()
    mock_result.success = True
    mock_result.data = {"suggestions": [{"text": "提案1", "type": "followup"}]}

    mock_service = MagicMock()
    mock_service.execute = AsyncMock(return_value=mock_result)

    agent._suggestion_service = mock_service

    result = await agent._generate_suggestions("テスト質問", "faq")

    mock_service.execute.assert_called_once_with(
        action="suggest",
        question="テスト質問",
        query_type="faq",
    )
    assert result == [{"text": "提案1", "type": "followup"}]


@pytest.mark.asyncio
async def test_generate_suggestions_fallback_on_failure(agent: EnhancedFAQAgent) -> None:
    """SuggestionService.execute が例外を送出した場合、フォールバックが返されることを確認."""
    mock_service = MagicMock()
    mock_service.execute = AsyncMock(side_effect=Exception("error"))

    agent._suggestion_service = mock_service

    result = await agent._generate_suggestions("テスト質問", "faq")

    # フォールバック提案が返る
    assert result == [
        {"text": "もう少し詳しく教えて", "type": "followup"},
        {"text": "関連する情報は？", "type": "followup"},
        {"text": "例を見せて", "type": "followup"},
    ]


@pytest.mark.asyncio
async def test_generate_suggestions_fallback_when_no_service(agent: EnhancedFAQAgent) -> None:
    """SuggestionService が None の場合、faq フォールバックが返されることを確認."""
    agent._suggestion_service = None

    result = await agent._generate_suggestions("テスト質問", "faq")

    assert result == [
        {"text": "もう少し詳しく教えて", "type": "followup"},
        {"text": "関連する情報は？", "type": "followup"},
        {"text": "例を見せて", "type": "followup"},
    ]


@pytest.mark.asyncio
async def test_generate_suggestions_sql_fallback(agent: EnhancedFAQAgent) -> None:
    """SuggestionService が None かつ query_type='sql' の場合、SQL フォールバックが返されることを確認."""
    agent._suggestion_service = None

    result = await agent._generate_suggestions("売上データを見せて", "sql")

    assert result == [
        {"text": "前月との比較を見せて", "type": "followup"},
        {"text": "カテゴリ別の内訳は？", "type": "followup"},
        {"text": "トップ10を表示", "type": "followup"},
    ]
