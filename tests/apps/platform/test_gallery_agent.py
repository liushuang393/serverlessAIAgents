"""Unit tests for GalleryAgent.

Note: These tests are in tests/apps/platform/ instead of apps/platform/tests/
to avoid Python's built-in 'platform' module name conflict.
"""

from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.platform.agents.gallery_agent import GalleryAgent


class TestGalleryAgentInit:
    """Test cases for GalleryAgent initialization."""

    def test_agent_creation(self) -> None:
        """Test creating a GalleryAgent instance."""
        agent = GalleryAgent()
        assert agent.name == "gallery-agent"
        assert agent._gallery is not None

    def test_agent_has_system_prompt(self) -> None:
        """Test that agent has system prompt."""
        agent = GalleryAgent()
        prompt = agent._get_system_prompt()
        assert len(prompt) > 0


class TestGalleryAgentRun:
    """Test cases for GalleryAgent.run method."""

    @pytest.mark.asyncio
    async def test_run_without_query_returns_error(self) -> None:
        """Test that run returns error when no query provided."""
        agent = GalleryAgent()
        result = await agent.run({})

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_run_with_empty_query_returns_error(self) -> None:
        """Test that run returns error when query is empty."""
        agent = GalleryAgent()
        result = await agent.run({"query": ""})

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_run_with_valid_query(self) -> None:
        """Test that run returns results for valid query."""
        agent = GalleryAgent()

        # Mock the gallery service
        mock_result = MagicMock()
        mock_result.items = []
        mock_result.total = 0
        mock_result.has_more = False
        agent._gallery.search = AsyncMock(return_value=mock_result)

        result = await agent.run({"query": "PDF処理Agent"})

        assert result["success"] is True
        assert result["query"] == "PDF処理Agent"
        assert "items" in result
        assert "total" in result
