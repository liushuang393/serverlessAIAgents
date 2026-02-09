# -*- coding: utf-8 -*-
"""Unit tests for RLM context_ops module."""

import pytest

from agentflow.context.rlm.context_ops import (
    ContextOps,
    create_context_ops_prompt,
)
from agentflow.context.rlm.context_store import ContextStore


class TestContextOps:
    """Tests for ContextOps."""

    @pytest.fixture
    def store(self) -> ContextStore:
        """Create a context store."""
        return ContextStore()

    @pytest.fixture
    def ops(self, store: ContextStore) -> ContextOps:
        """Create context ops with store."""
        return ContextOps(store)

    @pytest.mark.asyncio
    async def test_ctx_peek_success(self, store: ContextStore, ops: ContextOps) -> None:
        """Test successful peek operation."""
        content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
        handle = await store.store(content)

        result = ops.ctx_peek(handle.handle_id, start_line=1, num_lines=2)

        assert result["success"] is True
        assert "Line 2" in result["content"]
        assert "Line 3" in result["content"]
        assert "execution_time_ms" in result

    @pytest.mark.asyncio
    async def test_ctx_peek_invalid_handle(self, ops: ContextOps) -> None:
        """Test peek with invalid handle."""
        result = ops.ctx_peek("ctx_invalid", 0, 10)

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_ctx_regex_find_success(self, store: ContextStore, ops: ContextOps) -> None:
        """Test successful regex find."""
        content = "def foo():\n    pass\ndef bar():\n    return 1"
        handle = await store.store(content)

        result = ops.ctx_regex_find(handle.handle_id, r"def \w+", max_matches=10)

        assert result["success"] is True
        assert result["match_count"] == 2
        assert len(result["matches"]) == 2

    @pytest.mark.asyncio
    async def test_ctx_regex_find_no_matches(self, store: ContextStore, ops: ContextOps) -> None:
        """Test regex find with no matches."""
        content = "Just plain text"
        handle = await store.store(content)

        result = ops.ctx_regex_find(handle.handle_id, r"def \w+")

        assert result["success"] is True
        assert result["match_count"] == 0

    @pytest.mark.asyncio
    async def test_ctx_regex_find_invalid_pattern(
        self, store: ContextStore, ops: ContextOps
    ) -> None:
        """Test regex find with invalid pattern."""
        content = "Some content"
        handle = await store.store(content)

        result = ops.ctx_regex_find(handle.handle_id, r"[invalid")

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_ctx_keyword_find_or(self, store: ContextStore, ops: ContextOps) -> None:
        """Test keyword find with OR operator."""
        content = "Apple is red\nBanana is yellow\nOrange is orange"
        handle = await store.store(content)

        result = ops.ctx_keyword_find(handle.handle_id, ["red", "yellow"], operator="OR")

        assert result["success"] is True
        assert result["match_count"] == 2

    @pytest.mark.asyncio
    async def test_ctx_keyword_find_and(self, store: ContextStore, ops: ContextOps) -> None:
        """Test keyword find with AND operator."""
        content = "Apple is red and tasty\nBanana is yellow\nCherry is red"
        handle = await store.store(content)

        result = ops.ctx_keyword_find(handle.handle_id, ["is", "red"], operator="AND")

        assert result["success"] is True
        assert result["match_count"] == 2  # Apple and Cherry lines

    @pytest.mark.asyncio
    async def test_ctx_keyword_find_invalid_handle(self, ops: ContextOps) -> None:
        """Test keyword find with invalid handle."""
        result = ops.ctx_keyword_find("ctx_invalid", ["test"])

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_ctx_get_structure(self, store: ContextStore, ops: ContextOps) -> None:
        """Test structure retrieval."""
        content = "# Heading 1\n\nContent\n\n## Heading 2\n\nMore content"
        handle = await store.store(content)

        result = ops.ctx_get_structure(handle.handle_id)

        assert result["success"] is True
        assert "outline" in result
        assert len(result["headings"]) == 2

    @pytest.mark.asyncio
    async def test_ctx_get_structure_with_code_blocks(
        self, store: ContextStore, ops: ContextOps
    ) -> None:
        """Test structure retrieval with code blocks."""
        content = "# Title\n\n```python\ndef foo():\n    pass\n```\n\nText"
        handle = await store.store(content)

        result = ops.ctx_get_structure(handle.handle_id)

        assert result["success"] is True
        assert len(result["code_blocks"]) == 1
        assert result["code_blocks"][0]["language"] == "python"

    @pytest.mark.asyncio
    async def test_ctx_get_summary(self, store: ContextStore, ops: ContextOps) -> None:
        """Test summary retrieval."""
        content = "# Test Document\n\nThis is a test."
        handle = await store.store(content)

        result = ops.ctx_get_summary(handle.handle_id)

        assert result["success"] is True
        assert result["handle_id"] == handle.handle_id
        assert result["total_lines"] > 0

    @pytest.mark.asyncio
    async def test_to_tool_definitions(self, ops: ContextOps) -> None:
        """Test tool definition generation."""
        definitions = ops.to_tool_definitions()

        assert len(definitions) == 4
        tool_names = [d["function"]["name"] for d in definitions]
        assert "ctx_peek" in tool_names
        assert "ctx_regex_find" in tool_names
        assert "ctx_keyword_find" in tool_names
        assert "ctx_get_structure" in tool_names

    @pytest.mark.asyncio
    async def test_execute_tool_peek(self, store: ContextStore, ops: ContextOps) -> None:
        """Test execute_tool for peek."""
        content = "Line 1\nLine 2\nLine 3"
        handle = await store.store(content)

        result = ops.execute_tool("ctx_peek", {"handle_id": handle.handle_id, "num_lines": 2})

        assert result["success"] is True

    @pytest.mark.asyncio
    async def test_execute_tool_unknown(self, ops: ContextOps) -> None:
        """Test execute_tool with unknown tool."""
        result = ops.execute_tool("unknown_tool", {})

        assert result["success"] is False
        assert "Unknown tool" in result["error"]


class TestCreateContextOpsPrompt:
    """Tests for create_context_ops_prompt."""

    def test_prompt_generation(self) -> None:
        """Test prompt generation."""
        handles = [
            {
                "handle_id": "ctx_abc123",
                "total_lines": 100,
                "total_tokens": 5000,
                "summary": "API documentation",
            },
            {
                "handle_id": "ctx_def456",
                "total_lines": 50,
                "total_tokens": 2500,
                "summary": "User guide",
            },
        ]

        prompt = create_context_ops_prompt(handles)

        assert "ctx_abc123" in prompt
        assert "ctx_def456" in prompt
        assert "API documentation" in prompt
        assert "ctx_peek" in prompt
        assert "ctx_regex_find" in prompt

    def test_prompt_empty_handles(self) -> None:
        """Test prompt with empty handles."""
        prompt = create_context_ops_prompt([])

        assert "Available Contexts" in prompt
        assert "ctx_peek" in prompt

    def test_prompt_includes_strategy(self) -> None:
        """Test prompt includes strategy section."""
        prompt = create_context_ops_prompt([])

        assert "Strategy" in prompt
        assert "deterministic" in prompt.lower() or "low-cost" in prompt.lower()
