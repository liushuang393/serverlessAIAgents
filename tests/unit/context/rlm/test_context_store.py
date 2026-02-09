# -*- coding: utf-8 -*-
"""Unit tests for RLM context_store module."""

import pytest

from agentflow.context.rlm.context_store import (
    ContextHandle,
    ContextStore,
    StructureInfo,
)


class TestContextHandle:
    """Tests for ContextHandle."""

    def test_creation(self) -> None:
        """Test handle creation."""
        handle = ContextHandle(
            handle_id="ctx_test123",
            total_tokens=1000,
            total_lines=50,
            total_chars=5000,
            summary="Test summary",
        )

        assert handle.handle_id == "ctx_test123"
        assert handle.total_tokens == 1000
        assert handle.total_lines == 50
        assert handle.total_chars == 5000
        assert handle.summary == "Test summary"

    def test_to_prompt_reference(self) -> None:
        """Test prompt reference generation."""
        handle = ContextHandle(
            handle_id="ctx_abc",
            total_tokens=500,
            total_lines=25,
            total_chars=2500,
            summary="A test document",
        )

        ref = handle.to_prompt_reference()
        assert "ctx_abc" in ref
        assert "25" in ref  # lines
        assert "500" in ref  # tokens
        assert "A test document" in ref

    def test_to_dict(self) -> None:
        """Test dictionary conversion."""
        handle = ContextHandle(
            handle_id="ctx_test",
            total_tokens=100,
            total_lines=10,
            total_chars=500,
        )

        d = handle.to_dict()
        assert d["handle_id"] == "ctx_test"
        assert d["total_tokens"] == 100
        assert d["total_lines"] == 10
        assert "created_at" in d


class TestStructureInfo:
    """Tests for StructureInfo."""

    def test_empty_structure(self) -> None:
        """Test empty structure."""
        structure = StructureInfo()
        assert structure.headings == []
        assert structure.sections == []
        assert structure.code_blocks == []
        assert structure.tables == []

    def test_to_outline_with_headings(self) -> None:
        """Test outline generation with headings."""
        structure = StructureInfo(
            headings=[
                (1, 1, "Introduction"),
                (5, 2, "Background"),
                (10, 2, "Methods"),
                (20, 3, "Detailed Methods"),
            ]
        )

        outline = structure.to_outline(max_depth=2)
        assert "Introduction" in outline
        assert "Background" in outline
        assert "Methods" in outline
        assert "Detailed Methods" not in outline  # depth 3 excluded

    def test_to_outline_empty(self) -> None:
        """Test outline generation with no headings."""
        structure = StructureInfo()
        outline = structure.to_outline()
        assert "No headings found" in outline


class TestContextStore:
    """Tests for ContextStore."""

    @pytest.fixture
    def store(self) -> ContextStore:
        """Create a fresh store for each test."""
        return ContextStore()

    @pytest.mark.asyncio
    async def test_store_content(self, store: ContextStore) -> None:
        """Test storing content."""
        content = "# Test Document\n\nThis is test content."
        handle = await store.store(content)

        assert handle.handle_id.startswith("ctx_")
        assert handle.total_lines == 3
        assert handle.total_chars == len(content)

    @pytest.mark.asyncio
    async def test_store_deduplication(self, store: ContextStore) -> None:
        """Test content deduplication."""
        content = "Same content"
        handle1 = await store.store(content)
        handle2 = await store.store(content)

        # Same content should return same handle
        assert handle1.handle_id == handle2.handle_id

    @pytest.mark.asyncio
    async def test_store_no_deduplication(self, store: ContextStore) -> None:
        """Test storing without deduplication."""
        content = "Same content"
        handle1 = await store.store(content, deduplicate=False)
        handle2 = await store.store(content, deduplicate=False)

        # Should create separate handles
        assert handle1.handle_id != handle2.handle_id

    @pytest.mark.asyncio
    async def test_get_handle(self, store: ContextStore) -> None:
        """Test getting handle by ID."""
        content = "Test content"
        handle = await store.store(content)

        retrieved = store.get_handle(handle.handle_id)
        assert retrieved is not None
        assert retrieved.handle_id == handle.handle_id

    @pytest.mark.asyncio
    async def test_get_handle_not_found(self, store: ContextStore) -> None:
        """Test getting non-existent handle."""
        retrieved = store.get_handle("ctx_nonexistent")
        assert retrieved is None

    @pytest.mark.asyncio
    async def test_peek(self, store: ContextStore) -> None:
        """Test peeking at content."""
        content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
        handle = await store.store(content)

        result = store.peek(handle.handle_id, start_line=1, num_lines=2)
        assert "Line 2" in result
        assert "Line 3" in result
        assert "Line 1" not in result

    @pytest.mark.asyncio
    async def test_peek_with_line_numbers(self, store: ContextStore) -> None:
        """Test that peek includes line numbers."""
        content = "First\nSecond\nThird"
        handle = await store.store(content)

        result = store.peek(handle.handle_id, 0, 3)
        assert "1|" in result or "1 |" in result  # Line number formatting

    @pytest.mark.asyncio
    async def test_peek_not_found(self, store: ContextStore) -> None:
        """Test peek with invalid handle."""
        with pytest.raises(KeyError):
            store.peek("ctx_invalid", 0, 10)

    @pytest.mark.asyncio
    async def test_regex_find(self, store: ContextStore) -> None:
        """Test regex search."""
        content = "def foo():\n    pass\ndef bar():\n    return 1"
        handle = await store.store(content)

        matches = store.regex_find(handle.handle_id, r"def \w+")
        assert len(matches) == 2
        assert matches[0]["match"] == "def foo"
        assert matches[1]["match"] == "def bar"

    @pytest.mark.asyncio
    async def test_regex_find_with_context(self, store: ContextStore) -> None:
        """Test regex search with context lines."""
        content = "Line 1\nLine 2\nTarget Line\nLine 4\nLine 5"
        handle = await store.store(content)

        matches = store.regex_find(handle.handle_id, r"Target", context_lines=1)
        assert len(matches) == 1
        assert "Line 2" in matches[0]["context"]
        assert "Line 4" in matches[0]["context"]

    @pytest.mark.asyncio
    async def test_regex_find_invalid_pattern(self, store: ContextStore) -> None:
        """Test regex search with invalid pattern."""
        content = "Test content"
        handle = await store.store(content)

        matches = store.regex_find(handle.handle_id, r"[invalid")
        assert len(matches) == 1
        assert "error" in matches[0]

    @pytest.mark.asyncio
    async def test_keyword_find_or(self, store: ContextStore) -> None:
        """Test keyword search with OR operator."""
        content = "Apple is red\nBanana is yellow\nCherry is red"
        handle = await store.store(content)

        matches = store.keyword_find(handle.handle_id, ["red", "yellow"], operator="OR")
        assert len(matches) == 3

    @pytest.mark.asyncio
    async def test_keyword_find_and(self, store: ContextStore) -> None:
        """Test keyword search with AND operator."""
        content = "Apple is red\nBanana is yellow\nCherry is red"
        handle = await store.store(content)

        matches = store.keyword_find(handle.handle_id, ["is", "red"], operator="AND")
        assert len(matches) == 2  # Apple and Cherry lines

    @pytest.mark.asyncio
    async def test_keyword_find_case_insensitive(self, store: ContextStore) -> None:
        """Test keyword search is case insensitive."""
        content = "APPLE is RED"
        handle = await store.store(content)

        matches = store.keyword_find(handle.handle_id, ["apple", "red"])
        assert len(matches) == 1

    @pytest.mark.asyncio
    async def test_get_structure_headings(self, store: ContextStore) -> None:
        """Test structure extraction for headings."""
        content = "# Heading 1\n\n## Heading 2\n\nContent\n\n### Heading 3"
        handle = await store.store(content)

        structure = store.get_structure(handle.handle_id)
        assert len(structure.headings) == 3
        assert structure.headings[0][2] == "Heading 1"
        assert structure.headings[1][2] == "Heading 2"

    @pytest.mark.asyncio
    async def test_get_structure_code_blocks(self, store: ContextStore) -> None:
        """Test structure extraction for code blocks."""
        content = "Text\n```python\ndef foo():\n    pass\n```\nMore text"
        handle = await store.store(content)

        structure = store.get_structure(handle.handle_id)
        assert len(structure.code_blocks) == 1
        assert structure.code_blocks[0][2] == "python"

    @pytest.mark.asyncio
    async def test_get_full_content(self, store: ContextStore) -> None:
        """Test getting full content."""
        content = "Full content here"
        handle = await store.store(content)

        full = store.get_full_content(handle.handle_id)
        assert full == content

    @pytest.mark.asyncio
    async def test_delete(self, store: ContextStore) -> None:
        """Test deleting content."""
        content = "To be deleted"
        handle = await store.store(content)

        assert store.delete(handle.handle_id) is True
        assert store.get_handle(handle.handle_id) is None

    @pytest.mark.asyncio
    async def test_delete_not_found(self, store: ContextStore) -> None:
        """Test deleting non-existent content."""
        assert store.delete("ctx_nonexistent") is False

    @pytest.mark.asyncio
    async def test_list_handles(self, store: ContextStore) -> None:
        """Test listing all handles."""
        await store.store("Content 1")
        await store.store("Content 2")
        await store.store("Content 3")

        handles = store.list_handles()
        assert len(handles) == 3

    @pytest.mark.asyncio
    async def test_clear(self, store: ContextStore) -> None:
        """Test clearing all content."""
        await store.store("Content 1")
        await store.store("Content 2")

        store.clear()
        assert len(store.list_handles()) == 0

    @pytest.mark.asyncio
    async def test_get_stats(self, store: ContextStore) -> None:
        """Test getting statistics."""
        await store.store("Short")
        await store.store("A longer piece of content")

        stats = store.get_stats()
        assert stats["context_count"] == 2
        assert stats["total_chars"] > 0
        assert stats["total_lines"] >= 2

    @pytest.mark.asyncio
    async def test_summary_generation(self, store: ContextStore) -> None:
        """Test automatic summary generation."""
        content = "# API Documentation\n\nThis document describes the API."
        handle = await store.store(content)

        assert "API Documentation" in handle.summary or "API" in handle.summary
