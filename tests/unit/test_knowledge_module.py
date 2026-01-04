# -*- coding: utf-8 -*-
"""Knowledge 模块的完整测试.

覆盖 document_loader.py, rag_pipeline.py, hooks.py
"""

import asyncio
import tempfile
import unittest
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


class TestDocumentChunk(unittest.TestCase):
    """DocumentChunk 类的完整测试."""

    def test_create_with_defaults(self):
        """默认参数创建测试."""
        from agentflow.knowledge.document_loader import DocumentChunk

        chunk = DocumentChunk.create(content="Test content")
        self.assertIsNotNone(chunk.id)
        self.assertEqual(chunk.content, "Test content")
        self.assertEqual(chunk.source, "")
        self.assertEqual(chunk.chunk_index, 0)
        self.assertEqual(chunk.total_chunks, 1)
        self.assertEqual(chunk.metadata, {})
        self.assertIsNone(chunk.embedding)

    def test_create_with_all_params(self):
        """全参数创建测试."""
        from agentflow.knowledge.document_loader import DocumentChunk

        chunk = DocumentChunk.create(
            content="Test content",
            source="test.txt",
            metadata={"key": "value"},
            chunk_index=2,
            total_chunks=5,
        )

        self.assertIn("_2", chunk.id)
        self.assertEqual(chunk.metadata, {"key": "value"})
        self.assertEqual(chunk.chunk_index, 2)
        self.assertEqual(chunk.total_chunks, 5)


class TestChunkingConfig(unittest.TestCase):
    """ChunkingConfig 类的测试."""

    def test_default_values(self):
        """默认值测试."""
        from agentflow.knowledge.document_loader import ChunkingConfig

        config = ChunkingConfig()
        self.assertEqual(config.chunk_size, 1000)
        self.assertEqual(config.chunk_overlap, 200)
        self.assertEqual(config.separator, "\n\n")
        self.assertTrue(config.keep_separator)

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.knowledge.document_loader import ChunkingConfig

        config = ChunkingConfig(
            chunk_size=500,
            chunk_overlap=50,
            separator="\n",
            keep_separator=False,
        )
        self.assertEqual(config.chunk_size, 500)
        self.assertEqual(config.chunk_overlap, 50)


class TestTextLoader(unittest.TestCase):
    """TextLoader 的完整测试."""

    def test_supports_txt(self):
        """支持 .txt 文件."""
        from agentflow.knowledge.document_loader import TextLoader

        loader = TextLoader()
        self.assertTrue(loader.supports("file.txt"))
        self.assertTrue(loader.supports("file.text"))
        self.assertFalse(loader.supports("file.md"))
        self.assertFalse(loader.supports("file.pdf"))

    def test_load_text_file(self):
        """加载文本文件测试."""
        from agentflow.knowledge.document_loader import TextLoader

        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
            f.write("This is test content.\n\nThis is paragraph two.")
            f.flush()

            loader = TextLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertGreater(len(chunks), 0)
                self.assertIn("test content", chunks[0].content)
                self.assertEqual(chunks[0].metadata["loader"], "text")
            finally:
                loop.close()
                Path(f.name).unlink()


class TestMarkdownLoader(unittest.TestCase):
    """MarkdownLoader 的完整测试."""

    def test_supports_markdown(self):
        """支持 .md 文件."""
        from agentflow.knowledge.document_loader import MarkdownLoader

        loader = MarkdownLoader()
        self.assertTrue(loader.supports("file.md"))
        self.assertTrue(loader.supports("file.markdown"))
        self.assertFalse(loader.supports("file.txt"))

    def test_load_markdown_file(self):
        """加载 Markdown 文件测试."""
        from agentflow.knowledge.document_loader import MarkdownLoader

        content = """# Header 1

This is paragraph one.

## Header 2

This is paragraph two.
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(content)
            f.flush()

            loader = MarkdownLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertGreater(len(chunks), 0)
                self.assertEqual(chunks[0].metadata["loader"], "markdown")
            finally:
                loop.close()
                Path(f.name).unlink()


class TestCSVLoader(unittest.TestCase):
    """CSVLoader 的完整测试."""

    def test_supports_csv(self):
        """支持 .csv 文件."""
        from agentflow.knowledge.document_loader import CSVLoader

        loader = CSVLoader()
        self.assertTrue(loader.supports("file.csv"))
        self.assertTrue(loader.supports("file.tsv"))
        self.assertFalse(loader.supports("file.txt"))

    def test_load_csv_file(self):
        """加载 CSV 文件测试."""
        from agentflow.knowledge.document_loader import CSVLoader

        content = """name,description
Item1,Description of item 1
Item2,Description of item 2
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            f.write(content)
            f.flush()

            loader = CSVLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertEqual(len(chunks), 2)
                self.assertEqual(chunks[0].metadata["loader"], "csv")
                self.assertEqual(chunks[0].metadata["row_index"], 0)
            finally:
                loop.close()
                Path(f.name).unlink()

    def test_load_csv_with_content_columns(self):
        """指定内容列加载 CSV."""
        from agentflow.knowledge.document_loader import CSVLoader

        content = """name,description,extra
Item1,Desc1,Extra1
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            f.write(content)
            f.flush()

            loader = CSVLoader(
                content_columns=["description"],
                metadata_columns=["name"]
            )
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertEqual(chunks[0].content, "Desc1")
                self.assertEqual(chunks[0].metadata["name"], "Item1")
            finally:
                loop.close()
                Path(f.name).unlink()


class TestJSONLoader(unittest.TestCase):
    """JSONLoader 的完整测试."""

    def test_supports_json(self):
        """支持 .json 文件."""
        from agentflow.knowledge.document_loader import JSONLoader

        loader = JSONLoader()
        self.assertTrue(loader.supports("file.json"))
        self.assertTrue(loader.supports("file.jsonl"))
        self.assertFalse(loader.supports("file.txt"))

    def test_load_json_array(self):
        """加载 JSON 数组."""
        from agentflow.knowledge.document_loader import JSONLoader
        import json

        data = [
            {"name": "Item1", "content": "Content1"},
            {"name": "Item2", "content": "Content2"},
        ]
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump(data, f)
            f.flush()

            loader = JSONLoader(content_key="content", metadata_keys=["name"])
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertEqual(len(chunks), 2)
                self.assertEqual(chunks[0].content, "Content1")
                self.assertEqual(chunks[0].metadata["name"], "Item1")
            finally:
                loop.close()
                Path(f.name).unlink()

    def test_load_json_object(self):
        """加载单个 JSON 对象."""
        from agentflow.knowledge.document_loader import JSONLoader
        import json

        data = {"name": "SingleItem", "content": "SingleContent"}
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump(data, f)
            f.flush()

            loader = JSONLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertEqual(len(chunks), 1)
            finally:
                loop.close()
                Path(f.name).unlink()

    def test_load_jsonl(self):
        """加载 JSONL 文件."""
        from agentflow.knowledge.document_loader import JSONLoader
        import json

        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            f.write(json.dumps({"content": "Line1"}) + "\n")
            f.write(json.dumps({"content": "Line2"}) + "\n")
            f.flush()

            loader = JSONLoader(content_key="content")
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertEqual(len(chunks), 2)
            finally:
                loop.close()
                Path(f.name).unlink()


class TestHTMLLoader(unittest.TestCase):
    """HTMLLoader 的完整测试."""

    def test_supports_html(self):
        """支持 .html 文件."""
        from agentflow.knowledge.document_loader import HTMLLoader

        loader = HTMLLoader()
        self.assertTrue(loader.supports("file.html"))
        self.assertTrue(loader.supports("file.htm"))
        self.assertFalse(loader.supports("file.txt"))

    def test_load_html_file(self):
        """加载 HTML 文件."""
        from agentflow.knowledge.document_loader import HTMLLoader

        html = """<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
<h1>Header</h1>
<p>Paragraph content.</p>
<script>alert('test');</script>
</body>
</html>"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
            f.write(html)
            f.flush()

            loader = HTMLLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertGreater(len(chunks), 0)
                self.assertIn("Paragraph content", chunks[0].content)
                # Script should be removed
                self.assertNotIn("alert", chunks[0].content)
            finally:
                loop.close()
                Path(f.name).unlink()


class TestUniversalLoader(unittest.TestCase):
    """UniversalLoader 的完整测试."""

    def test_auto_select_loader(self):
        """自动选择加载器测试."""
        from agentflow.knowledge.document_loader import UniversalLoader

        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
            f.write("Test content")
            f.flush()

            loader = UniversalLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertGreater(len(chunks), 0)
            finally:
                loop.close()
                Path(f.name).unlink()

    def test_unknown_extension_fallback(self):
        """未知扩展名回退测试."""
        from agentflow.knowledge.document_loader import UniversalLoader

        with tempfile.NamedTemporaryFile(mode="w", suffix=".xyz", delete=False) as f:
            f.write("Unknown format content")
            f.flush()

            loader = UniversalLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(loader.load(f.name))
                self.assertGreater(len(chunks), 0)
            finally:
                loop.close()
                Path(f.name).unlink()

    def test_load_directory(self):
        """加载目录测试."""
        from agentflow.knowledge.document_loader import UniversalLoader

        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            (Path(tmpdir) / "file1.txt").write_text("Content 1")
            (Path(tmpdir) / "file2.txt").write_text("Content 2")

            loader = UniversalLoader()
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            try:
                chunks = loop.run_until_complete(
                    loader.load_directory(tmpdir, pattern="*.txt")
                )
                self.assertEqual(len(chunks), 2)
            finally:
                loop.close()


class TestRAGConfig(unittest.TestCase):
    """RAGConfig 的完整测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.knowledge.rag_pipeline import RAGConfig

        config = RAGConfig()
        self.assertEqual(config.collection_name, "agentflow_rag")
        self.assertEqual(config.top_k, 5)
        self.assertEqual(config.min_similarity, 0.3)
        self.assertEqual(config.max_context_length, 4000)
        self.assertTrue(config.include_sources)

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.knowledge.rag_pipeline import RAGConfig

        config = RAGConfig(
            collection_name="custom",
            top_k=10,
            min_similarity=0.5,
        )
        self.assertEqual(config.collection_name, "custom")
        self.assertEqual(config.top_k, 10)


class TestRAGResult(unittest.TestCase):
    """RAGResult 的完整测试."""

    def test_create_with_defaults(self):
        """默认值创建测试."""
        from agentflow.knowledge.rag_pipeline import RAGResult

        result = RAGResult(answer="Test answer")
        self.assertEqual(result.answer, "Test answer")
        self.assertEqual(result.sources, [])
        self.assertEqual(result.context_used, "")
        self.assertEqual(result.query, "")

    def test_create_with_all_params(self):
        """全参数创建测试."""
        from agentflow.knowledge.rag_pipeline import RAGResult

        result = RAGResult(
            answer="Test answer",
            sources=[{"id": "1"}],
            context_used="context",
            query="question",
            search_results=[{"doc": "test"}],
            metadata={"model": "gpt-4"},
        )
        self.assertEqual(len(result.sources), 1)
        self.assertEqual(result.metadata["model"], "gpt-4")


class TestVectorSearchHook(unittest.TestCase):
    """VectorSearchHook 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.knowledge.hooks import VectorSearchHook

        hook = VectorSearchHook(collection="test", min_similarity=0.5)
        self.assertEqual(hook._collection, "test")
        self.assertEqual(hook._min_similarity, 0.5)

    def test_use_vector_search_factory(self):
        """工厂函数测试."""
        from agentflow.knowledge.hooks import use_vector_search

        hook = use_vector_search(collection="my-docs", min_similarity=0.4)
        self.assertEqual(hook._collection, "my-docs")
        self.assertEqual(hook._min_similarity, 0.4)


class TestRAGHook(unittest.TestCase):
    """RAGHook 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.knowledge.hooks import RAGHook

        hook = RAGHook(collection="test", top_k=10)
        self.assertEqual(hook._collection, "test")
        self.assertEqual(hook._top_k, 10)

    def test_use_rag_factory(self):
        """工厂函数测试."""
        from agentflow.knowledge.hooks import use_rag

        hook = use_rag(
            collection="kb",
            top_k=8,
            min_similarity=0.4,
            system_prompt="Custom prompt"
        )
        self.assertEqual(hook._collection, "kb")
        self.assertEqual(hook._top_k, 8)
        self.assertEqual(hook._system_prompt, "Custom prompt")


class TestVectorSearchResult(unittest.TestCase):
    """VectorSearchResult 的测试."""

    def test_creation(self):
        """创建测试."""
        from agentflow.knowledge.hooks import VectorSearchResult

        result = VectorSearchResult(
            id="doc-1",
            content="Test content",
            similarity=0.95,
            metadata={"source": "test.txt"},
        )
        self.assertEqual(result.id, "doc-1")
        self.assertEqual(result.content, "Test content")
        self.assertEqual(result.similarity, 0.95)
        self.assertEqual(result.metadata["source"], "test.txt")


if __name__ == "__main__":
    unittest.main()

