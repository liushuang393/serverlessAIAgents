"""MCP RAG アーキテクチャ テスト.

Agent → MCP → Pipeline → Backend の統一アーキテクチャをテスト。
各層の結合・動作を検証する。
"""

from __future__ import annotations

import tempfile
from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)
from apps.faq_system.backend.mcp.backends.file_system import FileSystemBackend
from apps.faq_system.backend.mcp.backends.vector_store import VectorStoreBackend
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline
from apps.faq_system.backend.mcp.server import FAQMCPServer
from apps.faq_system.backend.mcp.tools.file_search import FileSearchTool
from apps.faq_system.backend.mcp.tools.hybrid_search import HybridSearchTool
from apps.faq_system.backend.mcp.tools.knowledge_search import KnowledgeSearchTool
from kernel.protocols.mcp_tool import MCPToolRequest


# =============================================================================
# Backend 層テスト
# =============================================================================


class TestRetrievalQuery:
    """RetrievalQuery のテスト."""

    def test_default_values(self) -> None:
        """デフォルト値で生成可能."""
        q = RetrievalQuery(query="テスト")
        assert q.query == "テスト"
        assert q.top_k == 5
        assert q.filters == {}
        assert q.options == {}

    def test_custom_values(self) -> None:
        """カスタム値で生成可能."""
        q = RetrievalQuery(query="検索", top_k=10, filters={"category": "faq"})
        assert q.top_k == 10
        assert q.filters == {"category": "faq"}


class TestRetrievalResult:
    """RetrievalResult のテスト."""

    def test_empty_result(self) -> None:
        """空の結果."""
        r = RetrievalResult(query="テスト")
        assert not r.has_results
        assert r.best_score < 1e-9

    def test_with_documents(self) -> None:
        """ドキュメント付きの結果."""
        docs = [
            RetrievedDocument(doc_id="1", content="内容1", score=0.9),
            RetrievedDocument(doc_id="2", content="内容2", score=0.7),
        ]
        r = RetrievalResult(documents=docs, query="テスト", total_found=2)
        assert r.has_results
        assert abs(r.best_score - 0.9) < 1e-9


class TestFileSystemBackend:
    """FileSystemBackend のテスト."""

    @pytest.mark.asyncio
    async def test_search_directory(self) -> None:
        """ディレクトリ内ファイルを検索."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テストファイル作成
            test_file = Path(tmpdir) / "test.md"
            test_file.write_text("返品ポリシーについての情報です。\n返品は30日以内に行ってください。")

            backend = FileSystemBackend(search_dirs=[tmpdir])
            query = RetrievalQuery(query="返品ポリシー", top_k=5)
            result = await backend.retrieve(query)

            assert result.has_results
            assert result.documents[0].source.endswith("test.md")
            assert result.backend_type == BackendType.FILE_SYSTEM

    @pytest.mark.asyncio
    async def test_no_match(self) -> None:
        """マッチなしの場合."""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test.md"
            test_file.write_text("関係ない内容です。")

            backend = FileSystemBackend(search_dirs=[tmpdir])
            query = RetrievalQuery(query="完全にマッチしないクエリ", top_k=5)
            result = await backend.retrieve(query)

            # 部分マッチがない場合は空
            assert result.total_found == 0

    @pytest.mark.asyncio
    async def test_no_dirs(self) -> None:
        """ディレクトリ未指定の場合."""
        backend = FileSystemBackend(search_dirs=[])
        query = RetrievalQuery(query="テスト")
        result = await backend.retrieve(query)

        assert not result.has_results
        assert "error" in result.metadata


# =============================================================================
# Pipeline 層テスト
# =============================================================================


class TestRetrievalPipeline:
    """RetrievalPipeline のテスト."""

    @pytest.mark.asyncio
    async def test_single_backend(self) -> None:
        """単一バックエンドでの実行."""
        # モックバックエンド作成
        mock_backend = AsyncMock(spec=RetrievalBackend)
        mock_backend.name = "mock"
        mock_backend.retrieve.return_value = RetrievalResult(
            documents=[RetrievedDocument(doc_id="1", content="テスト", score=0.9)],
            query="テスト",
            total_found=1,
        )

        pipeline = RetrievalPipeline(name="test")
        pipeline.add_backend(mock_backend)
        result = await pipeline.execute(RetrievalQuery(query="テスト"))

        assert result.has_results
        assert len(result.documents) == 1
        mock_backend.retrieve.assert_called_once()

    @pytest.mark.asyncio
    async def test_multiple_backends_merge(self) -> None:
        """複数バックエンドの結果マージ."""
        backend1 = AsyncMock(spec=RetrievalBackend)
        backend1.name = "backend1"
        backend1.retrieve.return_value = RetrievalResult(
            documents=[RetrievedDocument(doc_id="1", content="結果1", score=0.9)],
            query="テスト",
            total_found=1,
        )

        backend2 = AsyncMock(spec=RetrievalBackend)
        backend2.name = "backend2"
        backend2.retrieve.return_value = RetrievalResult(
            documents=[RetrievedDocument(doc_id="2", content="結果2", score=0.8)],
            query="テスト",
            total_found=1,
        )

        pipeline = RetrievalPipeline(name="test")
        pipeline.add_backend(backend1, priority=0)
        pipeline.add_backend(backend2, priority=1)
        result = await pipeline.execute(RetrievalQuery(query="テスト"))

        assert len(result.documents) == 2
        # スコア降順
        assert result.documents[0].score >= result.documents[1].score


# =============================================================================
# MCP Server 層テスト
# =============================================================================


class TestFAQMCPServer:
    """FAQMCPServer のテスト."""

    def test_create_default(self) -> None:
        """デフォルト構成で生成可能."""
        server = FAQMCPServer.create_default(collection="test")
        tools = server.list_tools()

        tool_names = [t["name"] for t in tools]
        assert "knowledge_search" in tool_names
        assert "file_search" in tool_names
        assert "hybrid_search" in tool_names

    @pytest.mark.asyncio
    async def test_call_unknown_tool(self) -> None:
        """未知のツール呼び出し."""
        server = FAQMCPServer()
        result = await server.call_tool("unknown_tool", {"query": "テスト"})
        assert not result.success
        assert result.errors is not None
        assert len(result.errors) > 0

    @pytest.mark.asyncio
    async def test_call_file_search(self) -> None:
        """file_search ツール呼び出し（ディレクトリ未指定でエラー）."""
        server = FAQMCPServer()
        server.register_tool(FileSearchTool(search_dirs=[]))

        result = await server.call_tool("file_search", {"query": "テスト"})
        # ディレクトリ未指定でも成功（0件返却）
        assert result.success

    @pytest.mark.asyncio
    async def test_call_without_query(self) -> None:
        """query パラメータなしの場合のエラー."""
        server = FAQMCPServer()
        server.register_tool(FileSearchTool(search_dirs=[]))

        result = await server.call_tool("file_search", {})
        assert not result.success


class TestFileSearchTool:
    """FileSearchTool 単体テスト."""

    @pytest.mark.asyncio
    async def test_search_with_directory(self) -> None:
        """実ファイルでの検索."""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "policy.md"
            test_file.write_text("返品ポリシーについての情報です。")

            tool = FileSearchTool(search_dirs=[tmpdir])
            request = MCPToolRequest(
                tool="file_search",
                input={"query": "返品ポリシー", "top_k": 3},
            )
            response = await tool.handle_request(request)

            assert response.success
            assert response.output is not None
            assert response.output["total_found"] > 0

