"""MCP 拡張テスト - DatabaseBackend / ExternalAPI / Reranker / Dedup / ScoreNorm.

4つの拡張機能の単体テスト + 統合テスト。
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
from apps.faq_system.backend.mcp.backends.database import DatabaseBackend
from apps.faq_system.backend.mcp.backends.external_api import ExternalAPIBackend
from apps.faq_system.backend.mcp.middlewares.dedup import create_dedup_middleware
from apps.faq_system.backend.mcp.middlewares.reranker import create_reranker_middleware
from apps.faq_system.backend.mcp.middlewares.score_normalize import (
    create_score_normalize_middleware,
)
from apps.faq_system.backend.mcp.pipeline import RetrievalPipeline


# =============================================================================
# DatabaseBackend テスト
# =============================================================================


class TestDatabaseBackend:
    """DatabaseBackend (Text2SQL 連携) のテスト."""

    def test_init_default(self) -> None:
        """デフォルト値で生成可能."""
        backend = DatabaseBackend()
        assert backend.backend_type == BackendType.DATABASE
        assert backend.name == "database:text2sql"

    @pytest.mark.asyncio
    async def test_health_check_before_init(self) -> None:
        """初期化前のヘルスチェックは False."""
        backend = DatabaseBackend()
        assert not await backend.health_check()

    @pytest.mark.asyncio
    async def test_retrieve_with_mock_text2sql(self) -> None:
        """モック Text2SQLService で検索実行."""
        backend = DatabaseBackend(dialect="sqlite", schema={"users": ["id", "name"]})
        backend._started = True

        # モック Text2SQLService
        mock_service = AsyncMock()
        mock_result = MagicMock()
        mock_result.success = True
        mock_result.data = {
            "sql": "SELECT * FROM users LIMIT 3",
            "rows": [
                {"id": 1, "name": "田中"},
                {"id": 2, "name": "鈴木"},
            ],
            "columns": ["id", "name"],
            "row_count": 2,
            "answer": "ユーザーは2名います。",
        }
        mock_service.execute.return_value = mock_result
        backend._text2sql = mock_service

        query = RetrievalQuery(query="全ユーザーを表示", top_k=5)
        result = await backend.retrieve(query)

        assert result.has_results
        # 回答ドキュメント + 2行 = 3ドキュメント
        assert len(result.documents) == 3
        # 最初は回答ドキュメント
        assert result.documents[0].source == "text2sql:answer"
        assert "ユーザー" in result.documents[0].content
        # メタデータに SQL が含まれる
        assert "sql" in result.metadata

    @pytest.mark.asyncio
    async def test_retrieve_failure(self) -> None:
        """Text2SQL 失敗時のエラーハンドリング."""
        backend = DatabaseBackend()
        backend._started = True

        mock_service = AsyncMock()
        mock_result = MagicMock()
        mock_result.success = False
        mock_result.error_message = "テーブルが存在しません"
        mock_service.execute.return_value = mock_result
        backend._text2sql = mock_service

        query = RetrievalQuery(query="存在しないテーブル")
        result = await backend.retrieve(query)

        assert not result.has_results
        assert "error" in result.metadata


# =============================================================================
# ExternalAPIBackend テスト
# =============================================================================


class TestExternalAPIBackend:
    """ExternalAPIBackend のテスト."""

    def test_init(self) -> None:
        """初期化確認."""
        backend = ExternalAPIBackend(api_url="https://api.example.com/search")
        assert backend.backend_type == BackendType.EXTERNAL_API

    @pytest.mark.asyncio
    async def test_health_check(self) -> None:
        """API URL が設定されていればヘルスチェック OK."""
        backend = ExternalAPIBackend(api_url="https://api.example.com/search")
        assert await backend.health_check()

    @pytest.mark.asyncio
    async def test_health_check_no_url(self) -> None:
        """API URL 未設定の場合."""
        backend = ExternalAPIBackend(api_url="")
        assert not await backend.health_check()

    @pytest.mark.asyncio
    async def test_parse_response(self) -> None:
        """レスポンスパース."""
        backend = ExternalAPIBackend(api_url="https://api.example.com/search")
        data = {
            "results": [
                {"content": "結果1", "title": "タイトル1", "url": "https://example.com/1"},
                {"content": "結果2", "title": "タイトル2", "url": "https://example.com/2"},
            ]
        }
        docs = backend._parse_response(data, top_k=5)
        assert len(docs) == 2
        assert docs[0].content == "結果1"
        assert docs[1].source == "https://example.com/2"

    @pytest.mark.asyncio
    async def test_parse_list_response(self) -> None:
        """リスト形式のレスポンスパース."""
        backend = ExternalAPIBackend(api_url="https://api.example.com")
        data = [
            {"content": "結果A", "title": "タイトルA"},
            {"content": "結果B", "title": "タイトルB"},
        ]
        docs = backend._parse_response(data, top_k=1)
        assert len(docs) == 1
        assert docs[0].content == "結果A"


# =============================================================================
# リランカーミドルウェア テスト
# =============================================================================


class TestRerankerMiddleware:
    """リランカーミドルウェアのテスト."""

    @pytest.mark.asyncio
    async def test_bm25_reranker(self) -> None:
        """BM25 リランカーで結果をリランキング."""
        docs = [
            RetrievedDocument(doc_id="1", content="返品ポリシー 30日以内", score=0.5),
            RetrievedDocument(doc_id="2", content="配送について", score=0.8),
            RetrievedDocument(doc_id="3", content="返品 手続き 方法", score=0.3),
        ]
        result = RetrievalResult(
            documents=docs, query="返品ポリシー", total_found=3
        )

        middleware = create_reranker_middleware("bm25", top_k=2)
        reranked = await middleware(result)

        assert reranked.has_results
        assert len(reranked.documents) <= 2
        # リランカー情報がメタデータに含まれる
        assert reranked.metadata.get("reranker_type") == "bm25"

    @pytest.mark.asyncio
    async def test_empty_result(self) -> None:
        """空の結果はそのまま返す."""
        result = RetrievalResult(query="テスト")
        middleware = create_reranker_middleware("bm25")
        reranked = await middleware(result)
        assert not reranked.has_results


# =============================================================================
# 重複排除ミドルウェア テスト
# =============================================================================


class TestDedupMiddleware:
    """重複排除ミドルウェアのテスト."""

    @pytest.mark.asyncio
    async def test_exact_dedup(self) -> None:
        """完全一致の重複排除."""
        docs = [
            RetrievedDocument(doc_id="1", content="同じ内容", score=0.9),
            RetrievedDocument(doc_id="2", content="同じ内容", score=0.8),
            RetrievedDocument(doc_id="3", content="異なる内容", score=0.7),
        ]
        result = RetrievalResult(documents=docs, query="テスト", total_found=3)

        middleware = create_dedup_middleware(similarity_threshold=0.95)
        deduped = await middleware(result)

        assert len(deduped.documents) == 2
        assert deduped.metadata.get("dedup_removed") == 1

    @pytest.mark.asyncio
    async def test_no_duplicates(self) -> None:
        """重複がない場合はそのまま."""
        docs = [
            RetrievedDocument(doc_id="1", content="内容A", score=0.9),
            RetrievedDocument(doc_id="2", content="内容B", score=0.8),
        ]
        result = RetrievalResult(documents=docs, query="テスト", total_found=2)

        middleware = create_dedup_middleware()
        deduped = await middleware(result)

        assert len(deduped.documents) == 2


# =============================================================================
# スコア正規化ミドルウェア テスト
# =============================================================================


class TestScoreNormalizeMiddleware:
    """スコア正規化ミドルウェアのテスト."""

    @pytest.mark.asyncio
    async def test_minmax_normalize(self) -> None:
        """Min-Max 正規化."""
        docs = [
            RetrievedDocument(doc_id="1", content="A", score=10.0),
            RetrievedDocument(doc_id="2", content="B", score=5.0),
            RetrievedDocument(doc_id="3", content="C", score=0.0),
        ]
        result = RetrievalResult(documents=docs, query="テスト", total_found=3)

        middleware = create_score_normalize_middleware(method="minmax")
        normalized = await middleware(result)

        # 最大値は 1.0、最小値は 0.0 に正規化
        assert abs(normalized.documents[0].score - 1.0) < 1e-9
        assert abs(normalized.documents[2].score - 0.0) < 1e-9
        # 元スコアがメタデータに保存
        assert abs(normalized.documents[0].metadata.get("raw_score", 0.0) - 10.0) < 1e-9

    @pytest.mark.asyncio
    async def test_rank_normalize(self) -> None:
        """ランクベース正規化."""
        docs = [
            RetrievedDocument(doc_id="1", content="A", score=100.0),
            RetrievedDocument(doc_id="2", content="B", score=50.0),
        ]
        result = RetrievalResult(documents=docs, query="テスト", total_found=2)

        middleware = create_score_normalize_middleware(method="rank")
        normalized = await middleware(result)

        # 1位 = 1.0、2位 = 0.5
        assert abs(normalized.documents[0].score - 1.0) < 1e-9
        assert abs(normalized.documents[1].score - 0.5) < 1e-9


# =============================================================================
# 統合テスト: Pipeline + ミドルウェア
# =============================================================================


class TestPipelineWithMiddlewares:
    """Pipeline + ミドルウェアの統合テスト."""

    @pytest.mark.asyncio
    async def test_pipeline_with_reranker_and_dedup(self) -> None:
        """Pipeline + リランカー + 重複排除."""
        # モックバックエンド
        backend = AsyncMock(spec=RetrievalBackend)
        backend.name = "mock"
        backend.retrieve.return_value = RetrievalResult(
            documents=[
                RetrievedDocument(doc_id="1", content="返品 ポリシー 情報", score=0.5),
                RetrievedDocument(doc_id="2", content="返品 ポリシー 情報", score=0.4),  # 重複
                RetrievedDocument(doc_id="3", content="配送 方法", score=0.8),
            ],
            query="返品ポリシー",
            total_found=3,
        )

        pipeline = RetrievalPipeline(name="test_integrated")
        pipeline.add_backend(backend)
        pipeline.add_middleware(create_dedup_middleware(similarity_threshold=0.9))
        pipeline.add_middleware(create_reranker_middleware("bm25", top_k=5))
        pipeline.add_middleware(create_score_normalize_middleware(method="minmax"))

        result = await pipeline.execute(RetrievalQuery(query="返品ポリシー"))

        # 重複排除でドキュメント削減
        assert result.has_results
        # リランカーとスコア正規化が適用されている
        assert "reranker_type" in result.metadata
        assert "score_normalization" in result.metadata

