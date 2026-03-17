"""API データソースからのインジェスト テスト."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from shared.rag.api_ingestion import APIIngestor, APIIngestResult


async def test_ingest_json_api_returns_typed_result() -> None:
    """JSON API からのインジェストが型付き結果を返すこと."""
    mock_response = [
        {"id": 1, "question": "Q1", "answer": "A1"},
        {"id": 2, "question": "Q2", "answer": "A2"},
    ]

    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = mock_response
    mock_resp.headers = {"content-type": "application/json"}
    mock_resp.raise_for_status = MagicMock()

    with patch("httpx.AsyncClient.get", new_callable=AsyncMock, return_value=mock_resp):
        ingestor = APIIngestor(source_uri="https://api.example.com/faq")
        result = await ingestor.ingest(collection_name="test_col")

    assert isinstance(result, APIIngestResult)
    assert result.chunks_created == 2
    assert result.source_uri == "https://api.example.com/faq"
    assert result.content_type == "application/json"
    assert result.success is True


async def test_ingest_handles_dict_with_data_key() -> None:
    """{"data": [...]} 形式の JSON を処理できること."""
    mock_response = {"data": [{"text": "doc1"}, {"text": "doc2"}], "total": 2}

    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = mock_response
    mock_resp.headers = {"content-type": "application/json"}
    mock_resp.raise_for_status = MagicMock()

    with patch("httpx.AsyncClient.get", new_callable=AsyncMock, return_value=mock_resp):
        ingestor = APIIngestor(source_uri="https://api.example.com/docs")
        result = await ingestor.ingest(collection_name="test_col")

    assert result.chunks_created == 2
    assert result.success is True


async def test_ingest_returns_error_result_on_failure() -> None:
    """API 接続失敗時にエラー結果を返すこと（例外を上げない）."""
    with patch("httpx.AsyncClient.get", side_effect=Exception("Connection refused")):
        ingestor = APIIngestor(source_uri="https://unreachable.example.com")
        result = await ingestor.ingest(collection_name="test_col")

    assert result.success is False
    assert "Connection refused" in result.error_message
    assert result.chunks_created == 0
