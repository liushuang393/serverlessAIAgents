"""Qdrant 接続失敗時のグレースフルデグラデーション テスト.

qdrant_client 未インストール / サーバー未起動の両ケースを対象とする。
"""
from __future__ import annotations

import pytest

from infrastructure.memory.vector_db.qdrant_db import QdrantDB


async def test_connect_or_warn_returns_false_when_client_not_installed() -> None:
    """qdrant_client 未インストール時に connect_or_warn() が False を返すこと."""
    db = QdrantDB(host="localhost", port=6333, collection_name="test")
    result = await db.connect_or_warn()
    # qdrant_client がない場合は False
    assert result is False
    assert db.is_available() is False


async def test_connect_or_warn_returns_false_when_server_unreachable() -> None:
    """Qdrant サーバー未起動時に connect_or_warn() が False を返すこと."""
    try:
        import qdrant_client  # noqa: F401
    except ImportError:
        pytest.skip("qdrant_client not installed; server-unreachable path untestable")

    db = QdrantDB(host="localhost", port=19999, collection_name="test")  # 存在しないポート
    result = await db.connect_or_warn()
    assert result is False
    assert db.is_available() is False


async def test_search_safe_returns_empty_when_not_connected() -> None:
    """未接続時の search_safe() は空リストを返すこと（例外を上げない）."""
    db = QdrantDB(host="localhost", port=6333, collection_name="test")
    # connect() を呼ばずに search_safe() を呼ぶ
    results = await db.search_safe(
        query_embedding=[0.1] * 384,
        limit=5,
    )
    assert results == []


async def test_is_available_false_before_connect() -> None:
    """connect() 前は is_available() が False であること."""
    db = QdrantDB(host="localhost", port=6333, collection_name="test")
    assert db.is_available() is False
