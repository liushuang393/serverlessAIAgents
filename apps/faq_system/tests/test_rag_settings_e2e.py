"""FAQ System RAG 設定 API /api/rag/settings のユニットテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    import httpx


async def test_get_rag_settings_returns_current_config(
    client: httpx.AsyncClient,
) -> None:
    """GET /api/rag/settings が現在の設定を返すこと."""
    resp = await client.get("/api/rag/settings")
    assert resp.status_code == 200
    data = resp.json()
    assert "pattern" in data
    assert "chunk_strategy" in data
    assert "vector_provider" in data
    assert "enabled" in data


async def test_patch_rag_settings_with_pattern_autosets_params(
    client: httpx.AsyncClient,
) -> None:
    """PATCH /api/rag/settings でパターン選択時にパラメータが自動セットされること."""
    resp = await client.patch(
        "/api/rag/settings",
        json={"pattern": "faq_precision"},
    )
    assert resp.status_code in (200, 207)  # 207: platform 未起動時も正常
    data = resp.json()
    # faq_precision パターンの期待値
    assert data["chunk_strategy"] == "sentence"
    assert data["chunk_size"] == 500
    assert data["top_k"] == 8


async def test_patch_rag_settings_validates_unknown_pattern(
    client: httpx.AsyncClient,
) -> None:
    """不明なパターン名で 422 が返ること."""
    resp = await client.patch(
        "/api/rag/settings",
        json={"pattern": "unknown_pattern_xyz"},
    )
    assert resp.status_code == 422


async def test_patch_rag_settings_vector_db_setup_flag(
    client: httpx.AsyncClient,
) -> None:
    """auto_create_collection=True 時にコレクション作成を試みること（Qdrant 未起動でも 5xx にならない）."""
    resp = await client.patch(
        "/api/rag/settings",
        json={
            "enabled": True,
            "vector_provider": "qdrant",
            "auto_create_collection": True,
        },
    )
    # Qdrant 未起動の場合でも 500 にならず、警告付きで成功すること
    assert resp.status_code in (200, 207)
