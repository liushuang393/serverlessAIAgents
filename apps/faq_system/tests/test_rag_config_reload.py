"""RAG 設定変更後のキャッシュ無効化テスト。"""

from __future__ import annotations

import pytest
from unittest.mock import AsyncMock, MagicMock, patch

from apps.faq_system.routers.collections import update_collection, UpdateCollectionRequest


class TestRAGConfigReload:
    """コレクション設定変更時に RAGService が再構築されることを検証。"""

    @pytest.mark.asyncio
    async def test_update_collection_invalidates_cache(self) -> None:
        """PATCH /api/collections/{name} 後に service cache が無効化される。"""
        fake_model = MagicMock()
        fake_model.to_dict.return_value = {"collection_name": "test_col"}

        fake_mgr = MagicMock()
        fake_mgr.update_collection = AsyncMock(return_value=fake_model)

        request = UpdateCollectionRequest(top_k=10)

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=fake_mgr,
            ),
            patch(
                "apps.faq_system.routers.collections.invalidate_service_cache",
            ) as mock_invalidate,
        ):
            result = await update_collection(
                name="test_col",
                request=request,
                _user=MagicMock(),
            )

        # invalidate_service_cache が正しいプレフィックスで呼ばれたことを検証
        mock_invalidate.assert_called_once_with("rag:test_col", "faq_agent")

        # update_collection の戻り値が正しいことを検証
        assert result == {"collection": {"collection_name": "test_col"}}

    @pytest.mark.asyncio
    async def test_update_collection_no_invalidate_on_failure(self) -> None:
        """update_collection が ValueError を投げた場合、キャッシュ無効化されない。"""
        fake_mgr = MagicMock()
        fake_mgr.update_collection = AsyncMock(side_effect=ValueError("Not found"))

        request = UpdateCollectionRequest(top_k=10)

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=fake_mgr,
            ),
            patch(
                "apps.faq_system.routers.collections.invalidate_service_cache",
            ) as mock_invalidate,
        ):
            from fastapi import HTTPException

            with pytest.raises(HTTPException) as exc_info:
                await update_collection(
                    name="missing_col",
                    request=request,
                    _user=MagicMock(),
                )

            assert exc_info.value.status_code == 404

        # 失敗時はキャッシュ無効化が呼ばれないことを検証
        mock_invalidate.assert_not_called()

    @pytest.mark.asyncio
    async def test_update_collection_no_invalidate_on_empty_update(self) -> None:
        """更新フィールドが空の場合、400 エラーでキャッシュ無効化されない。"""
        request = UpdateCollectionRequest()  # 全フィールド None

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=MagicMock(),
            ),
            patch(
                "apps.faq_system.routers.collections.invalidate_service_cache",
            ) as mock_invalidate,
        ):
            from fastapi import HTTPException

            with pytest.raises(HTTPException) as exc_info:
                await update_collection(
                    name="test_col",
                    request=request,
                    _user=MagicMock(),
                )

            assert exc_info.value.status_code == 400

        mock_invalidate.assert_not_called()
