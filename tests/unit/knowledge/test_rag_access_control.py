"""RAGAccessControl のユニットテスト."""
from __future__ import annotations

from unittest.mock import AsyncMock

import pytest

from shared.rag.rag_access_control import RAGAccessControl
from shared.rag.scope_resolver import CollectionTarget, ScopeResolver


class TestRAGAccessControl:
    """RAGAccessControl のテスト."""

    @pytest.fixture
    def mock_resolver(self) -> AsyncMock:
        resolver = AsyncMock(spec=ScopeResolver)
        resolver.resolve_collections.return_value = [
            CollectionTarget(
                collection="faq__default__common",
                scope="common",
                backend_key="shared",
            ),
            CollectionTarget(
                collection="faq__default__manager",
                scope="manager",
                backend_key="shared",
            ),
        ]
        return resolver

    @pytest.fixture
    def rac(self, mock_resolver: AsyncMock) -> RAGAccessControl:
        return RAGAccessControl(scope_resolver=mock_resolver)

    @pytest.mark.asyncio
    async def test_get_search_targets(self, rac: RAGAccessControl, mock_resolver: AsyncMock) -> None:
        """role に応じた collection 一覧を返す."""
        targets = await rac.get_search_targets(
            role="manager", app_name="faq_system", tenant_id="default",
        )
        assert len(targets) == 2
        assert targets[0].collection == "faq__default__common"

    @pytest.mark.asyncio
    async def test_separate_by_backend(self, rac: RAGAccessControl) -> None:
        """shared と confidential を分離."""
        targets = [
            CollectionTarget(collection="a", scope="common", backend_key="shared"),
            CollectionTarget(collection="b", scope="conf", backend_key="confidential"),
        ]
        shared, confidential = rac.separate_by_backend(targets)
        assert len(shared) == 1
        assert len(confidential) == 1
        assert confidential[0].collection == "b"
