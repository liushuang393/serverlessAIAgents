"""Control-plane PlatformEngine の正規実装."""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from infrastructure.llm.providers import get_llm
from kernel.engines.base import EngineConfig
from kernel.engines.simple_engine import SimpleEngine
from control_plane.dashboards.runtime import TenantDashboard
from control_plane.publish.contracts import PublishResponse
from control_plane.publish.runtime import (
    ComponentEntry,
    ComponentLibrary,
    ComponentType,
    GalleryService,
    PublishOrchestrator,
    get_component_library,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from control_plane.schemas.gallery_schemas import (
        FeaturedResponse,
        GalleryItem,
        GallerySearchRequest,
        GallerySearchResponse,
    )

    from control_plane.publish.contracts import PublishEvent, PublishRequest


class PlatformEngine(SimpleEngine):
    """Platform エンジン.

    Gallery、公開、ダッシュボードを統合した Control-plane 実行窓口。
    """

    def __init__(
        self,
        llm_client: Any = None,
        component_library: ComponentLibrary | None = None,
    ) -> None:
        """初期化."""
        if llm_client is None:
            llm_client = get_llm()

        super().__init__(
            agent=None,
            config=EngineConfig(
                name="platform-engine",
                enable_events=True,
                enable_memory=True,
                timeout_seconds=900,
            ),
        )

        self._llm_client = llm_client
        self._library = component_library or get_component_library()
        self._gallery = GalleryService(component_library=self._library)
        self._publisher = PublishOrchestrator(component_library=self._library)
        self._dashboard = TenantDashboard(component_library=self._library)
        self._logger = logging.getLogger(__name__)

    async def search_gallery(self, query: str | GallerySearchRequest) -> GallerySearchResponse:
        """Gallery を検索する。"""
        return await self._gallery.search(query)

    async def get_featured(self) -> FeaturedResponse:
        """おすすめ一覧を返す。"""
        return await self._gallery.get_featured()

    async def get_gallery_item(self, item_id: str) -> GalleryItem | None:
        """単一 Gallery アイテムを返す。"""
        return await self._gallery.get_item(item_id)

    async def install_from_marketplace(self, item_id: str) -> ComponentEntry:
        """マーケットプレイスからコンポーネントを導入する。"""
        return await self._gallery.install_from_marketplace(item_id)

    def register_component(self, entry: ComponentEntry, *, overwrite: bool = False) -> None:
        """コンポーネントを登録する。"""
        self._library.register(entry, overwrite=overwrite)

    async def register_component_async(self, entry: ComponentEntry, *, overwrite: bool = False) -> None:
        """コンポーネントを非同期登録する。"""
        await self._library.register_async(entry, overwrite=overwrite)

    def get_component(self, component_id: str) -> ComponentEntry | None:
        """コンポーネントを取得する。"""
        return self._library.get_component(component_id)

    def search_components(
        self,
        query: str = "",
        *,
        types: list[ComponentType] | None = None,
        categories: list[str] | None = None,
        tags: list[str] | None = None,
        limit: int = 50,
        offset: int = 0,
    ) -> list[ComponentEntry]:
        """コンポーネントを検索する。"""
        return self._library.search(
            query=query,
            types=types,
            categories=categories,
            tags=tags,
            limit=limit,
            offset=offset,
        )

    def count_components(
        self,
        query: str = "",
        *,
        types: list[ComponentType] | None = None,
        categories: list[str] | None = None,
        tags: list[str] | None = None,
    ) -> int:
        """条件に合致するコンポーネント件数を返す。"""
        return self._library.count(query=query, types=types, categories=categories, tags=tags)

    async def publish(self, request: PublishRequest) -> AsyncIterator[PublishEvent]:
        """公開フローを実行する。"""
        async for event in self._publisher.publish(request):
            yield event

    async def publish_once(self, request: PublishRequest) -> PublishResponse:
        """公開フローを完了まで実行して最終結果を返す。"""
        last_response: PublishResponse | None = None
        async for event in self.publish(request):
            if getattr(event, "response", None) is not None:
                last_response = event.response

        if last_response is None:
            return PublishResponse(
                success=False,
                target=request.target,
                message="公開結果を取得できませんでした",
                deployed_at=datetime.now(UTC),
            )
        return last_response

    async def get_dashboard_summary(self, tenant_id: str) -> dict[str, Any]:
        """テナント別ダッシュボード要約を返す。"""
        return await self._dashboard.get_summary(tenant_id)
