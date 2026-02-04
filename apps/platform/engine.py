# -*- coding: utf-8 -*-
"""PlatformEngine - Platform アプリケーションエンジン.

Gallery、一键发布、多租户Dashboard を統合したエンジン。

使用例:
    >>> engine = PlatformEngine()
    >>> # Gallery検索
    >>> results = await engine.search_gallery("PDF")
    >>> # コンポーネント登録
    >>> await engine.register_component(entry)
    >>> # 一键发布
    >>> async for event in engine.publish(request):
    ...     print(event)
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator
from typing import Any

from agentflow.engines import SimpleEngine, EngineConfig
from agentflow.providers import get_llm
from apps.platform.services.component_library import (
    ComponentLibrary,
    ComponentEntry,
    ComponentType,
    get_component_library,
)
from apps.platform.services.gallery_service import GalleryService
from apps.platform.services.publish_orchestrator import PublishOrchestrator
from apps.platform.services.tenant_dashboard import TenantDashboard
from apps.platform.schemas.gallery_schemas import (
    GallerySearchRequest,
    GallerySearchResponse,
    GalleryItem,
    FeaturedResponse,
)
from apps.platform.schemas.publish_schemas import (
    PublishRequest,
    PublishEvent,
    PublishResponse,
)


class PlatformEngine(SimpleEngine):
    """Platform エンジン.

    AgentFlow プラットフォームの統合エンジン。
    Gallery、発布、ダッシュボード機能を提供。
    """

    def __init__(
        self,
        llm_client: Any = None,
        component_library: ComponentLibrary | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（省略時は自動取得）
            component_library: コンポーネントライブラリ
        """
        # LLM自動取得（省略時）
        if llm_client is None:
            llm_client = get_llm()

        # SimpleEngine 初期化
        super().__init__(
            agent=None,  # Platform は単一 Agent を持たない
            config=EngineConfig(
                name="platform-engine",
                enable_events=True,
                enable_memory=True,
                timeout_seconds=600,
            ),
        )

        self._llm_client = llm_client
        self._library = component_library or get_component_library()
        self._gallery = GalleryService(component_library=self._library)
        self._publisher = PublishOrchestrator(component_library=self._library)
        self._dashboard = TenantDashboard(component_library=self._library)
        self._logger = logging.getLogger(__name__)

    # =========================================================================
    # Gallery API
    # =========================================================================

    async def search_gallery(
        self,
        query: str | GallerySearchRequest,
    ) -> GallerySearchResponse:
        """Gallery検索.

        Args:
            query: 検索クエリまたはリクエスト

        Returns:
            検索結果
        """
        return await self._gallery.search(query)

    async def get_featured(self) -> FeaturedResponse:
        """推荐リストを取得.

        Returns:
            推荐レスポンス
        """
        return await self._gallery.get_featured()

    async def get_gallery_item(self, item_id: str) -> GalleryItem | None:
        """Galleryアイテムを取得.

        Args:
            item_id: アイテムID

        Returns:
            Galleryアイテム
        """
        return await self._gallery.get_item(item_id)

    async def install_from_marketplace(self, item_id: str) -> ComponentEntry:
        """マーケットプレイスからインストール.

        Args:
            item_id: アイテムID

        Returns:
            インストールされたコンポーネント
        """
        return await self._gallery.install_from_marketplace(item_id)

    # =========================================================================
    # Component Library API
    # =========================================================================

    def register_component(self, entry: ComponentEntry, *, overwrite: bool = False) -> None:
        """コンポーネントを登録.

        Args:
            entry: コンポーネントエントリ
            overwrite: 上書き許可
        """
        self._library.register(entry, overwrite=overwrite)

    async def register_component_async(
        self, entry: ComponentEntry, *, overwrite: bool = False
    ) -> None:
        """コンポーネントを非同期登録.

        Args:
            entry: コンポーネントエントリ
            overwrite: 上書き許可
        """
        await self._library.register_async(entry, overwrite=overwrite)

    def get_component(self, component_id: str) -> ComponentEntry | None:
        """コンポーネントを取得.

        Args:
            component_id: コンポーネントID

        Returns:
            コンポーネントエントリ
        """
        return self._library.get_component(component_id)

    def search_components(
        self,
        query: str = "",
        *,
        types: list[ComponentType] | None = None,
        categories: list[str] | None = None,
        tags: list[str] | None = None,
        limit: int = 50,
    ) -> list[ComponentEntry]:
        """コンポーネントを検索.

        Args:
            query: 検索クエリ
            types: タイプフィルター
            categories: カテゴリフィルター
            tags: タグフィルター
            limit: 最大取得数

        Returns:
            コンポーネントリスト
        """
        return self._library.search(
            query=query,
            types=types,
            categories=categories,
            tags=tags,
            limit=limit,
        )

    def list_components_by_type(self, component_type: ComponentType) -> list[ComponentEntry]:
        """タイプ別にコンポーネントを取得.

        Args:
            component_type: コンポーネントタイプ

        Returns:
            コンポーネントリスト
        """
        return self._library.list_by_type(component_type)

    # =========================================================================
    # Publish API
    # =========================================================================

    async def publish(self, request: PublishRequest) -> AsyncIterator[PublishEvent]:
        """一键发布を実行.

        Args:
            request: 発布リクエスト

        Yields:
            発布イベント
        """
        async for event in self._publisher.publish(request):
            yield event

    async def publish_sync(self, request: PublishRequest) -> PublishResponse:
        """一键发布を同期実行.

        Args:
            request: 発布リクエスト

        Returns:
            発布レスポンス
        """
        response: PublishResponse | None = None
        async for event in self._publisher.publish(request):
            # 最終イベントのステータスを使用
            response = self._publisher.get_publish_status(event.publish_id)

        return response or PublishResponse(
            publish_id="",
            status=PublishResponse.model_fields["status"].default,
            target=request.target,
            started_at=None,
        )

    def get_publish_status(self, publish_id: str) -> PublishResponse | None:
        """発布ステータスを取得.

        Args:
            publish_id: 発布ID

        Returns:
            発布レスポンス
        """
        return self._publisher.get_publish_status(publish_id)

    async def cancel_publish(self, publish_id: str) -> bool:
        """発布をキャンセル.

        Args:
            publish_id: 発布ID

        Returns:
            キャンセル成功の場合 True
        """
        return await self._publisher.cancel_publish(publish_id)

    # =========================================================================
    # Dashboard API
    # =========================================================================

    async def get_dashboard_summary(self, tenant_id: str) -> dict[str, Any]:
        """ダッシュボードサマリーを取得.

        Args:
            tenant_id: テナントID

        Returns:
            サマリー情報
        """
        return await self._dashboard.get_dashboard_summary(tenant_id)

    async def get_tenant_stats(self, tenant_id: str) -> dict[str, Any]:
        """テナント統計を取得.

        Args:
            tenant_id: テナントID

        Returns:
            統計情報
        """
        stats = await self._dashboard.get_stats(tenant_id)
        return stats.to_dict()

    async def get_top_components(
        self,
        tenant_id: str,
        limit: int = 10,
    ) -> list[dict[str, Any]]:
        """人気コンポーネントを取得.

        Args:
            tenant_id: テナントID
            limit: 取得数

        Returns:
            人気コンポーネントリスト
        """
        components = await self._dashboard.get_top_components(tenant_id, limit)
        return [
            {
                "id": c.component_id,
                "name": c.name,
                "type": c.type.value,
                "usage_count": c.usage_count,
            }
            for c in components
        ]

    # =========================================================================
    # SimpleEngine オーバーライド
    # =========================================================================

    async def run(self, input_data: dict[str, Any]) -> Any:
        """Platform エンジンを実行.

        Args:
            input_data: 入力データ

        Returns:
            実行結果
        """
        action = input_data.get("action", "search")

        if action == "search":
            query = input_data.get("query", "")
            return await self.search_gallery(query)

        elif action == "publish":
            request = PublishRequest(**input_data.get("request", {}))
            return await self.publish_sync(request)

        elif action == "dashboard":
            tenant_id = input_data.get("tenant_id", "default")
            return await self.get_dashboard_summary(tenant_id)

        else:
            return {"error": f"Unknown action: {action}"}


__all__ = ["PlatformEngine"]
