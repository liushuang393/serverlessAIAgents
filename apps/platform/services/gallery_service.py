"""GalleryService - Gallery検索・発見サービス.

ローカルコンポーネントとマーケットプレイスの統一検索を提供。

使用例:
    >>> service = GalleryService()
    >>> # 検索
    >>> results = await service.search("PDF processor")
    >>> # 推荐リスト
    >>> featured = await service.get_featured()
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime

from apps.platform.schemas.gallery_schemas import (
    FeaturedItem,
    FeaturedResponse,
    GalleryItem,
    GalleryItemType,
    GallerySearchRequest,
    GallerySearchResponse,
)
from apps.platform.services.component_library import (
    ComponentEntry,
    ComponentLibrary,
    ComponentType,
    get_component_library,
)

from agentflow.marketplace.client import MarketplaceAgent, MarketplaceClient


class GalleryService:
    """Galleryサービス.

    ローカルコンポーネントライブラリとマーケットプレイスの統一検索を提供。
    """

    def __init__(
        self,
        component_library: ComponentLibrary | None = None,
        marketplace_client: MarketplaceClient | None = None,
    ) -> None:
        """初期化.

        Args:
            component_library: コンポーネントライブラリ
            marketplace_client: マーケットプレイスクライアント
        """
        self._library = component_library or get_component_library()
        self._marketplace = marketplace_client or MarketplaceClient()
        self._logger = logging.getLogger(__name__)

    async def search(
        self,
        request: GallerySearchRequest | str,
    ) -> GallerySearchResponse:
        """Gallery検索.

        Args:
            request: 検索リクエストまたはクエリ文字列

        Returns:
            検索結果
        """
        # 文字列の場合はリクエストに変換
        if isinstance(request, str):
            request = GallerySearchRequest(query=request)

        results: list[GalleryItem] = []

        # ローカル検索
        if request.filter.include_local:
            local_results = await self._search_local(request)
            results.extend(local_results)

        # マーケットプレイス検索
        if request.filter.include_marketplace:
            marketplace_results = await self._search_marketplace(request)
            results.extend(marketplace_results)

        # 重複除去（IDベース）
        seen_ids: set[str] = set()
        unique_results: list[GalleryItem] = []
        for item in results:
            if item.id not in seen_ids:
                seen_ids.add(item.id)
                unique_results.append(item)

        # ソート
        unique_results = self._sort_results(unique_results, request.sort_by, request.sort_order)

        # ページネーション
        total = len(unique_results)
        paginated = unique_results[request.offset : request.offset + request.limit]

        return GallerySearchResponse(
            items=paginated,
            total=total,
            limit=request.limit,
            offset=request.offset,
            query=request.query,
            has_more=(request.offset + len(paginated)) < total,
        )

    async def _search_local(self, request: GallerySearchRequest) -> list[GalleryItem]:
        """ローカルコンポーネント検索.

        Args:
            request: 検索リクエスト

        Returns:
            Galleryアイテムリスト
        """
        # タイプフィルターを変換
        types = None
        if request.filter.types:
            types = [
                self._gallery_to_component_type(t)
                for t in request.filter.types
                if self._gallery_to_component_type(t) is not None
            ]

        # ローカル検索
        entries = self._library.search(
            query=request.query,
            types=types,
            categories=request.filter.categories,
            tags=request.filter.tags,
            limit=request.limit * 2,  # マージ用に多めに取得
        )

        # GalleryItem に変換
        return [self._entry_to_gallery_item(entry) for entry in entries]

    async def _search_marketplace(self, request: GallerySearchRequest) -> list[GalleryItem]:
        """マーケットプレイス検索.

        Args:
            request: 検索リクエスト

        Returns:
            Galleryアイテムリスト
        """
        try:
            # マーケットプレイス検索
            agents = self._marketplace.search(
                query=request.query,
                category=request.filter.categories[0] if request.filter.categories else None,
                protocols=request.filter.protocols,
                limit=request.limit,
            )

            # GalleryItem に変換
            return [self._marketplace_to_gallery_item(agent) for agent in agents]

        except Exception as e:
            self._logger.warning(f"Marketplace search failed: {e}")
            return []

    def _entry_to_gallery_item(self, entry: ComponentEntry) -> GalleryItem:
        """ComponentEntry を GalleryItem に変換.

        Args:
            entry: コンポーネントエントリ

        Returns:
            Galleryアイテム
        """
        return GalleryItem(
            id=entry.id,
            name=entry.name,
            type=self._component_to_gallery_type(entry.type),
            version=entry.version,
            description=entry.description,
            author=entry.author,
            category=entry.category,
            tags=entry.tags,
            protocols=entry.protocols,
            icon=self._get_type_icon(entry.type),
            rating=entry.metadata.get("rating", 0.0),
            downloads=entry.usage_count,
            verified=entry.metadata.get("verified", False),
            source="local",
            created_at=entry.created_at,
            updated_at=entry.updated_at,
            metadata=entry.metadata,
        )

    def _marketplace_to_gallery_item(self, agent: MarketplaceAgent) -> GalleryItem:
        """MarketplaceAgent を GalleryItem に変換.

        Args:
            agent: マーケットプレイスエージェント

        Returns:
            Galleryアイテム
        """
        return GalleryItem(
            id=f"marketplace:{agent.id}",
            name=agent.name,
            type=GalleryItemType.AGENT,
            version=agent.version,
            description=agent.description,
            author=agent.author,
            category=agent.category,
            protocols=agent.protocols,
            icon="📦",
            source="marketplace",
        )

    def _gallery_to_component_type(self, gallery_type: GalleryItemType) -> ComponentType | None:
        """GalleryItemType を ComponentType に変換.

        Args:
            gallery_type: Galleryアイテムタイプ

        Returns:
            コンポーネントタイプ（変換不可の場合 None）
        """
        mapping = {
            GalleryItemType.AGENT: ComponentType.AGENT,
            GalleryItemType.FLOW: ComponentType.FLOW,
            GalleryItemType.TOOL: ComponentType.TOOL,
            GalleryItemType.SKILL: ComponentType.SKILL,
            GalleryItemType.ENGINE: ComponentType.ENGINE,
            GalleryItemType.TEMPLATE: ComponentType.TEMPLATE,
        }
        return mapping.get(gallery_type)

    def _component_to_gallery_type(self, component_type: ComponentType) -> GalleryItemType:
        """ComponentType を GalleryItemType に変換.

        Args:
            component_type: コンポーネントタイプ

        Returns:
            Galleryアイテムタイプ
        """
        mapping = {
            ComponentType.AGENT: GalleryItemType.AGENT,
            ComponentType.FLOW: GalleryItemType.FLOW,
            ComponentType.TOOL: GalleryItemType.TOOL,
            ComponentType.SKILL: GalleryItemType.SKILL,
            ComponentType.ENGINE: GalleryItemType.ENGINE,
            ComponentType.TEMPLATE: GalleryItemType.TEMPLATE,
        }
        return mapping.get(component_type, GalleryItemType.AGENT)

    def _get_type_icon(self, component_type: ComponentType) -> str:
        """コンポーネントタイプのアイコンを取得.

        Args:
            component_type: コンポーネントタイプ

        Returns:
            アイコン文字列
        """
        icons = {
            ComponentType.AGENT: "🤖",
            ComponentType.FLOW: "🔄",
            ComponentType.TOOL: "🔧",
            ComponentType.SKILL: "⚡",
            ComponentType.ENGINE: "⚙️",
            ComponentType.TEMPLATE: "📋",
        }
        return icons.get(component_type, "📦")

    def _sort_results(
        self,
        results: list[GalleryItem],
        sort_by: str,
        sort_order: str,
    ) -> list[GalleryItem]:
        """結果をソート.

        Args:
            results: Galleryアイテムリスト
            sort_by: ソート項目
            sort_order: ソート順序

        Returns:
            ソートされたリスト
        """
        reverse = sort_order.lower() == "desc"

        if sort_by == "relevance":
            # relevance はデフォルト順序を維持
            return results
        if sort_by == "name":
            return sorted(results, key=lambda x: x.name.lower(), reverse=reverse)
        if sort_by == "downloads":
            return sorted(results, key=lambda x: x.downloads, reverse=reverse)
        if sort_by == "rating":
            return sorted(results, key=lambda x: x.rating, reverse=reverse)
        if sort_by == "updated":
            return sorted(
                results,
                key=lambda x: x.updated_at or datetime.min.replace(tzinfo=UTC),
                reverse=reverse,
            )
        return results

    async def get_featured(self) -> FeaturedResponse:
        """推荐リストを取得.

        Returns:
            推荐レスポンス
        """
        # 人気コンポーネントを取得
        popular = self._library.search(limit=10)

        featured_items = [
            FeaturedItem(
                item=self._entry_to_gallery_item(entry),
                featured_reason="Popular in your organization",
                priority=i,
            )
            for i, entry in enumerate(popular)
        ]

        # カテゴリ一覧
        categories = list({entry.category for entry in self._library.list_all().values()})

        # 人気タグ
        all_tags: list[str] = []
        for entry in self._library.list_all().values():
            all_tags.extend(entry.tags)
        tag_counts: dict[str, int] = {}
        for tag in all_tags:
            tag_counts[tag] = tag_counts.get(tag, 0) + 1
        popular_tags = sorted(tag_counts.keys(), key=lambda t: tag_counts[t], reverse=True)[:10]

        return FeaturedResponse(
            featured=featured_items,
            categories=sorted(categories),
            popular_tags=popular_tags,
        )

    async def get_item(self, item_id: str) -> GalleryItem | None:
        """アイテム詳細を取得.

        Args:
            item_id: アイテムID

        Returns:
            Galleryアイテム（存在しない場合 None）
        """
        # マーケットプレイスアイテムの場合
        if item_id.startswith("marketplace:"):
            actual_id = item_id.replace("marketplace:", "")
            agents = self._marketplace.search(query=actual_id, limit=1)
            if agents:
                return self._marketplace_to_gallery_item(agents[0])
            return None

        # ローカルアイテム
        entry = self._library.get_component(item_id)
        if entry:
            return self._entry_to_gallery_item(entry)

        return None

    async def install_from_marketplace(self, item_id: str) -> ComponentEntry:
        """マーケットプレイスからインストール.

        Args:
            item_id: アイテムID

        Returns:
            インストールされたコンポーネントエントリ

        Raises:
            ValueError: インストール失敗
        """
        # マーケットプレイスIDを抽出
        actual_id = item_id.replace("marketplace:", "") if item_id.startswith("marketplace:") else item_id

        # インストール
        install_path = self._marketplace.install(actual_id)

        # コンポーネントライブラリに登録
        agents = self._marketplace.search(query=actual_id, limit=1)
        if not agents:
            msg = f"Agent not found: {actual_id}"
            raise ValueError(msg)

        agent = agents[0]
        entry = ComponentEntry(
            id=agent.id,
            name=agent.name,
            type=ComponentType.AGENT,
            version=agent.version,
            description=agent.description,
            author=agent.author,
            category=agent.category,
            protocols=agent.protocols,
            source_path=str(install_path),
            metadata={"source": "marketplace"},
        )

        self._library.register(entry)
        return entry


__all__ = ["GalleryService"]
