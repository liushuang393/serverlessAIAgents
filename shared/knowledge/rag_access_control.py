"""role/scope 制限付き RAG 検索の共通モジュール.

ScopeResolver で許可された collection のみ検索し、
結果を merge する共通エンジン。他 App からも再利用可能。

CollectionManager と連携し、DB 管理されたコレクションに対する
ロールベースアクセス制御も提供する。
"""
from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from shared.knowledge.scope_resolver import FALLBACK_ROLE_KB_MAP


if TYPE_CHECKING:
    from shared.knowledge.collection_manager import CollectionManager
    from shared.knowledge.models import CollectionConfigModel
    from shared.knowledge.scope_resolver import CollectionTarget, ScopeResolver

logger = logging.getLogger(__name__)


class RAGAccessControl:
    """許可された collection のみ検索し、結果を merge する共通エンジン.

    Args:
        scope_resolver: ScopeResolver インスタンス
    """

    def __init__(self, scope_resolver: ScopeResolver | None = None) -> None:
        self._resolver = scope_resolver

    async def get_search_targets(
        self,
        role: str,
        app_name: str,
        tenant_id: str | None = None,
        token: str | None = None,
    ) -> list[CollectionTarget]:
        """ユーザーの role/tenant から検索対象 collection を取得.

        Args:
            role: ユーザーのロール名
            app_name: App 名
            tenant_id: テナントID
            token: 認証トークン

        Returns:
            検索対象 CollectionTarget のリスト
        """
        if self._resolver is None:
            return []
        return await self._resolver.resolve_collections(
            role=role,
            app_name=app_name,
            tenant_id=tenant_id,
            token=token,
        )

    @staticmethod
    def check_kb_type_access(role: str, kb_type: str) -> bool:
        """ロールが指定された KB タイプにアクセス可能か判定.

        Args:
            role: ユーザーのロール名
            kb_type: KB 種別 (internal / external / confidential)

        Returns:
            アクセス可能なら True
        """
        allowed = FALLBACK_ROLE_KB_MAP.get(role, ["external"])
        return kb_type in allowed

    @staticmethod
    async def filter_accessible_collections(
        collection_manager: CollectionManager,
        role: str,
        app_name: str,
        tenant_id: str | None = None,
    ) -> list[CollectionConfigModel]:
        """CollectionManager を使用してアクセス可能なコレクションをフィルタリング.

        Args:
            collection_manager: CollectionManager インスタンス
            role: ユーザーのロール名
            app_name: アプリ名
            tenant_id: テナントID

        Returns:
            アクセス可能な CollectionConfigModel のリスト
        """
        return await collection_manager.resolve_accessible_collections(
            role=role,
            app_name=app_name,
            tenant_id=tenant_id,
        )

    @staticmethod
    def separate_by_backend(
        targets: list[CollectionTarget],
    ) -> tuple[list[CollectionTarget], list[CollectionTarget]]:
        """shared と confidential を分離.

        Args:
            targets: CollectionTarget リスト

        Returns:
            (shared, confidential) のタプル
        """
        shared: list[CollectionTarget] = []
        confidential: list[CollectionTarget] = []
        for t in targets:
            if t.backend_key == "confidential":
                confidential.append(t)
            else:
                shared.append(t)
        return shared, confidential
