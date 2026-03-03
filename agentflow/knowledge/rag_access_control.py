"""role/scope 制限付き RAG 検索の共通モジュール.

ScopeResolver で許可された collection のみ検索し、
結果を merge する共通エンジン。他 App からも再利用可能。
"""
from __future__ import annotations

import logging
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.knowledge.scope_resolver import CollectionTarget, ScopeResolver

logger = logging.getLogger(__name__)


class RAGAccessControl:
    """許可された collection のみ検索し、結果を merge する共通エンジン.

    Args:
        scope_resolver: ScopeResolver インスタンス
    """

    def __init__(self, scope_resolver: ScopeResolver) -> None:
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
        return await self._resolver.resolve_collections(
            role=role,
            app_name=app_name,
            tenant_id=tenant_id,
            token=token,
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
