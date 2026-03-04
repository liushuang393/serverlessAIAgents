"""コレクションライフサイクル管理.

各アプリが自身の DB セッションで CollectionManager をインスタンス化し、
コレクション単位の RAG 設定・アクセス制御・RAGService 構築を行う。

使用例:
    >>> from agentflow.knowledge.collection_manager import CollectionManager
    >>>
    >>> mgr = CollectionManager(session_factory=async_session)
    >>> await mgr.create_collection(
    ...     collection_name="internal_kb",
    ...     app_name="faq_system",
    ...     chunk_strategy="semantic",
    ... )
    >>> config = await mgr.build_rag_config("internal_kb")
"""

from __future__ import annotations

import logging
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from sqlalchemy import select

from agentflow.knowledge.models import CollectionConfigModel
from agentflow.services.rag_service import ChunkStrategy as ServiceChunkStrategy
from agentflow.services.rag_service import RAGConfig, RerankerType


if TYPE_CHECKING:
    from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker

logger = logging.getLogger(__name__)

# --- auth_service 未接続時のフォールバック RBAC マッピング ---
_FALLBACK_ROLE_KB_MAP: dict[str, list[str]] = {
    "admin": ["internal", "external", "confidential"],
    "manager": ["internal", "external"],
    "employee": ["internal", "external"],
    "guest": ["external"],
}

# --- テナントプロビジョニング用デフォルトテンプレート ---
_DEFAULT_TENANT_TEMPLATE = ["internal", "external", "confidential"]


class CollectionManager:
    """フレームワーク級コレクションライフサイクル管理.

    各アプリが自身の DB セッションファクトリで初期化し、
    コレクション CRUD・統計・RAGService 構築・アクセス制御解決を行う。

    Args:
        session_factory: SQLAlchemy async セッションファクトリ
    """

    def __init__(self, session_factory: async_sessionmaker[AsyncSession]) -> None:
        self._session_factory = session_factory

    # ------------------------------------------------------------------
    # CRUD
    # ------------------------------------------------------------------

    async def create_collection(
        self,
        *,
        collection_name: str,
        app_name: str,
        tenant_id: str | None = None,
        display_name: str = "",
        description: str = "",
        chunk_strategy: str = "recursive",
        chunk_size: int = 1000,
        chunk_overlap: int = 200,
        embedding_model: str | None = None,
        retrieval_method: str = "semantic",
        reranker: str | None = None,
        top_k: int = 5,
        min_similarity: float = 0.3,
        vector_db_type: str | None = None,
        vector_db_url: str | None = None,
    ) -> CollectionConfigModel:
        """コレクションを作成.

        Args:
            collection_name: コレクション名（一意）
            app_name: アプリ名
            tenant_id: テナントID
            display_name: 表示名
            description: 説明
            chunk_strategy: チャンキング戦略
            chunk_size: チャンクサイズ
            chunk_overlap: チャンクオーバーラップ
            embedding_model: エンベディングモデル
            retrieval_method: 検索手法
            reranker: リランカー
            top_k: 上位K件
            min_similarity: 最小類似度
            vector_db_type: ベクトルDB種別
            vector_db_url: ベクトルDB URL

        Returns:
            作成された CollectionConfigModel

        Raises:
            ValueError: 同名コレクションが既に存在する場合
        """
        async with self._session_factory() as session:
            existing = await session.execute(
                select(CollectionConfigModel).where(
                    CollectionConfigModel.collection_name == collection_name
                )
            )
            if existing.scalar_one_or_none() is not None:
                msg = f"Collection '{collection_name}' already exists"
                raise ValueError(msg)

            model = CollectionConfigModel(
                collection_name=collection_name,
                app_name=app_name,
                tenant_id=tenant_id,
                display_name=display_name or collection_name,
                description=description,
                chunk_strategy=chunk_strategy,
                chunk_size=chunk_size,
                chunk_overlap=chunk_overlap,
                embedding_model=embedding_model,
                retrieval_method=retrieval_method,
                reranker=reranker,
                top_k=top_k,
                min_similarity=min_similarity,
                vector_db_type=vector_db_type,
                vector_db_url=vector_db_url,
            )
            session.add(model)
            await session.commit()
            await session.refresh(model)
            return model

    async def get_collection(self, collection_name: str) -> CollectionConfigModel | None:
        """コレクションを取得.

        Args:
            collection_name: コレクション名

        Returns:
            CollectionConfigModel または None
        """
        async with self._session_factory() as session:
            result = await session.execute(
                select(CollectionConfigModel).where(
                    CollectionConfigModel.collection_name == collection_name
                )
            )
            return result.scalar_one_or_none()

    async def list_collections(
        self,
        app_name: str | None = None,
        tenant_id: str | None = None,
    ) -> list[CollectionConfigModel]:
        """コレクション一覧を取得.

        Args:
            app_name: アプリ名でフィルタ（None で全件）
            tenant_id: テナントIDでフィルタ（None で全件）

        Returns:
            CollectionConfigModel のリスト
        """
        async with self._session_factory() as session:
            stmt = select(CollectionConfigModel)
            if app_name is not None:
                stmt = stmt.where(CollectionConfigModel.app_name == app_name)
            if tenant_id is not None:
                stmt = stmt.where(CollectionConfigModel.tenant_id == tenant_id)
            result = await session.execute(stmt)
            return list(result.scalars().all())

    async def update_collection(
        self,
        collection_name: str,
        updates: dict[str, Any],
    ) -> CollectionConfigModel:
        """コレクション設定を更新.

        Args:
            collection_name: コレクション名
            updates: 更新するフィールドと値の辞書

        Returns:
            更新された CollectionConfigModel

        Raises:
            ValueError: コレクションが存在しない場合
        """
        # 変更禁止フィールド
        _immutable = {"id", "collection_name", "created_at"}
        safe_updates = {k: v for k, v in updates.items() if k not in _immutable}

        async with self._session_factory() as session:
            result = await session.execute(
                select(CollectionConfigModel).where(
                    CollectionConfigModel.collection_name == collection_name
                )
            )
            model = result.scalar_one_or_none()
            if model is None:
                msg = f"Collection '{collection_name}' not found"
                raise ValueError(msg)

            for key, value in safe_updates.items():
                if hasattr(model, key):
                    setattr(model, key, value)

            model.updated_at = datetime.now(UTC)
            await session.commit()
            await session.refresh(model)
            return model

    async def delete_collection(self, collection_name: str) -> None:
        """コレクションを削除.

        Args:
            collection_name: コレクション名

        Raises:
            ValueError: コレクションが存在しない場合
        """
        async with self._session_factory() as session:
            result = await session.execute(
                select(CollectionConfigModel).where(
                    CollectionConfigModel.collection_name == collection_name
                )
            )
            model = result.scalar_one_or_none()
            if model is None:
                msg = f"Collection '{collection_name}' not found"
                raise ValueError(msg)

            await session.delete(model)
            await session.commit()

    # ------------------------------------------------------------------
    # 統計
    # ------------------------------------------------------------------

    async def get_collection_stats(self, collection_name: str) -> dict[str, Any]:
        """コレクションの統計情報を取得.

        Args:
            collection_name: コレクション名

        Returns:
            統計情報の辞書

        Raises:
            ValueError: コレクションが存在しない場合
        """
        model = await self.get_collection(collection_name)
        if model is None:
            msg = f"Collection '{collection_name}' not found"
            raise ValueError(msg)

        return {
            "collection_name": model.collection_name,
            "app_name": model.app_name,
            "document_count": model.document_count,
            "chunk_strategy": model.chunk_strategy,
            "retrieval_method": model.retrieval_method,
            "last_indexed_at": (
                model.last_indexed_at.isoformat() if model.last_indexed_at else None
            ),
        }

    # ------------------------------------------------------------------
    # RAGService 構築
    # ------------------------------------------------------------------

    async def build_rag_config(self, collection_name: str) -> RAGConfig:
        """コレクション設定から RAGConfig を構築.

        Args:
            collection_name: コレクション名

        Returns:
            RAGConfig インスタンス

        Raises:
            ValueError: コレクションが存在しない場合
        """
        model = await self.get_collection(collection_name)
        if model is None:
            msg = f"Collection '{collection_name}' not found"
            raise ValueError(msg)

        # チャンク戦略のマッピング（knowledge/chunking と services/rag_service の差異を吸収）
        strategy_map: dict[str, ServiceChunkStrategy] = {
            "fixed": ServiceChunkStrategy.RECURSIVE,
            "recursive": ServiceChunkStrategy.RECURSIVE,
            "semantic": ServiceChunkStrategy.SEMANTIC,
            "sentence": ServiceChunkStrategy.SENTENCE,
            "token": ServiceChunkStrategy.TOKEN,
            "markdown": ServiceChunkStrategy.MARKDOWN,
        }
        chunk_strategy = strategy_map.get(
            model.chunk_strategy, ServiceChunkStrategy.RECURSIVE
        )

        # リランカーのマッピング
        reranker_map: dict[str, RerankerType] = {
            "cohere": RerankerType.COHERE,
            "cross_encoder": RerankerType.CROSS_ENCODER,
            "bm25": RerankerType.BM25,
            "none": RerankerType.NONE,
        }
        reranker = reranker_map.get(model.reranker or "bm25", RerankerType.BM25)

        return RAGConfig(
            collection=model.collection_name,
            chunk_strategy=chunk_strategy,
            chunk_size=model.chunk_size,
            chunk_overlap=model.chunk_overlap,
            reranker=reranker,
            top_k=model.top_k,
            min_similarity=model.min_similarity,
        )

    # ------------------------------------------------------------------
    # アクセス制御
    # ------------------------------------------------------------------

    async def resolve_accessible_collections(
        self,
        role: str,
        app_name: str,
        tenant_id: str | None = None,
        token: str | None = None,
    ) -> list[CollectionConfigModel]:
        """ユーザーの role/tenant から検索可能なコレクション一覧を解決.

        auth_service が利用不可の場合はフォールバック RBAC マッピングを使用。

        Args:
            role: ユーザーのロール名
            app_name: アプリ名
            tenant_id: テナントID
            token: 認証トークン

        Returns:
            アクセス可能な CollectionConfigModel のリスト
        """
        all_collections = await self.list_collections(
            app_name=app_name, tenant_id=tenant_id
        )

        if not all_collections:
            return []

        # フォールバック: ロールに基づく KB タイプマッピング
        allowed_kb_types = _FALLBACK_ROLE_KB_MAP.get(role, ["external"])

        accessible: list[CollectionConfigModel] = []
        for col in all_collections:
            # コレクション名に KB タイプが含まれているか判定
            col_lower = col.collection_name.lower()
            for kb_type in allowed_kb_types:
                if kb_type in col_lower:
                    accessible.append(col)
                    break

        return accessible

    # ------------------------------------------------------------------
    # テナントプロビジョニング
    # ------------------------------------------------------------------

    async def provision_tenant(
        self,
        app_name: str,
        tenant_id: str,
        template: list[str] | None = None,
    ) -> list[CollectionConfigModel]:
        """テナント用コレクションを一括作成.

        Args:
            app_name: アプリ名
            tenant_id: テナントID
            template: KB タイプのリスト（デフォルト: internal, external, confidential）

        Returns:
            作成された CollectionConfigModel のリスト
        """
        kb_types = template or _DEFAULT_TENANT_TEMPLATE
        created: list[CollectionConfigModel] = []

        for kb_type in kb_types:
            col_name = f"{app_name}__{tenant_id}__{kb_type}"
            display = f"{tenant_id} - {kb_type}"
            model = await self.create_collection(
                collection_name=col_name,
                app_name=app_name,
                tenant_id=tenant_id,
                display_name=display,
                description=f"{app_name} テナント {tenant_id} の {kb_type} 知識ベース",
            )
            created.append(model)

        return created
