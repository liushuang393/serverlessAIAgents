"""RAG コレクション・ドキュメント管理用 SQLAlchemy モデル.

各アプリの DB に追加されるテーブル定義。
コレクション単位のチャンキング・検索設定と、
ドキュメントのライフサイクル追跡を提供する。
"""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from sqlalchemy import DateTime, Float, Integer, String, Text, func
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


class Base(DeclarativeBase):
    """RAG 管理テーブル用ベースクラス.

    アプリ側の既存 Base と共存させるため、
    マイグレーション時は ``Base.metadata.create_all`` を呼ぶ。
    """


class DocumentStatus(str, Enum):
    """ドキュメント処理ステータス."""

    UPLOADED = "uploaded"
    CHUNKED = "chunked"
    INDEXED = "indexed"
    ERROR = "error"


class CollectionConfigModel(Base):
    """コレクション設定テーブル.

    コレクション単位で RAG パイプラインの設定を保持する。
    app_name + tenant_id でマルチテナント分離を実現。
    """

    __tablename__ = "rag_collection_configs"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)

    # --- 識別 ---
    collection_name: Mapped[str] = mapped_column(
        String(255), unique=True, nullable=False, index=True
    )
    app_name: Mapped[str] = mapped_column(String(128), nullable=False, index=True)
    tenant_id: Mapped[str | None] = mapped_column(String(128), nullable=True, index=True)

    # --- 表示 ---
    display_name: Mapped[str] = mapped_column(String(255), nullable=False, default="")
    description: Mapped[str] = mapped_column(Text, nullable=False, default="")

    # --- チャンキング ---
    chunk_strategy: Mapped[str] = mapped_column(String(64), nullable=False, default="recursive")
    chunk_size: Mapped[int] = mapped_column(Integer, nullable=False, default=1000)
    chunk_overlap: Mapped[int] = mapped_column(Integer, nullable=False, default=200)

    # --- エンベディング ---
    embedding_model: Mapped[str | None] = mapped_column(String(255), nullable=True)

    # --- 検索 ---
    retrieval_method: Mapped[str] = mapped_column(String(64), nullable=False, default="semantic")
    reranker: Mapped[str | None] = mapped_column(String(64), nullable=True)
    top_k: Mapped[int] = mapped_column(Integer, nullable=False, default=5)
    min_similarity: Mapped[float] = mapped_column(Float, nullable=False, default=0.3)

    # --- ベクトル DB ---
    vector_db_type: Mapped[str | None] = mapped_column(String(64), nullable=True)
    vector_db_url: Mapped[str | None] = mapped_column(String(512), nullable=True)

    # --- 統計 ---
    document_count: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    last_indexed_at: Mapped[datetime | None] = mapped_column(
        DateTime(timezone=True), nullable=True
    )

    # --- タイムスタンプ ---
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        server_default=func.now(),
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        server_default=func.now(),
        onupdate=func.now(),
    )

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "collection_name": self.collection_name,
            "app_name": self.app_name,
            "tenant_id": self.tenant_id,
            "display_name": self.display_name,
            "description": self.description,
            "chunk_strategy": self.chunk_strategy,
            "chunk_size": self.chunk_size,
            "chunk_overlap": self.chunk_overlap,
            "embedding_model": self.embedding_model,
            "retrieval_method": self.retrieval_method,
            "reranker": self.reranker,
            "top_k": self.top_k,
            "min_similarity": self.min_similarity,
            "vector_db_type": self.vector_db_type,
            "vector_db_url": self.vector_db_url,
            "document_count": self.document_count,
            "last_indexed_at": (
                self.last_indexed_at.isoformat() if self.last_indexed_at else None
            ),
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
        }


class DocumentRecordModel(Base):
    """ドキュメント記録テーブル.

    アップロードからインデックスまでのライフサイクルを追跡。
    content_hash による重複検出にも対応。
    """

    __tablename__ = "rag_document_records"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)

    # --- 識別 ---
    document_id: Mapped[str] = mapped_column(
        String(64), unique=True, nullable=False, index=True
    )
    collection_name: Mapped[str] = mapped_column(
        String(255), nullable=False, index=True
    )

    # --- ファイル情報 ---
    filename: Mapped[str] = mapped_column(String(512), nullable=False)
    file_type: Mapped[str] = mapped_column(String(64), nullable=False, default="text/plain")
    file_size: Mapped[int] = mapped_column(Integer, nullable=False, default=0)

    # --- 処理状態 ---
    status: Mapped[str] = mapped_column(
        String(32), nullable=False, default=DocumentStatus.UPLOADED.value
    )
    chunk_count: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    chunk_ids_json: Mapped[str] = mapped_column(Text, nullable=False, default="[]")
    metadata_json: Mapped[str] = mapped_column(Text, nullable=False, default="{}")

    # --- 重複検出 ---
    content_hash: Mapped[str] = mapped_column(String(64), nullable=False, index=True)

    # --- ユーザー ---
    uploaded_by: Mapped[str | None] = mapped_column(String(128), nullable=True)

    # --- タイムスタンプ ---
    uploaded_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        server_default=func.now(),
    )
    indexed_at: Mapped[datetime | None] = mapped_column(
        DateTime(timezone=True), nullable=True
    )

    # --- エラー ---
    error_message: Mapped[str | None] = mapped_column(Text, nullable=True)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "document_id": self.document_id,
            "collection_name": self.collection_name,
            "filename": self.filename,
            "file_type": self.file_type,
            "file_size": self.file_size,
            "status": self.status,
            "chunk_count": self.chunk_count,
            "content_hash": self.content_hash,
            "uploaded_by": self.uploaded_by,
            "uploaded_at": self.uploaded_at.isoformat() if self.uploaded_at else None,
            "indexed_at": self.indexed_at.isoformat() if self.indexed_at else None,
            "error_message": self.error_message,
        }
