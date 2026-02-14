# -*- coding: utf-8 -*-
"""RAG 管理 API 用スキーマ."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field


class RAGDataSourceInput(BaseModel):
    """RAG データソース入力."""

    type: str = Field(..., min_length=1, description="データソース種別")
    uri: str = Field(..., min_length=1, description="接続先 URI / パス")
    label: str = Field(default="", description="表示ラベル")
    enabled: bool = Field(default=True, description="有効化")
    schedule: str | None = Field(default=None, description="更新スケジュール")
    options: dict[str, Any] = Field(default_factory=dict, description="追加設定")


class RAGConfigPatchRequest(BaseModel):
    """App 単位の RAG 設定更新リクエスト."""

    enabled: bool | None = Field(default=None, description="RAG 有効化")
    pattern: str | None = Field(default=None, description="適用パターン")
    vector_provider: str | None = Field(default=None, description="VectorDB プロバイダ")
    vector_url: str | None = Field(default=None, description="VectorDB URL")
    vector_collection: str | None = Field(default=None, description="VectorDB コレクション")
    embedding_model: str | None = Field(default=None, description="埋め込みモデル")
    chunk_strategy: str | None = Field(default=None, description="チャンク分割方式")
    chunk_size: int | None = Field(default=None, ge=100, le=8000, description="チャンクサイズ")
    chunk_overlap: int | None = Field(default=None, ge=0, le=2000, description="チャンク重複サイズ")
    retrieval_method: str | None = Field(default=None, description="検索方式")
    reranker: str | None = Field(default=None, description="リランカー")
    top_k: int | None = Field(default=None, ge=1, le=100, description="Top K")
    score_threshold: float | None = Field(default=None, ge=0.0, le=1.0, description="スコア閾値")
    indexing_schedule: str | None = Field(default=None, description="インデックス更新スケジュール")
    data_sources: list[RAGDataSourceInput] | None = Field(
        default=None,
        description="データソース一覧",
    )
