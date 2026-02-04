# -*- coding: utf-8 -*-
"""Publish Schemas - 発布API のリクエスト/レスポンスモデル."""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class PublishTarget(str, Enum):
    """発布ターゲット."""

    DOCKER = "docker"
    VERCEL = "vercel"
    AWS_LAMBDA = "aws_lambda"
    GITHUB_ACTIONS = "github_actions"
    LOCAL = "local"
    GALLERY = "gallery"


class PublishStatus(str, Enum):
    """発布ステータス."""

    PENDING = "pending"
    VALIDATING = "validating"
    GENERATING = "generating"
    DEPLOYING = "deploying"
    REGISTERING = "registering"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class PublishMode(str, Enum):
    """発布モード."""

    STUDIO = "studio"  # Studio UI から
    CLI = "cli"  # CLI から
    API = "api"  # API から
    AI_ASSISTANT = "ai_assistant"  # AI助手から自動発布


class PublishRequest(BaseModel):
    """発布リクエスト."""

    # ソース指定（いずれか必須）
    component_id: str | None = Field(default=None, description="コンポーネントID")
    source_path: str | None = Field(default=None, description="ソースパス")
    source_code: str | None = Field(default=None, description="ソースコード")

    # ターゲット
    target: PublishTarget = Field(..., description="発布ターゲット")

    # オプション
    version: str | None = Field(default=None, description="バージョン (自動インクリメント可)")
    name: str | None = Field(default=None, description="発布名")
    description: str | None = Field(default=None, description="説明")
    mode: PublishMode = Field(default=PublishMode.API, description="発布モード")

    # ターゲット固有設定
    config: dict[str, Any] = Field(default_factory=dict, description="ターゲット固有設定")

    # Gallery 発布オプション
    publish_to_gallery: bool = Field(default=False, description="Galleryにも発布")
    gallery_visibility: str = Field(default="private", description="Gallery可視性")

    # 環境変数
    env_vars: dict[str, str] = Field(default_factory=dict, description="環境変数")


class PublishPhase(BaseModel):
    """発布フェーズ."""

    name: str = Field(..., description="フェーズ名")
    status: PublishStatus = Field(..., description="ステータス")
    message: str = Field(default="", description="メッセージ")
    progress: float = Field(default=0.0, ge=0.0, le=100.0, description="進捗率")
    started_at: datetime | None = Field(default=None, description="開始日時")
    completed_at: datetime | None = Field(default=None, description="完了日時")
    details: dict[str, Any] = Field(default_factory=dict, description="詳細情報")


class PublishResponse(BaseModel):
    """発布レスポンス."""

    publish_id: str = Field(..., description="発布ID")
    status: PublishStatus = Field(..., description="ステータス")
    target: PublishTarget = Field(..., description="発布ターゲット")
    phases: list[PublishPhase] = Field(default_factory=list, description="フェーズ一覧")
    current_phase: str = Field(default="", description="現在のフェーズ")
    progress: float = Field(default=0.0, ge=0.0, le=100.0, description="全体進捗率")
    started_at: datetime = Field(..., description="開始日時")
    completed_at: datetime | None = Field(default=None, description="完了日時")

    # 結果（成功時）
    deployment_id: str | None = Field(default=None, description="デプロイメントID")
    deployment_url: str | None = Field(default=None, description="デプロイメントURL")
    gallery_id: str | None = Field(default=None, description="GalleryアイテムID")

    # エラー（失敗時）
    error: str | None = Field(default=None, description="エラーメッセージ")
    error_details: dict[str, Any] | None = Field(default=None, description="エラー詳細")

    # メタデータ
    logs: list[str] = Field(default_factory=list, description="ログ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")


class PublishEvent(BaseModel):
    """発布イベント（SSE/WebSocket用）."""

    publish_id: str = Field(..., description="発布ID")
    event_type: str = Field(..., description="イベントタイプ")
    phase: str = Field(default="", description="フェーズ名")
    status: PublishStatus = Field(..., description="ステータス")
    message: str = Field(default="", description="メッセージ")
    progress: float = Field(default=0.0, description="進捗率")
    timestamp: datetime = Field(..., description="タイムスタンプ")
    data: dict[str, Any] = Field(default_factory=dict, description="追加データ")


class PublishHistoryItem(BaseModel):
    """発布履歴アイテム."""

    publish_id: str = Field(..., description="発布ID")
    component_id: str | None = Field(default=None, description="コンポーネントID")
    target: PublishTarget = Field(..., description="発布ターゲット")
    status: PublishStatus = Field(..., description="ステータス")
    version: str = Field(default="", description="バージョン")
    mode: PublishMode = Field(..., description="発布モード")
    deployment_url: str | None = Field(default=None, description="デプロイメントURL")
    started_at: datetime = Field(..., description="開始日時")
    completed_at: datetime | None = Field(default=None, description="完了日時")
    duration_ms: float | None = Field(default=None, description="所要時間（ミリ秒）")
