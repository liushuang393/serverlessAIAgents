"""AgentFlow Studio API モデル定義.

全APIエンドポイントで使用するPydanticリクエスト/レスポンスモデル。
"""

from __future__ import annotations

from typing import Any, Literal

from pydantic import BaseModel, Field


# =============================================================================
# エージェント API モデル
# =============================================================================


class AgentRunRequest(BaseModel):
    """エージェント実行リクエスト."""

    input_data: dict[str, Any] = Field(..., description="入力データ")


class AgentRunResponse(BaseModel):
    """エージェント実行レスポンス."""

    status: str = Field(..., description="実行ステータス")
    result: dict[str, Any] | None = Field(None, description="実行結果")
    error: str | None = Field(None, description="エラーメッセージ")


# =============================================================================
# ワークフロー API モデル
# =============================================================================


class WorkflowCreateRequest(BaseModel):
    """ワークフロー作成リクエスト."""

    name: str = Field(..., description="ワークフロー名")
    description: str = Field("", description="ワークフロー説明")
    nodes: list[dict[str, Any]] = Field(..., description="ノードリスト")
    edges: list[dict[str, Any]] = Field(..., description="エッジリスト")


class WorkflowUpdateRequest(BaseModel):
    """ワークフロー更新リクエスト."""

    name: str | None = Field(None, description="ワークフロー名")
    description: str | None = Field(None, description="ワークフロー説明")
    nodes: list[dict[str, Any]] | None = Field(None, description="ノードリスト")
    edges: list[dict[str, Any]] | None = Field(None, description="エッジリスト")


# =============================================================================
# マーケットプレイス API モデル
# =============================================================================


class MarketplaceSearchRequest(BaseModel):
    """マーケットプレイス検索リクエスト."""

    query: str | None = Field(None, description="検索クエリ")
    category: str | None = Field(None, description="カテゴリフィルター")
    protocols: list[str] | None = Field(None, description="プロトコルフィルター")


class MarketplaceInstallRequest(BaseModel):
    """マーケットプレイスインストールリクエスト."""

    agent_id: str = Field(..., description="エージェント ID")
    force: bool = Field(False, description="強制上書き")


# =============================================================================
# ナレッジベース API モデル
# =============================================================================


class KnowledgeAddRequest(BaseModel):
    """知識ベース追加リクエスト."""

    content: str = Field(..., description="ドキュメント内容")
    topic: str = Field("default", description="トピック分類")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class RAGQueryRequest(BaseModel):
    """RAG クエリリクエスト."""

    question: str = Field(..., description="質問文")
    topic: str | None = Field(None, description="検索対象トピック")


class ChatRequest(BaseModel):
    """チャットリクエスト."""

    session_id: str | None = Field(None, description="セッション ID")
    message: str = Field(..., description="ユーザーメッセージ")


# =============================================================================
# Preview API モデル (v0.3.0)
# =============================================================================


class PreviewRunRequest(BaseModel):
    """プレビュー実行リクエスト."""

    workflow: dict[str, Any] = Field(..., description="ワークフロー定義")
    input_data: dict[str, Any] = Field(default_factory=dict, description="入力データ")
    debug: bool = Field(False, description="デバッグモード")


class PreviewRunResponse(BaseModel):
    """プレビュー実行レスポンス."""

    status: str = Field(..., description="実行ステータス")
    result: dict[str, Any] | None = Field(None, description="実行結果")
    logs: list[dict[str, Any]] = Field(default_factory=list, description="実行ログ")
    duration_ms: float | None = Field(None, description="実行時間（ミリ秒）")
    error: str | None = Field(None, description="エラーメッセージ")


# =============================================================================
# Publish API モデル (v0.3.0)
# =============================================================================


class PublishExportRequest(BaseModel):
    """エクスポートリクエスト."""

    workflow: dict[str, Any] = Field(..., description="ワークフロー定義")
    target: Literal["fastapi", "cli", "vercel", "lambda", "docker"] = Field(
        ..., description="出力タイプ"
    )
    app_name: str | None = Field(None, description="アプリケーション名")
    version: str = Field("1.0.0", description="バージョン")
    include_tests: bool = Field(True, description="テストコードを含める")
    include_readme: bool = Field(True, description="README を含める")


class PublishDeployRequest(BaseModel):
    """デプロイリクエスト."""

    workflow: dict[str, Any] = Field(..., description="ワークフロー定義")
    target: Literal["vercel", "docker_hub"] = Field(..., description="デプロイ先")
    app_name: str | None = Field(None, description="アプリケーション名")
    credentials: dict[str, str] = Field(default_factory=dict, description="認証情報")


class PublishDeployResponse(BaseModel):
    """デプロイレスポンス."""

    status: str = Field(..., description="デプロイステータス")
    deployment_id: str | None = Field(None, description="デプロイメント ID")
    url: str | None = Field(None, description="デプロイ URL")
    logs: list[str] = Field(default_factory=list, description="デプロイログ")
    error: str | None = Field(None, description="エラーメッセージ")
