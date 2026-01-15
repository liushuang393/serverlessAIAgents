"""AgentFlow Studio REST API.

FastAPI アプリケーションで、エージェント管理、ワークフロー実行、
マーケットプレイス統合、Preview/Publish機能のための REST エンドポイントを提供します。

v0.5.0: モジュラー化 - ルートを機能別ファイルに分割
- models.py: リクエスト/レスポンスモデル
- routes/: 機能別ルーターモジュール
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from fastapi import FastAPI, WebSocket
from fastapi.middleware.cors import CORSMiddleware

from agentflow.marketplace.client import MarketplaceClient
from agentflow.marketplace.registry import LocalRegistry

# モデルを再エクスポート（後方互換性）
from agentflow.studio.models import (
    AgentRunRequest,
    AgentRunResponse,
    ChatRequest,
    KnowledgeAddRequest,
    MarketplaceInstallRequest,
    MarketplaceSearchRequest,
    PreviewRunRequest,
    PreviewRunResponse,
    PublishDeployRequest,
    PublishDeployResponse,
    PublishExportRequest,
    RAGQueryRequest,
    WorkflowCreateRequest,
    WorkflowUpdateRequest,
)


def create_app(
    agents_dir: Path | None = None,
    workflows_dir: Path | None = None,
) -> FastAPI:
    """FastAPI アプリケーションを作成.

    v0.5.0: モジュラー化 - ルートを機能別ファイルに分割

    Args:
        agents_dir: エージェントディレクトリ
        workflows_dir: ワークフローディレクトリ

    Returns:
        FastAPI アプリケーション
    """
    from agentflow.studio.routes import (
        create_agents_router,
        create_knowledge_router,
        create_marketplace_router,
        create_preview_router,
        create_publish_router,
        create_service_nodes_router,
        create_workflows_router,
    )

    app = FastAPI(
        title="AgentFlow Studio API",
        description="REST API for AgentFlow visual workflow editor",
        version="0.5.0",
        docs_url="/api/docs",
        redoc_url="/api/redoc",
        openapi_url="/api/openapi.json",
    )

    # CORS 設定
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["http://localhost:3000", "http://localhost:5173"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    # デフォルトディレクトリ
    if agents_dir is None:
        agents_dir = Path.home() / ".agentflow" / "agents"
    if workflows_dir is None:
        workflows_dir = Path.home() / ".agentflow" / "workflows"

    # ディレクトリを作成
    agents_dir.mkdir(parents=True, exist_ok=True)
    workflows_dir.mkdir(parents=True, exist_ok=True)

    # グローバル状態
    registry = LocalRegistry()
    marketplace = MarketplaceClient()
    active_connections: dict[str, WebSocket] = {}

    # ルーターを登録
    app.include_router(
        create_agents_router(agents_dir, registry, active_connections),
    )
    app.include_router(create_workflows_router(workflows_dir))
    app.include_router(create_marketplace_router(marketplace))
    app.include_router(create_preview_router())
    app.include_router(create_publish_router())
    app.include_router(create_knowledge_router())
    app.include_router(create_service_nodes_router())

    # ヘルスチェック
    @app.get("/api/health")
    async def health_check() -> dict[str, Any]:
        """ヘルスチェック."""
        return {"status": "ok", "version": "0.5.0"}

    return app
