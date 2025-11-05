"""AgentFlow Studio REST API.

FastAPI アプリケーションで、エージェント管理、ワークフロー実行、
マーケットプレイス統合のための REST エンドポイントを提供します。
"""

from __future__ import annotations

import asyncio
from pathlib import Path
from typing import Any

from fastapi import FastAPI, HTTPException, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.schemas import SchemaLoader
from agentflow.marketplace.client import MarketplaceClient
from agentflow.marketplace.registry import LocalRegistry


# リクエスト/レスポンスモデル
class AgentRunRequest(BaseModel):
    """エージェント実行リクエスト."""

    input_data: dict[str, Any] = Field(..., description="入力データ")


class AgentRunResponse(BaseModel):
    """エージェント実行レスポンス."""

    status: str = Field(..., description="実行ステータス")
    result: dict[str, Any] | None = Field(None, description="実行結果")
    error: str | None = Field(None, description="エラーメッセージ")


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


class MarketplaceSearchRequest(BaseModel):
    """マーケットプレイス検索リクエスト."""

    query: str | None = Field(None, description="検索クエリ")
    category: str | None = Field(None, description="カテゴリフィルター")
    protocols: list[str] | None = Field(None, description="プロトコルフィルター")


class MarketplaceInstallRequest(BaseModel):
    """マーケットプレイスインストールリクエスト."""

    agent_id: str = Field(..., description="エージェント ID")
    force: bool = Field(False, description="強制上書き")


def create_app(
    agents_dir: Path | None = None,
    workflows_dir: Path | None = None,
) -> FastAPI:
    """FastAPI アプリケーションを作成.

    Args:
        agents_dir: エージェントディレクトリ
        workflows_dir: ワークフローディレクトリ

    Returns:
        FastAPI アプリケーション
    """
    app = FastAPI(
        title="AgentFlow Studio API",
        description="REST API for AgentFlow visual workflow editor",
        version="1.0.0",
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
    AgentFlowEngine()
    registry = LocalRegistry()
    marketplace = MarketplaceClient()
    active_connections: dict[str, WebSocket] = {}

    # エージェント API
    @app.get("/api/agents")
    async def list_agents() -> list[dict[str, Any]]:
        """インストール済みエージェント一覧を取得."""
        installed = registry.list_agents()
        return [
            {
                "id": agent.id,
                "name": agent.name,
                "version": agent.version,
                "description": agent.description,
                "category": agent.category,
                "installed_at": agent.installed_at,
            }
            for agent in installed
        ]

    @app.get("/api/agents/{agent_id}")
    async def get_agent(agent_id: str) -> dict[str, Any]:
        """エージェント詳細を取得."""
        agent_info = registry.get_agent(agent_id)
        if not agent_info:
            raise HTTPException(status_code=404, detail="Agent not found")

        # メタデータを読み込む
        agent_path = Path(agent_info.install_path)
        metadata_path = agent_path / "agent.yaml"

        if not metadata_path.exists():
            raise HTTPException(status_code=500, detail="Agent metadata not found")

        loader = SchemaLoader()
        metadata = loader.load_from_file(metadata_path)

        return {
            "id": agent_id,
            "metadata": metadata.model_dump(),
            "path": str(agent_path),
            "installed_at": agent_info.installed_at,
        }

    @app.post("/api/agents/{agent_id}/run")
    async def run_agent(agent_id: str, request: AgentRunRequest) -> AgentRunResponse:
        """エージェントを実行."""
        agent_info = registry.get_agent(agent_id)
        if not agent_info:
            raise HTTPException(status_code=404, detail="Agent not found")

        agent_path = Path(agent_info.install_path)
        metadata_path = agent_path / "agent.yaml"

        try:
            # エージェントを動的にロード (簡易実装)
            # 実際には main.py を import して実行
            loader = SchemaLoader()
            loader.load_from_file(metadata_path)

            # ダミー実行 (実際には AgentBlock をロードして実行)
            result = {
                "message": f"Agent {agent_id} executed successfully",
                "input": request.input_data,
            }

            return AgentRunResponse(status="success", result=result)

        except Exception as e:
            return AgentRunResponse(status="error", result=None, error=str(e))

    @app.get("/api/agents/{agent_id}/events")
    async def stream_agent_events(agent_id: str) -> StreamingResponse:
        """エージェントイベントを SSE でストリーミング."""

        async def event_generator():
            """SSE イベントジェネレーター."""
            # ダミーイベント
            for i in range(5):
                yield f"data: {{'type': 'log', 'message': 'Step {i+1}'}}\n\n"
                await asyncio.sleep(1)
            yield "data: {'type': 'complete', 'message': 'Done'}\n\n"

        return StreamingResponse(event_generator(), media_type="text/event-stream")

    # マーケットプレイス API
    @app.post("/api/marketplace/search")
    async def search_marketplace(
        request: MarketplaceSearchRequest,
    ) -> list[dict[str, Any]]:
        """マーケットプレイスを検索."""
        results = marketplace.search(
            query=request.query,
            category=request.category,
            protocols=request.protocols,
        )
        return [
            {
                "id": agent.id,
                "name": agent.name,
                "version": agent.version,
                "description": agent.description,
                "author": agent.author,
                "category": agent.category,
                "protocols": agent.protocols,
            }
            for agent in results
        ]

    @app.post("/api/marketplace/install")
    async def install_agent(
        request: MarketplaceInstallRequest,
    ) -> dict[str, Any]:
        """マーケットプレイスからエージェントをインストール."""
        try:
            await marketplace.install(request.agent_id, force=request.force)
            return {
                "status": "success",
                "message": f"Agent {request.agent_id} installed successfully",
            }
        except Exception as e:
            raise HTTPException(status_code=500, detail=str(e))

    # ワークフロー API
    @app.get("/api/workflows")
    async def list_workflows() -> list[dict[str, Any]]:
        """ワークフロー一覧を取得."""
        workflows = []
        for workflow_file in workflows_dir.glob("*.yaml"):
            workflows.append(
                {
                    "id": workflow_file.stem,
                    "name": workflow_file.stem,
                    "path": str(workflow_file),
                }
            )
        return workflows

    @app.post("/api/workflows")
    async def create_workflow(
        request: WorkflowCreateRequest,
    ) -> dict[str, Any]:
        """ワークフローを作成."""
        workflow_id = request.name.lower().replace(" ", "-")
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if workflow_path.exists():
            raise HTTPException(status_code=409, detail="Workflow already exists")

        # ワークフローを保存 (簡易実装)
        import yaml

        workflow_data = {
            "name": request.name,
            "description": request.description,
            "nodes": request.nodes,
            "edges": request.edges,
        }

        with open(workflow_path, "w", encoding="utf-8") as f:
            yaml.dump(workflow_data, f, allow_unicode=True)

        return {
            "id": workflow_id,
            "name": request.name,
            "path": str(workflow_path),
        }

    @app.put("/api/workflows/{workflow_id}")
    async def update_workflow(workflow_id: str, request: WorkflowUpdateRequest) -> dict[str, Any]:
        """ワークフローを更新."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        # ワークフローを読み込む
        import yaml

        with open(workflow_path, encoding="utf-8") as f:
            workflow_data = yaml.safe_load(f)

        # 更新
        if request.name:
            workflow_data["name"] = request.name
        if request.description is not None:
            workflow_data["description"] = request.description
        if request.nodes:
            workflow_data["nodes"] = request.nodes
        if request.edges:
            workflow_data["edges"] = request.edges

        # 保存
        with open(workflow_path, "w", encoding="utf-8") as f:
            yaml.dump(workflow_data, f, allow_unicode=True)

        return {
            "id": workflow_id,
            "name": workflow_data["name"],
            "path": str(workflow_path),
        }

    @app.post("/api/workflows/{workflow_id}/run")
    async def run_workflow(workflow_id: str, request: AgentRunRequest) -> AgentRunResponse:
        """ワークフローを実行."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        try:
            # ワークフローを実行 (簡易実装)
            result = {
                "message": f"Workflow {workflow_id} executed successfully",
                "input": request.input_data,
            }
            return AgentRunResponse(status="success", result=result)

        except Exception as e:
            return AgentRunResponse(status="error", result=None, error=str(e))

    # WebSocket エンドポイント
    @app.websocket("/ws/{client_id}")
    async def websocket_endpoint(websocket: WebSocket, client_id: str) -> None:
        """WebSocket 接続を処理."""
        await websocket.accept()
        active_connections[client_id] = websocket

        try:
            while True:
                data = await websocket.receive_text()
                # エコーバック (簡易実装)
                await websocket.send_text(f"Echo: {data}")

        except WebSocketDisconnect:
            del active_connections[client_id]

    return app
