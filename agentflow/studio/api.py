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
        version="0.2.0",
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
        """インストール済みエージェント一覧を取得.

        v0.2.0: @agent デコレータで定義されたAgentも含む
        """
        result = []

        # 方式1: レジストリから取得（agent.yaml ベース）
        installed = registry.list_agents()
        for agent in installed:
            result.append({
                "id": agent.id,
                "name": agent.name,
                "version": agent.version,
                "description": agent.description,
                "category": agent.category,
                "installed_at": agent.installed_at,
                "type": "yaml",  # agent.yaml ベース
            })

        # 方式2: @agent デコレータで定義されたAgent（v0.2.0 NEW）
        try:
            from agentflow.agent_decorator import AgentClient

            decorated_agents = AgentClient.list_agents()
            for agent_name in decorated_agents:
                # 重複チェック
                if not any(a["id"] == agent_name for a in result):
                    result.append({
                        "id": agent_name,
                        "name": agent_name,
                        "version": "0.2.0",
                        "description": f"@agent decorator agent: {agent_name}",
                        "category": "decorator",
                        "installed_at": None,
                        "type": "decorator",  # @agent デコレータ
                    })
        except Exception:
            # @agent が利用できない場合はスキップ
            pass

        return result

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
        """エージェントを実行.

        v0.2.0: @agent デコレータで定義されたAgentもサポート
        """
        try:
            # 方式1: @agent デコレータで定義されたAgent（v0.2.0 NEW）
            try:
                from agentflow.agent_decorator import AgentClient

                if agent_id in AgentClient.list_agents():
                    client = AgentClient.get(agent_id)
                    result = await client.invoke(request.input_data)
                    return AgentRunResponse(status="success", result=result)
            except (ImportError, ValueError):
                # @agent が利用できない、またはAgentが見つからない場合は次へ
                pass

            # 方式2: agent.yaml ベース（従来）
            agent_info = registry.get_agent(agent_id)
            if not agent_info:
                raise HTTPException(status_code=404, detail="Agent not found")

            agent_path = Path(agent_info.install_path)
            metadata_path = agent_path / "agent.yaml"

            # エージェントを動的にロード
            loader = SchemaLoader()
            metadata = loader.load_from_file(metadata_path)

            # entry から Flow を取得
            entry_parts = metadata.pocketflow.entry.split(":")
            if len(entry_parts) != 2:
                raise ValueError(f"Invalid entry format: {metadata.pocketflow.entry}")

            module_path, flow_name = entry_parts
            entry_point = agent_path / module_path

            if not entry_point.exists():
                raise FileNotFoundError(f"Entry point not found: {entry_point}")

            # モジュールをロード
            from importlib.util import module_from_spec, spec_from_file_location
            import sys

            spec = spec_from_file_location("agent_module", entry_point)
            if spec is None or spec.loader is None:
                raise ValueError(f"Failed to load module: {entry_point}")

            module = module_from_spec(spec)
            sys.modules["agent_module"] = module
            spec.loader.exec_module(module)

            # Flowを取得
            flow = getattr(module, flow_name, None)
            if flow is None:
                raise ValueError(f"Flow '{flow_name}' not found in {entry_point}")

            # 実行
            if hasattr(flow, "run"):
                result = await flow.run(request.input_data)
            elif hasattr(flow, "run_async"):
                await flow.run_async(request.input_data)
                result = request.input_data
            else:
                raise ValueError(f"Flow '{flow_name}' has no run/run_async method")

            return AgentRunResponse(status="success", result=result)

        except Exception as e:
            return AgentRunResponse(status="error", result=None, error=str(e))

    @app.get("/api/agents/{agent_id}/events")
    async def stream_agent_events(agent_id: str) -> StreamingResponse:
        """エージェントイベントを SSE でストリーミング.

        v0.2.0: @agent デコレータで定義されたAgentもサポート
        """

        async def event_generator():
            """SSE イベントジェネレーター."""
            import json

            try:
                # 方式1: @agent デコレータで定義されたAgent（v0.2.0 NEW）
                try:
                    from agentflow.agent_decorator import AgentClient

                    if agent_id in AgentClient.list_agents():
                        client = AgentClient.get(agent_id)
                        async for chunk in client.stream({}):
                            event_data = {
                                "type": "chunk",
                                "data": chunk,
                            }
                            yield f"data: {json.dumps(event_data, ensure_ascii=False)}\n\n"
                        yield "data: {'type': 'complete', 'message': 'Done'}\n\n"
                        return
                except (ImportError, ValueError):
                    pass

                # 方式2: create_flow の run_stream（v0.2.0 NEW）
                agent_info = registry.get_agent(agent_id)
                if agent_info:
                    agent_path = Path(agent_info.install_path)
                    metadata_path = agent_path / "agent.yaml"

                    if metadata_path.exists():
                        loader = SchemaLoader()
                        metadata = loader.load_from_file(metadata_path)

                        entry_parts = metadata.pocketflow.entry.split(":")
                        if len(entry_parts) == 2:
                            module_path, flow_name = entry_parts
                            entry_point = agent_path / module_path

                            if entry_point.exists():
                                from importlib.util import module_from_spec, spec_from_file_location
                                import sys

                                spec = spec_from_file_location("agent_module", entry_point)
                                if spec and spec.loader:
                                    module = module_from_spec(spec)
                                    sys.modules["agent_module"] = module
                                    spec.loader.exec_module(module)

                                    flow = getattr(module, flow_name, None)
                                    if flow and hasattr(flow, "run_stream"):
                                        async for event in flow.run_stream({}):
                                            yield f"data: {json.dumps(event, ensure_ascii=False)}\n\n"
                                        return

                # フォールバック: ダミーイベント
                for i in range(5):
                    yield f"data: {{'type': 'log', 'message': 'Step {i+1}'}}\n\n"
                    await asyncio.sleep(1)
                yield "data: {'type': 'complete', 'message': 'Done'}\n\n"

            except Exception as e:
                error_data = {"type": "error", "message": str(e)}
                yield f"data: {json.dumps(error_data, ensure_ascii=False)}\n\n"

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

    # ========================================
    # Knowledge / RAG API
    # ========================================
    from agentflow.skills.chatbot import ChatBotSkill
    from agentflow.skills.rag import RAGSkill

    # グローバル RAG/ChatBot インスタンス（遅延初期化）
    rag_skill: RAGSkill | None = None
    chatbot_skill: ChatBotSkill | None = None

    @app.post("/api/knowledge/add")
    async def add_knowledge(request: KnowledgeAddRequest) -> dict[str, Any]:
        """知識ベースにドキュメントを追加."""
        nonlocal rag_skill
        if rag_skill is None:
            rag_skill = RAGSkill()
            await rag_skill.start()

        doc_id = await rag_skill.add_document(
            content=request.content,
            topic=request.topic,
            metadata=request.metadata,
        )
        return {"status": "success", "document_id": doc_id}

    @app.post("/api/knowledge/query")
    async def query_knowledge(request: RAGQueryRequest) -> dict[str, Any]:
        """RAG で質問応答."""
        nonlocal rag_skill
        if rag_skill is None:
            rag_skill = RAGSkill()
            await rag_skill.start()

        result = await rag_skill.query(
            question=request.question,
            topic=request.topic,
        )
        return {
            "answer": result.answer,
            "sources": result.sources,
        }

    @app.get("/api/knowledge/status")
    async def get_knowledge_status() -> dict[str, Any]:
        """知識ベース状態を取得."""
        if rag_skill is None:
            return {"status": "not_initialized"}
        return rag_skill.get_status()

    # ========================================
    # ChatBot API
    # ========================================
    @app.post("/api/chat")
    async def chat(request: ChatRequest) -> dict[str, Any]:
        """チャット応答を生成."""
        nonlocal chatbot_skill, rag_skill

        # ChatBot 初期化（RAG 連携あり）
        if chatbot_skill is None:
            if rag_skill is None:
                rag_skill = RAGSkill()
                await rag_skill.start()

            from agentflow.skills.chatbot import ChatBotConfig

            chatbot_skill = ChatBotSkill(
                config=ChatBotConfig(enable_rag=True),
                rag_skill=rag_skill,
            )

        # セッション管理
        if request.session_id:
            session = chatbot_skill.get_session(request.session_id)
            if not session:
                session = chatbot_skill.create_session()
        else:
            session = chatbot_skill.create_session()

        # チャット実行
        response = await chatbot_skill.chat(session.id, request.message)

        return {
            "session_id": session.id,
            "response": response,
        }

    @app.get("/api/chat/sessions")
    async def list_chat_sessions() -> list[dict[str, Any]]:
        """チャットセッション一覧を取得."""
        if chatbot_skill is None:
            return []
        return chatbot_skill.list_sessions()

    @app.delete("/api/chat/sessions/{session_id}")
    async def delete_chat_session(session_id: str) -> dict[str, Any]:
        """チャットセッションを削除."""
        if chatbot_skill is None:
            raise HTTPException(status_code=404, detail="ChatBot not initialized")

        if chatbot_skill.clear_session(session_id):
            return {"status": "success"}
        raise HTTPException(status_code=404, detail="Session not found")

    return app
