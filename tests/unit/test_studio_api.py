"""AgentFlow Studio API のユニットテスト."""

from __future__ import annotations

import tempfile
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from agentflow.studio.api import create_app


@pytest.fixture
def temp_dirs():
    """一時ディレクトリを作成."""
    with (
        tempfile.TemporaryDirectory() as agents_dir,
        tempfile.TemporaryDirectory() as workflows_dir,
    ):
        yield Path(agents_dir), Path(workflows_dir)


@pytest.fixture
def client(temp_dirs):
    """テストクライアントを作成."""
    agents_dir, workflows_dir = temp_dirs
    app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
    return TestClient(app)


class TestAgentsAPI:
    """エージェント API のテスト."""

    def test_list_agents_empty(self, client):
        """空のエージェントリストを取得."""
        response = client.get("/api/agents")
        assert response.status_code == 200
        assert response.json() == []

    def test_get_agent_not_found(self, client):
        """存在しないエージェントを取得."""
        response = client.get("/api/agents/nonexistent")
        assert response.status_code == 404
        assert "not found" in response.json()["detail"].lower()

    def test_run_agent_not_found(self, client):
        """存在しないエージェントを実行."""
        response = client.post(
            "/api/agents/nonexistent/run",
            json={"input_data": {"text": "hello"}},
        )
        assert response.status_code == 404

    def test_stream_agent_events(self, client):
        """エージェントイベントをストリーミング."""
        response = client.get("/api/agents/test-agent/events")
        assert response.status_code == 200
        assert response.headers["content-type"] == "text/event-stream; charset=utf-8"


class TestMarketplaceAPI:
    """マーケットプレイス API のテスト."""

    def test_search_marketplace_empty(self, client):
        """空の検索結果を取得."""
        response = client.post(
            "/api/marketplace/search",
            json={"query": None, "category": None, "protocols": None},
        )
        assert response.status_code == 200
        # マーケットプレイスが空の場合は空リスト
        assert isinstance(response.json(), list)

    def test_search_marketplace_with_query(self, client):
        """クエリ付きで検索."""
        response = client.post(
            "/api/marketplace/search",
            json={"query": "text", "category": None, "protocols": None},
        )
        assert response.status_code == 200
        assert isinstance(response.json(), list)

    def test_search_marketplace_with_filters(self, client):
        """フィルター付きで検索."""
        response = client.post(
            "/api/marketplace/search",
            json={
                "query": "processor",
                "category": "utility",
                "protocols": ["mcp", "a2a"],
            },
        )
        assert response.status_code == 200
        assert isinstance(response.json(), list)

    def test_install_agent_not_found(self, client):
        """存在しないエージェントをインストール."""
        response = client.post(
            "/api/marketplace/install",
            json={"agent_id": "nonexistent", "force": False},
        )
        # マーケットプレイスにエージェントが存在しない場合はエラー
        assert response.status_code == 500


class TestWorkflowsAPI:
    """ワークフロー API のテスト."""

    def test_list_workflows_empty(self, client):
        """空のワークフローリストを取得."""
        response = client.get("/api/workflows")
        assert response.status_code == 200
        assert response.json() == []

    def test_create_workflow(self, client):
        """ワークフローを作成."""
        response = client.post(
            "/api/workflows",
            json={
                "name": "Test Workflow",
                "description": "A test workflow",
                "nodes": [{"id": "node1", "type": "agent", "data": {"agent_id": "test"}}],
                "edges": [],
            },
        )
        assert response.status_code == 200
        data = response.json()
        assert data["id"] == "test-workflow"
        assert data["name"] == "Test Workflow"
        assert "path" in data

    def test_create_workflow_duplicate(self, client):
        """重複したワークフローを作成."""
        # 最初の作成
        client.post(
            "/api/workflows",
            json={
                "name": "Test Workflow",
                "description": "A test workflow",
                "nodes": [],
                "edges": [],
            },
        )

        # 重複作成
        response = client.post(
            "/api/workflows",
            json={
                "name": "Test Workflow",
                "description": "A test workflow",
                "nodes": [],
                "edges": [],
            },
        )
        assert response.status_code == 409
        assert "already exists" in response.json()["detail"].lower()

    def test_update_workflow(self, client):
        """ワークフローを更新."""
        # ワークフローを作成
        create_response = client.post(
            "/api/workflows",
            json={
                "name": "Test Workflow",
                "description": "Original description",
                "nodes": [],
                "edges": [],
            },
        )
        workflow_id = create_response.json()["id"]

        # 更新
        response = client.put(
            f"/api/workflows/{workflow_id}",
            json={
                "name": "Updated Workflow",
                "description": "Updated description",
                "nodes": [{"id": "node1"}],
                "edges": [{"id": "edge1"}],
            },
        )
        assert response.status_code == 200
        data = response.json()
        assert data["name"] == "Updated Workflow"

    def test_update_workflow_not_found(self, client):
        """存在しないワークフローを更新."""
        response = client.put(
            "/api/workflows/nonexistent",
            json={"name": "Updated", "description": None, "nodes": None, "edges": None},
        )
        assert response.status_code == 404

    def test_run_workflow(self, client):
        """ワークフローを実行."""
        # ワークフローを作成
        create_response = client.post(
            "/api/workflows",
            json={
                "name": "Test Workflow",
                "description": "A test workflow",
                "nodes": [],
                "edges": [],
            },
        )
        workflow_id = create_response.json()["id"]

        # 実行
        response = client.post(
            f"/api/workflows/{workflow_id}/run",
            json={"input_data": {"text": "hello"}},
        )
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "success"
        assert data["result"] is not None

    def test_run_workflow_not_found(self, client):
        """存在しないワークフローを実行."""
        response = client.post(
            "/api/workflows/nonexistent/run",
            json={"input_data": {"text": "hello"}},
        )
        assert response.status_code == 404


class TestWebSocket:
    """WebSocket のテスト."""

    def test_websocket_connection(self, client):
        """WebSocket 接続をテスト."""
        with client.websocket_connect("/ws/test-client") as websocket:
            # メッセージを送信
            websocket.send_text("Hello")

            # エコーバックを受信
            data = websocket.receive_text()
            assert data == "Echo: Hello"


class TestCORS:
    """CORS のテスト."""

    def test_cors_headers(self, client):
        """CORS ヘッダーを確認."""
        response = client.options(
            "/api/agents",
            headers={"Origin": "http://localhost:3000"},
        )
        assert response.status_code == 200
        assert "access-control-allow-origin" in response.headers


class TestOpenAPI:
    """OpenAPI ドキュメントのテスト."""

    def test_openapi_json(self, client):
        """OpenAPI JSON を取得."""
        response = client.get("/api/openapi.json")
        assert response.status_code == 200
        data = response.json()
        assert data["info"]["title"] == "AgentFlow Studio API"
        assert data["info"]["version"] == "1.0.0"

    def test_swagger_ui(self, client):
        """Swagger UI を取得."""
        response = client.get("/api/docs")
        assert response.status_code == 200
        assert "swagger" in response.text.lower()

    def test_redoc(self, client):
        """ReDoc を取得."""
        response = client.get("/api/redoc")
        assert response.status_code == 200
        assert "redoc" in response.text.lower()
