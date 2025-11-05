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
        # レジストリにエージェントが存在する可能性があるため、リストであることのみ確認
        assert isinstance(response.json(), list)

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
        # GET リクエストで CORS ヘッダーを確認
        response = client.get(
            "/api/agents",
            headers={"Origin": "http://localhost:3000"},
        )
        assert response.status_code == 200
        assert "access-control-allow-origin" in response.headers


class TestOpenAPI:
    """OpenAPI ドキュメントのテスト."""

    @pytest.mark.skip(reason="Pydantic RootModel issue in Python 3.13")
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


class TestDefaultDirectories:
    """デフォルトディレクトリのテスト."""

    def test_create_app_with_default_dirs(self):
        """デフォルトディレクトリで API を作成."""
        # agents_dir と workflows_dir を None にして作成
        app = create_app(agents_dir=None, workflows_dir=None)
        client = TestClient(app)

        # API が正常に動作することを確認
        response = client.get("/api/agents")
        assert response.status_code == 200
        assert isinstance(response.json(), list)


class TestAgentDetails:
    """エージェント詳細のテスト."""

    def test_get_agent_with_metadata(self, temp_dirs):
        """メタデータ付きエージェントを取得."""
        from unittest.mock import MagicMock, patch

        agents_dir, workflows_dir = temp_dirs

        # テスト用エージェントディレクトリを作成
        agent_dir = agents_dir / "test-agent"
        agent_dir.mkdir(parents=True)

        # agent.yaml を作成 (完全な有効なスキーマ)
        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """
meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test Author
  icon: 🤖
  category: test
  description: A test agent

interfaces:
  inputs:
    - name: input1
      type: string
      description: Test input
      required: true
  outputs:
    - name: output1
      type: string

protocols:
  mcp:
    tools: []
    resources: []
  a2a:
    enabled: true
    skills: []
  agui:
    enabled: true
    events: []

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: flow.py:flow
  shared_schema: schemas.py:Schema

visual:
  color: "#000000"
  size: medium
  ports: {}
""",
            encoding="utf-8",
        )

        # レジストリをモック
        with patch("agentflow.studio.api.LocalRegistry") as mock_registry_class:
            mock_registry = MagicMock()
            mock_agent_info = MagicMock()
            mock_agent_info.install_path = str(agent_dir)
            mock_agent_info.installed_at = "2024-01-01T00:00:00"
            mock_registry.get_agent.return_value = mock_agent_info
            mock_registry_class.return_value = mock_registry

            app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
            client = TestClient(app)

            response = client.get("/api/agents/test-agent")
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == "test-agent"
            assert "metadata" in data
            # metadata は AgentMetadata の model_dump() なので meta.name でアクセス
            assert data["metadata"]["meta"]["name"] == "Test Agent"

    def test_get_agent_metadata_not_found(self, temp_dirs):
        """メタデータファイルが存在しないエージェントを取得."""
        from unittest.mock import MagicMock, patch

        agents_dir, workflows_dir = temp_dirs

        # テスト用エージェントディレクトリを作成 (agent.yaml なし)
        agent_dir = agents_dir / "test-agent"
        agent_dir.mkdir(parents=True)

        # レジストリをモック
        with patch("agentflow.studio.api.LocalRegistry") as mock_registry_class:
            mock_registry = MagicMock()
            mock_agent_info = MagicMock()
            mock_agent_info.install_path = str(agent_dir)
            mock_registry.get_agent.return_value = mock_agent_info
            mock_registry_class.return_value = mock_registry

            app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
            client = TestClient(app)

            response = client.get("/api/agents/test-agent")
            assert response.status_code == 500
            assert "metadata not found" in response.json()["detail"].lower()


class TestAgentExecution:
    """エージェント実行のテスト."""

    def test_run_agent_success(self, temp_dirs):
        """エージェントを正常に実行."""
        from unittest.mock import MagicMock, patch

        agents_dir, workflows_dir = temp_dirs

        # テスト用エージェントディレクトリを作成
        agent_dir = agents_dir / "test-agent"
        agent_dir.mkdir(parents=True)

        # agent.yaml を作成 (完全な有効なスキーマ)
        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text(
            """
meta:
  id: test-agent
  name: Test Agent
  version: 1.0.0
  author: Test Author
  icon: 🤖
  category: test
  description: A test agent

interfaces:
  inputs:
    - name: input1
      type: string
      description: Test input
      required: true
  outputs:
    - name: output1
      type: string

protocols:
  mcp:
    tools: []
    resources: []
  a2a:
    enabled: true
    skills: []
  agui:
    enabled: true
    events: []

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: flow.py:flow
  shared_schema: schemas.py:Schema

visual:
  color: "#000000"
  size: medium
  ports: {}
""",
            encoding="utf-8",
        )

        # レジストリをモック
        with patch("agentflow.studio.api.LocalRegistry") as mock_registry_class:
            mock_registry = MagicMock()
            mock_agent_info = MagicMock()
            mock_agent_info.install_path = str(agent_dir)
            mock_registry.get_agent.return_value = mock_agent_info
            mock_registry_class.return_value = mock_registry

            app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
            client = TestClient(app)

            response = client.post(
                "/api/agents/test-agent/run",
                json={"input_data": {"text": "hello"}},
            )
            assert response.status_code == 200
            data = response.json()
            assert data["status"] == "success"
            assert "result" in data

    def test_run_agent_with_error(self, temp_dirs):
        """エージェント実行でエラーが発生."""
        from unittest.mock import MagicMock, patch

        agents_dir, workflows_dir = temp_dirs

        # テスト用エージェントディレクトリを作成 (不正な agent.yaml)
        agent_dir = agents_dir / "test-agent"
        agent_dir.mkdir(parents=True)

        # 不正な agent.yaml を作成
        agent_yaml = agent_dir / "agent.yaml"
        agent_yaml.write_text("invalid: yaml: content: [", encoding="utf-8")

        # レジストリをモック
        with patch("agentflow.studio.api.LocalRegistry") as mock_registry_class:
            mock_registry = MagicMock()
            mock_agent_info = MagicMock()
            mock_agent_info.install_path = str(agent_dir)
            mock_registry.get_agent.return_value = mock_agent_info
            mock_registry_class.return_value = mock_registry

            app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
            client = TestClient(app)

            response = client.post(
                "/api/agents/test-agent/run",
                json={"input_data": {"text": "hello"}},
            )
            assert response.status_code == 200
            data = response.json()
            assert data["status"] == "error"
            assert "error" in data


class TestMarketplaceInstall:
    """マーケットプレイスインストールのテスト."""

    def test_install_agent_success(self, temp_dirs):
        """エージェントを正常にインストール."""
        from unittest.mock import AsyncMock, MagicMock, patch

        agents_dir, workflows_dir = temp_dirs

        # MarketplaceClient をモック
        with patch("agentflow.studio.api.MarketplaceClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.install = AsyncMock()
            mock_client_class.return_value = mock_client

            app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
            client = TestClient(app)

            response = client.post(
                "/api/marketplace/install",
                json={"agent_id": "test-agent", "force": False},
            )
            assert response.status_code == 200
            data = response.json()
            assert data["status"] == "success"
            assert "installed successfully" in data["message"]


class TestWorkflowsWithFiles:
    """ワークフローファイルのテスト."""

    def test_list_workflows_with_files(self, temp_dirs):
        """ワークフローファイルが存在する場合のリスト取得."""
        agents_dir, workflows_dir = temp_dirs

        # テスト用ワークフローファイルを作成
        workflow_file = workflows_dir / "test-workflow.yaml"
        workflow_file.write_text(
            """
name: Test Workflow
description: A test workflow
nodes: []
edges: []
""",
            encoding="utf-8",
        )

        app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
        client = TestClient(app)

        response = client.get("/api/workflows")
        assert response.status_code == 200
        workflows = response.json()
        assert len(workflows) > 0
        assert any(w["id"] == "test-workflow" for w in workflows)


class TestWorkflowUpdate:
    """ワークフロー更新のテスト."""

    def test_update_workflow_all_fields(self, temp_dirs):
        """ワークフローの全フィールドを更新."""
        import yaml

        agents_dir, workflows_dir = temp_dirs

        # テスト用ワークフローファイルを作成
        workflow_file = workflows_dir / "test-workflow.yaml"
        initial_data = {
            "name": "Old Name",
            "description": "Old description",
            "nodes": [],
            "edges": [],
        }
        with open(workflow_file, "w", encoding="utf-8") as f:
            yaml.dump(initial_data, f, allow_unicode=True)

        app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
        client = TestClient(app)

        # 全フィールドを更新
        response = client.put(
            "/api/workflows/test-workflow",
            json={
                "name": "New Name",
                "description": "New description",
                "nodes": [{"id": "node1", "type": "agent"}],
                "edges": [{"from": "node1", "to": "node2"}],
            },
        )
        assert response.status_code == 200
        data = response.json()
        assert data["name"] == "New Name"

        # ファイルが更新されたことを確認
        with open(workflow_file, encoding="utf-8") as f:
            updated_data = yaml.safe_load(f)
        assert updated_data["name"] == "New Name"
        assert updated_data["description"] == "New description"
        assert len(updated_data["nodes"]) == 1
        assert len(updated_data["edges"]) == 1

    def test_update_workflow_partial_fields(self, temp_dirs):
        """ワークフローの一部フィールドのみ更新."""
        import yaml

        agents_dir, workflows_dir = temp_dirs

        # テスト用ワークフローファイルを作成
        workflow_file = workflows_dir / "test-workflow.yaml"
        initial_data = {
            "name": "Old Name",
            "description": "Old description",
            "nodes": [],
            "edges": [],
        }
        with open(workflow_file, "w", encoding="utf-8") as f:
            yaml.dump(initial_data, f, allow_unicode=True)

        app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
        client = TestClient(app)

        # name のみ更新
        response = client.put(
            "/api/workflows/test-workflow",
            json={"name": "New Name Only"},
        )
        assert response.status_code == 200

        # ファイルが更新されたことを確認
        with open(workflow_file, encoding="utf-8") as f:
            updated_data = yaml.safe_load(f)
        assert updated_data["name"] == "New Name Only"
        assert updated_data["description"] == "Old description"  # 変更なし


class TestWorkflowExecution:
    """ワークフロー実行のテスト."""

    def test_run_workflow_success(self, temp_dirs):
        """ワークフローを正常に実行."""
        agents_dir, workflows_dir = temp_dirs

        # テスト用ワークフローファイルを作成
        workflow_file = workflows_dir / "test-workflow.yaml"
        workflow_file.write_text(
            """
name: Test Workflow
description: A test workflow
nodes: []
edges: []
""",
            encoding="utf-8",
        )

        app = create_app(agents_dir=agents_dir, workflows_dir=workflows_dir)
        client = TestClient(app)

        response = client.post(
            "/api/workflows/test-workflow/run",
            json={"input_data": {"text": "hello"}},
        )
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "success"
        assert "result" in data
