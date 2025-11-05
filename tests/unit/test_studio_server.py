"""Studio Server のユニットテスト."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from agentflow.studio.server import StudioServer, main


@pytest.fixture
def temp_dirs(tmp_path: Path) -> tuple[Path, Path]:
    """一時ディレクトリを作成.

    Args:
        tmp_path: pytest の一時パス

    Returns:
        エージェントディレクトリとワークフローディレクトリのタプル
    """
    agents_dir = tmp_path / "agents"
    workflows_dir = tmp_path / "workflows"
    agents_dir.mkdir(parents=True)
    workflows_dir.mkdir(parents=True)
    return agents_dir, workflows_dir


class TestStudioServer:
    """Studio Server のテスト."""

    def test_server_initialization(self, temp_dirs: tuple[Path, Path]) -> None:
        """サーバーの初期化をテスト."""
        agents_dir, workflows_dir = temp_dirs
        server = StudioServer(
            host="127.0.0.1",
            port=8080,
            agents_dir=agents_dir,
            workflows_dir=workflows_dir,
            reload=True,
        )

        assert server.host == "127.0.0.1"
        assert server.port == 8080
        assert server.agents_dir == agents_dir
        assert server.workflows_dir == workflows_dir
        assert server.reload is True
        assert server.app is not None
        assert server.console is not None

    def test_server_initialization_defaults(self) -> None:
        """デフォルト設定でサーバーを初期化できることをテスト."""
        server = StudioServer()

        assert server.host == "0.0.0.0"
        assert server.port == 8000
        assert server.agents_dir is None
        assert server.workflows_dir is None
        assert server.reload is False
        assert server.app is not None

    def test_get_app(self, temp_dirs: tuple[Path, Path]) -> None:
        """FastAPI アプリを取得できることをテスト."""
        agents_dir, workflows_dir = temp_dirs
        server = StudioServer(
            agents_dir=agents_dir,
            workflows_dir=workflows_dir,
        )

        app = server.get_app()
        assert app is not None
        assert app.title == "AgentFlow Studio API"

    @patch("agentflow.studio.server.uvicorn.run")
    def test_run_server(self, mock_uvicorn_run: MagicMock, temp_dirs: tuple[Path, Path]) -> None:
        """サーバーを起動できることをテスト (ブロッキング)."""
        agents_dir, workflows_dir = temp_dirs
        server = StudioServer(
            host="127.0.0.1",
            port=8080,
            agents_dir=agents_dir,
            workflows_dir=workflows_dir,
            reload=True,
        )

        server.run()

        # uvicorn.run が呼ばれたことを確認
        mock_uvicorn_run.assert_called_once()
        call_kwargs = mock_uvicorn_run.call_args[1]
        assert call_kwargs["host"] == "127.0.0.1"
        assert call_kwargs["port"] == 8080
        assert call_kwargs["reload"] is True
        assert call_kwargs["log_level"] == "info"

    @patch("agentflow.studio.server.uvicorn.Server")
    async def test_start_server(
        self, mock_server_class: MagicMock, temp_dirs: tuple[Path, Path]
    ) -> None:
        """サーバーを起動できることをテスト (非同期)."""
        from unittest.mock import AsyncMock

        agents_dir, workflows_dir = temp_dirs
        server = StudioServer(
            host="127.0.0.1",
            port=8080,
            agents_dir=agents_dir,
            workflows_dir=workflows_dir,
            reload=False,
        )

        # モックサーバーを設定
        mock_server = MagicMock()
        mock_server.serve = AsyncMock()
        mock_server_class.return_value = mock_server

        await server.start()

        # uvicorn.Server が作成されたことを確認
        mock_server_class.assert_called_once()
        # serve が呼ばれたことを確認
        mock_server.serve.assert_called_once()

    @patch("agentflow.studio.server.uvicorn.run")
    def test_run_server_with_defaults(self, mock_uvicorn_run: MagicMock) -> None:
        """デフォルト設定でサーバーを起動できることをテスト."""
        server = StudioServer()

        server.run()

        # uvicorn.run が呼ばれたことを確認
        mock_uvicorn_run.assert_called_once()
        call_kwargs = mock_uvicorn_run.call_args[1]
        assert call_kwargs["host"] == "0.0.0.0"
        assert call_kwargs["port"] == 8000
        assert call_kwargs["reload"] is False


class TestCLI:
    """CLI エントリーポイントのテスト."""

    @patch("agentflow.studio.server.StudioServer")
    @patch("sys.argv", ["agentflow-studio"])
    def test_main_with_defaults(self, mock_server_class: MagicMock) -> None:
        """デフォルト引数で main を実行できることをテスト."""
        mock_server = MagicMock()
        mock_server_class.return_value = mock_server

        main()

        # StudioServer が作成されたことを確認
        mock_server_class.assert_called_once_with(
            host="0.0.0.0",
            port=8000,
            agents_dir=None,
            workflows_dir=None,
            reload=False,
        )
        # run が呼ばれたことを確認
        mock_server.run.assert_called_once()

    @patch("agentflow.studio.server.StudioServer")
    @patch("sys.argv", ["agentflow-studio", "--host", "127.0.0.1", "--port", "9000"])
    def test_main_with_custom_host_port(self, mock_server_class: MagicMock) -> None:
        """カスタムホストとポートで main を実行できることをテスト."""
        mock_server = MagicMock()
        mock_server_class.return_value = mock_server

        main()

        # StudioServer が作成されたことを確認
        mock_server_class.assert_called_once_with(
            host="127.0.0.1",
            port=9000,
            agents_dir=None,
            workflows_dir=None,
            reload=False,
        )
        # run が呼ばれたことを確認
        mock_server.run.assert_called_once()

    @patch("agentflow.studio.server.StudioServer")
    @patch(
        "sys.argv",
        [
            "agentflow-studio",
            "--agents-dir",
            "/tmp/agents",
            "--workflows-dir",
            "/tmp/workflows",
            "--reload",
        ],
    )
    def test_main_with_directories_and_reload(self, mock_server_class: MagicMock) -> None:
        """ディレクトリとリロードオプション付きで main を実行できることをテスト."""
        mock_server = MagicMock()
        mock_server_class.return_value = mock_server

        main()

        # StudioServer が作成されたことを確認
        call_kwargs = mock_server_class.call_args[1]
        assert call_kwargs["host"] == "0.0.0.0"
        assert call_kwargs["port"] == 8000
        assert call_kwargs["agents_dir"] == Path("/tmp/agents")
        assert call_kwargs["workflows_dir"] == Path("/tmp/workflows")
        assert call_kwargs["reload"] is True
        # run が呼ばれたことを確認
        mock_server.run.assert_called_once()

    @patch("agentflow.studio.server.StudioServer")
    @patch(
        "sys.argv",
        [
            "agentflow-studio",
            "--host",
            "localhost",
            "--port",
            "3000",
            "--agents-dir",
            "/custom/agents",
            "--workflows-dir",
            "/custom/workflows",
            "--reload",
        ],
    )
    def test_main_with_all_options(self, mock_server_class: MagicMock) -> None:
        """全オプション付きで main を実行できることをテスト."""
        mock_server = MagicMock()
        mock_server_class.return_value = mock_server

        main()

        # StudioServer が作成されたことを確認
        mock_server_class.assert_called_once_with(
            host="localhost",
            port=3000,
            agents_dir=Path("/custom/agents"),
            workflows_dir=Path("/custom/workflows"),
            reload=True,
        )
        # run が呼ばれたことを確認
        mock_server.run.assert_called_once()
