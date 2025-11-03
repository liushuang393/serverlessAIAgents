"""AgentFlow CLI のテスト."""

from click.testing import CliRunner

from agentflow.cli.main import cli


class TestCLI:
    """CLI メインコマンドのテスト."""

    def test_cli_help(self) -> None:
        """--help オプションが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["--help"])

        assert result.exit_code == 0
        assert "AgentFlow" in result.output
        assert "Lightweight AI Agent Development Framework" in result.output

    def test_cli_version(self) -> None:
        """--version オプションが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["--version"])

        assert result.exit_code == 0
        assert "agentflow" in result.output
        assert "0.1.0" in result.output

    def test_cli_verbose_flag(self) -> None:
        """--verbose フラグが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["--verbose", "list"])

        assert result.exit_code == 0
        # verbose フラグが設定されていることを確認
        # (実際の出力は実装に依存)

    def test_cli_no_command(self) -> None:
        """コマンドなしで実行した場合にヘルプが表示されることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, [])

        # Click はコマンドなしの場合 exit_code 0 を返す
        # (ヘルプを表示するため)
        assert result.exit_code in (0, 2)  # 2 は "no command" エラー
        assert "AgentFlow" in result.output or "Usage" in result.output


class TestCreateCommand:
    """create コマンドグループのテスト."""

    def test_create_help(self) -> None:
        """create --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["create", "--help"])

        assert result.exit_code == 0
        assert "create" in result.output.lower()


class TestProtocolsCommand:
    """protocols コマンドグループのテスト."""

    def test_protocols_help(self) -> None:
        """protocols --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["protocols", "--help"])

        assert result.exit_code == 0
        assert "protocols" in result.output.lower()


class TestRunCommand:
    """run コマンドのテスト."""

    def test_run_help(self) -> None:
        """run --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["run", "--help"])

        assert result.exit_code == 0
        assert "run" in result.output.lower()
        assert "AGENT_PATH" in result.output

    def test_run_missing_argument(self) -> None:
        """引数なしで run を実行するとエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["run"])

        assert result.exit_code != 0
        assert "Missing argument" in result.output or "Error" in result.output

    def test_run_nonexistent_path(self) -> None:
        """存在しないパスで run を実行するとエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["run", "/nonexistent/path"])

        assert result.exit_code != 0


class TestListCommand:
    """list コマンドのテスト."""

    def test_list_help(self) -> None:
        """list --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["list", "--help"])

        assert result.exit_code == 0
        assert "list" in result.output.lower()

    def test_list_command(self) -> None:
        """list コマンドが実行できることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["list"])

        assert result.exit_code == 0
        # 未実装メッセージが表示されることを確認
        assert "Not implemented" in result.output or "⚠" in result.output


class TestInfoCommand:
    """info コマンドのテスト."""

    def test_info_help(self) -> None:
        """info --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["info", "--help"])

        assert result.exit_code == 0
        assert "info" in result.output.lower()
        assert "AGENT_ID" in result.output

    def test_info_missing_argument(self) -> None:
        """引数なしで info を実行するとエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["info"])

        assert result.exit_code != 0
        assert "Missing argument" in result.output or "Error" in result.output

    def test_info_command(self) -> None:
        """info コマンドが実行できることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["info", "test-agent"])

        assert result.exit_code == 0
        # 未実装メッセージが表示されることを確認
        assert "Not implemented" in result.output or "⚠" in result.output


class TestErrorHandling:
    """エラーハンドリングのテスト."""

    def test_invalid_command(self) -> None:
        """無効なコマンドでエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["invalid-command"])

        assert result.exit_code != 0
        assert "Error" in result.output or "No such command" in result.output

    def test_help_message_on_error(self) -> None:
        """エラー時にヘルプメッセージが表示されることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["run"])

        assert result.exit_code != 0
        # エラーメッセージが含まれることを確認
        assert "Error" in result.output or "Missing" in result.output
