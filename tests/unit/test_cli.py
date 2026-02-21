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
        assert "0.2.0" in result.output

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
        assert "Installed Agents" in result.output or "No agents found" in result.output


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


class TestSkillsCommand:
    """skills コマンドグループのテスト."""

    def test_skills_help(self) -> None:
        """skills --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "--help"])

        assert result.exit_code == 0
        assert "skills" in result.output.lower()
        assert "list" in result.output.lower()
        assert "show" in result.output.lower()
        assert "create" in result.output.lower()
        assert "validate" in result.output.lower()
        assert "search" in result.output.lower()
        assert "delete" in result.output.lower()
        assert "mount" in result.output.lower()

    def test_skills_list(self) -> None:
        """skills list コマンドが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "list"])

        # exit_code 0 または出力があればOK
        assert result.exit_code == 0
        # "skills" または "Total" が出力に含まれる
        assert "skill" in result.output.lower() or "total" in result.output.lower() or "No skills" in result.output

    def test_skills_list_learned_option(self) -> None:
        """skills list --learned オプションが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "list", "--learned"])

        assert result.exit_code == 0

    def test_skills_list_project_option(self) -> None:
        """skills list --project オプションが動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "list", "--project"])

        assert result.exit_code == 0

    def test_skills_show_missing_name(self) -> None:
        """skills show に引数がない場合エラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "show"])

        assert result.exit_code != 0
        assert "Missing argument" in result.output or "Error" in result.output

    def test_skills_show_not_found(self) -> None:
        """skills show で存在しない Skill を指定した場合."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "show", "nonexistent-skill"])

        assert result.exit_code == 0
        assert "not found" in result.output.lower()

    def test_skills_create_help(self) -> None:
        """skills create --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "create", "--help"])

        assert result.exit_code == 0
        assert "name" in result.output.lower()
        assert "--description" in result.output
        assert "--triggers" in result.output
        assert "--scope" in result.output

    def test_skills_create_invalid_name(self) -> None:
        """skills create で無効な名前を指定した場合."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "create", "Invalid_Name"])

        assert result.exit_code == 0
        assert "invalid" in result.output.lower() or "kebab-case" in result.output.lower()

    def test_skills_validate_help(self) -> None:
        """skills validate --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "validate", "--help"])

        assert result.exit_code == 0
        assert "path" in result.output.lower()
        assert "--strict" in result.output

    def test_skills_validate_nonexistent_path(self) -> None:
        """skills validate で存在しないパスを指定した場合."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "validate", "/nonexistent/path"])

        assert result.exit_code != 0

    def test_skills_search_help(self) -> None:
        """skills search --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "search", "--help"])

        assert result.exit_code == 0
        assert "query" in result.output.lower()
        assert "--top" in result.output

    def test_skills_search_no_results(self) -> None:
        """skills search で結果がない場合."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "search", "xyznonexistent123"])

        assert result.exit_code == 0
        # "No skills" または検索結果の出力
        assert "skill" in result.output.lower()

    def test_skills_delete_help(self) -> None:
        """skills delete --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "delete", "--help"])

        assert result.exit_code == 0
        assert "name" in result.output.lower()
        assert "--scope" in result.output
        assert "--force" in result.output


class TestFlowGroup:
    """flow コマンドグループのテスト."""

    def test_flow_help(self) -> None:
        """flow --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["flow", "--help"])

        assert result.exit_code == 0
        assert "run" in result.output.lower()


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
