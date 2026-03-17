"""AgentFlow CLI create コマンドのテスト."""

from pathlib import Path

from click.testing import CliRunner

from platform.cli.commands.create import validate_agent_id
from platform.cli.main import cli


class TestValidateAgentId:
    """validate_agent_id 関数のテスト."""

    def test_valid_kebab_case(self) -> None:
        """有効な kebab-case ID をテスト."""
        assert validate_agent_id("my-agent")
        assert validate_agent_id("test-agent-123")
        assert validate_agent_id("a")

    def test_invalid_uppercase(self) -> None:
        """大文字を含む ID が無効であることをテスト."""
        assert not validate_agent_id("My-Agent")
        assert not validate_agent_id("TEST")

    def test_invalid_underscore(self) -> None:
        """アンダースコアを含む ID が無効であることをテスト."""
        assert not validate_agent_id("my_agent")

    def test_invalid_special_chars(self) -> None:
        """特殊文字を含む ID が無効であることをテスト."""
        assert not validate_agent_id("my@agent")
        assert not validate_agent_id("test.agent")


class TestCreateCommand:
    """create コマンドグループのテスト."""

    def test_create_help(self) -> None:
        """create --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["create", "--help"])

        assert result.exit_code == 0
        assert "create" in result.output.lower()


class TestCreateAgentCommand:
    """create agent コマンドのテスト."""

    def test_create_agent_help(self) -> None:
        """create agent --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["create", "agent", "--help"])

        assert result.exit_code == 0
        assert "agent" in result.output.lower()
        assert "AGENT_NAME" in result.output

    def test_create_agent_basic(self, tmp_path: Path) -> None:
        """create agent が基本的なエージェントを作成することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["create", "agent", "test-agent"])

            assert result.exit_code == 0
            assert "created successfully" in result.output.lower()

            # エージェントディレクトリが作成されたことを確認
            agent_dir = Path("test-agent")
            assert agent_dir.exists()
            assert agent_dir.is_dir()

            # 必須ファイルが作成されたことを確認
            assert (agent_dir / "agent.yaml").exists()
            assert (agent_dir / "main.py").exists()
            assert (agent_dir / "tests").exists()
            assert (agent_dir / "tests" / "__init__.py").exists()

    def test_create_agent_with_author(self, tmp_path: Path) -> None:
        """--author フラグが動作することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["create", "agent", "test-agent", "--author", "Test Author"],
            )

            assert result.exit_code == 0

            # agent.yaml に作成者名が含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")

            assert "Test Author" in content

    def test_create_agent_with_description(self, tmp_path: Path) -> None:
        """--description フラグが動作することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["create", "agent", "test-agent", "--description", "Test Description"],
            )

            assert result.exit_code == 0

            # agent.yaml に説明が含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")

            assert "Test Description" in content

    def test_create_agent_with_icon(self, tmp_path: Path) -> None:
        """--icon フラグが動作することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["create", "agent", "test-agent", "--icon", "🚀"],
            )

            assert result.exit_code == 0

            # agent.yaml にアイコンが含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")

            assert "🚀" in content

    def test_create_agent_with_category(self, tmp_path: Path) -> None:
        """--category フラグが動作することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["create", "agent", "test-agent", "--category", "test-category"],
            )

            assert result.exit_code == 0

            # agent.yaml にカテゴリが含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")

            assert "test-category" in content

    def test_create_agent_disable_protocols(self, tmp_path: Path) -> None:
        """プロトコルを無効化できることをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["create", "agent", "test-agent", "--no-mcp", "--no-agui"],
            )

            assert result.exit_code == 0

            # agent.yaml に A2A のみが含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")

            assert "a2a:" in content
            assert "mcp:" not in content
            assert "agui:" not in content

    def test_create_agent_existing_directory(self, tmp_path: Path) -> None:
        """既存ディレクトリがある場合にエラーになることをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            # 最初のエージェントを作成
            result1 = runner.invoke(cli, ["create", "agent", "test-agent"])
            assert result1.exit_code == 0

            # 同じ名前でもう一度作成しようとする
            result2 = runner.invoke(cli, ["create", "agent", "test-agent"])
            assert result2.exit_code != 0
            assert "already exists" in result2.output.lower()

    def test_create_agent_invalid_name(self, tmp_path: Path) -> None:
        """無効なエージェント名でエラーになることをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["create", "agent", "invalid@name"])

            assert result.exit_code != 0
            assert "Invalid agent name" in result.output or "kebab-case" in result.output

    def test_create_agent_missing_argument(self) -> None:
        """引数なしで create agent を実行するとエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["create", "agent"])

        assert result.exit_code != 0
        assert "Missing argument" in result.output or "Error" in result.output

    def test_generated_files_are_valid(self, tmp_path: Path) -> None:
        """生成されたファイルが有効であることをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["create", "agent", "test-agent"])
            assert result.exit_code == 0

            agent_dir = Path("test-agent")

            # agent.yaml が有効な YAML であることを確認
            import yaml

            agent_yaml = agent_dir / "agent.yaml"
            with agent_yaml.open("r", encoding="utf-8") as f:
                data = yaml.safe_load(f)
                assert data is not None
                assert "meta" in data
                assert "interfaces" in data
                assert "protocols" in data

            # main.py が有効な Python であることを確認
            main_py = agent_dir / "main.py"
            content = main_py.read_text(encoding="utf-8")
            compile(content, str(main_py), "exec")  # 構文エラーがないことを確認

    def test_create_agent_verbose_flag(self, tmp_path: Path) -> None:
        """--verbose フラグが動作することをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["--verbose", "create", "agent", "test-agent"])

            assert result.exit_code == 0
            # verbose 出力が含まれることを確認
            # (実際の出力は実装に依存)
