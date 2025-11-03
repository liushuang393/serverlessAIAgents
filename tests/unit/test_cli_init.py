"""AgentFlow CLI init コマンドのテスト."""

from pathlib import Path

import pytest
from click.testing import CliRunner

from agentflow.cli.commands.init import init, validate_agent_id
from agentflow.cli.main import cli


class TestValidateAgentId:
    """validate_agent_id 関数のテスト."""

    def test_valid_kebab_case(self) -> None:
        """有効な kebab-case ID をテスト."""
        assert validate_agent_id("my-agent")
        assert validate_agent_id("test-agent-123")
        assert validate_agent_id("a")
        assert validate_agent_id("agent-1-2-3")

    def test_invalid_uppercase(self) -> None:
        """大文字を含む ID が無効であることをテスト."""
        assert not validate_agent_id("My-Agent")
        assert not validate_agent_id("TEST")

    def test_invalid_underscore(self) -> None:
        """アンダースコアを含む ID が無効であることをテスト."""
        assert not validate_agent_id("my_agent")
        assert not validate_agent_id("test_123")

    def test_invalid_special_chars(self) -> None:
        """特殊文字を含む ID が無効であることをテスト."""
        assert not validate_agent_id("my@agent")
        assert not validate_agent_id("test.agent")
        assert not validate_agent_id("agent!")


class TestInitCommand:
    """init コマンドのテスト."""

    def test_init_help(self) -> None:
        """init --help が動作することをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["init", "--help"])

        assert result.exit_code == 0
        assert "init" in result.output.lower()
        assert "PROJECT_NAME" in result.output

    def test_init_creates_project(self, tmp_path: Path) -> None:
        """init コマンドがプロジェクトを作成することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["init", "test-agent"])
            
            assert result.exit_code == 0
            assert "created successfully" in result.output.lower()
            
            # プロジェクトディレクトリが作成されたことを確認
            project_dir = Path("test-agent")
            assert project_dir.exists()
            assert project_dir.is_dir()
            
            # 必須ファイルが作成されたことを確認
            assert (project_dir / "agent.yaml").exists()
            assert (project_dir / "main.py").exists()
            assert (project_dir / "README.md").exists()
            assert (project_dir / "requirements.txt").exists()
            assert (project_dir / ".gitignore").exists()
            
            # tests ディレクトリが作成されたことを確認
            assert (project_dir / "tests").exists()
            assert (project_dir / "tests" / "__init__.py").exists()

    def test_init_with_protocols_flag(self, tmp_path: Path) -> None:
        """--protocols フラグが動作することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["init", "test-agent", "--protocols", "mcp", "--protocols", "a2a"],
            )
            
            assert result.exit_code == 0
            
            # agent.yaml に指定したプロトコルが含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")
            
            assert "mcp:" in content
            assert "a2a:" in content
            # agui は指定していないので含まれない
            assert "agui:" not in content

    def test_init_with_author_flag(self, tmp_path: Path) -> None:
        """--author フラグが動作することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["init", "test-agent", "--author", "Test Author"],
            )
            
            assert result.exit_code == 0
            
            # agent.yaml に作成者名が含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")
            
            assert "Test Author" in content

    def test_init_with_description_flag(self, tmp_path: Path) -> None:
        """--description フラグが動作することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(
                cli,
                ["init", "test-agent", "--description", "Test Description"],
            )
            
            assert result.exit_code == 0
            
            # agent.yaml に説明が含まれることを確認
            agent_yaml = Path("test-agent") / "agent.yaml"
            content = agent_yaml.read_text(encoding="utf-8")
            
            assert "Test Description" in content

    def test_init_dry_run(self, tmp_path: Path) -> None:
        """--dry-run フラグが動作することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["init", "test-agent", "--dry-run"])
            
            assert result.exit_code == 0
            assert "Dry-run mode" in result.output
            
            # ファイルが作成されていないことを確認
            project_dir = Path("test-agent")
            assert not project_dir.exists()

    def test_init_existing_project(self, tmp_path: Path) -> None:
        """既存プロジェクトがある場合にエラーになることをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            # 最初のプロジェクトを作成
            result1 = runner.invoke(cli, ["init", "test-agent"])
            assert result1.exit_code == 0
            
            # 同じ名前でもう一度作成しようとする
            result2 = runner.invoke(cli, ["init", "test-agent"])
            assert result2.exit_code != 0
            assert "already exists" in result2.output.lower()

    def test_init_invalid_project_name(self, tmp_path: Path) -> None:
        """無効なプロジェクト名でエラーになることをテスト."""
        runner = CliRunner()

        with runner.isolated_filesystem(temp_dir=tmp_path):
            # 特殊文字を含む名前はエラーになる
            result = runner.invoke(cli, ["init", "invalid@name"])

            assert result.exit_code != 0
            assert "Invalid project name" in result.output or "kebab-case" in result.output

    def test_init_missing_argument(self) -> None:
        """引数なしで init を実行するとエラーになることをテスト."""
        runner = CliRunner()
        result = runner.invoke(cli, ["init"])
        
        assert result.exit_code != 0
        assert "Missing argument" in result.output or "Error" in result.output

    def test_init_verbose_flag(self, tmp_path: Path) -> None:
        """--verbose フラグが動作することをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["--verbose", "init", "test-agent"])
            
            assert result.exit_code == 0
            # verbose 出力が含まれることを確認
            # (実際の出力は実装に依存)

    def test_generated_files_are_valid(self, tmp_path: Path) -> None:
        """生成されたファイルが有効であることをテスト."""
        runner = CliRunner()
        
        with runner.isolated_filesystem(temp_dir=tmp_path):
            result = runner.invoke(cli, ["init", "test-agent"])
            assert result.exit_code == 0
            
            project_dir = Path("test-agent")
            
            # agent.yaml が有効な YAML であることを確認
            import yaml
            agent_yaml = project_dir / "agent.yaml"
            with agent_yaml.open("r", encoding="utf-8") as f:
                data = yaml.safe_load(f)
                assert data is not None
                assert "meta" in data
                assert "interfaces" in data
                assert "protocols" in data
            
            # main.py が有効な Python であることを確認
            main_py = project_dir / "main.py"
            content = main_py.read_text(encoding="utf-8")
            compile(content, str(main_py), "exec")  # 構文エラーがないことを確認

