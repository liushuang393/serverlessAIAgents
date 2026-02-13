"""flow CLI コマンドのテスト."""

from pathlib import Path

import yaml
from click.testing import CliRunner

from agentflow.cli.main import cli


class TestFlowCommand:
    """flow コマンドテスト."""

    def test_flow_run_help(self) -> None:
        """flow run --help が表示できる."""
        runner = CliRunner()
        result = runner.invoke(cli, ["flow", "run", "--help"])

        assert result.exit_code == 0
        assert "WORKFLOW_PATH" in result.output
        assert "--stream" in result.output

    def test_flow_run_success(self, tmp_path: Path) -> None:
        """workflow YAML を実行できる."""
        runner = CliRunner()
        workflow_file = tmp_path / "workflow.yaml"
        workflow_file.write_text(
            yaml.safe_dump(
                {
                    "workflow_type": "reflection",
                    "task": "test task",
                    "config": {"max_iterations": 1},
                },
                allow_unicode=True,
                sort_keys=False,
            ),
            encoding="utf-8",
        )

        result = runner.invoke(cli, ["flow", "run", str(workflow_file), "--json"])

        assert result.exit_code == 0
        assert '"success": true' in result.output.lower()

    def test_flow_run_requires_workflow_type(self, tmp_path: Path) -> None:
        """workflow_type がない場合はエラー."""
        runner = CliRunner()
        workflow_file = tmp_path / "workflow.yaml"
        workflow_file.write_text(
            yaml.safe_dump({"task": "missing workflow type"}, allow_unicode=True),
            encoding="utf-8",
        )

        result = runner.invoke(cli, ["flow", "run", str(workflow_file)])

        assert result.exit_code != 0
        assert "workflow_type" in result.output

