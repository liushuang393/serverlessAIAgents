"""skills mount CLI コマンドのテスト."""

from pathlib import Path

from click.testing import CliRunner

from agentflow.cli.main import cli


def _write_skill(skill_dir: Path, name: str) -> None:
    """最小構成の SKILL.md を作成."""
    skill_dir.mkdir(parents=True, exist_ok=True)
    (skill_dir / "SKILL.md").write_text(
        f"""---
name: {name}
description: This is a valid test skill description.
version: 1.0.0
triggers:
  - test
---

# Instructions

Run test steps.
""",
        encoding="utf-8",
    )


class TestSkillsMountCommand:
    """skills mount コマンドテスト."""

    def test_mount_help(self) -> None:
        """skills mount --help が表示できる."""
        runner = CliRunner()
        result = runner.invoke(cli, ["skills", "mount", "--help"])

        assert result.exit_code == 0
        assert "SOURCE" in result.output
        assert "--scope" in result.output

    def test_mount_single_skill_to_project(self) -> None:
        """単一 Skill を project scope へ挂载できる."""
        runner = CliRunner()
        with runner.isolated_filesystem():
            source = Path("external/my-skill")
            _write_skill(source, "my-skill")

            result = runner.invoke(
                cli,
                ["skills", "mount", str(source), "--scope", "project"],
            )

            assert result.exit_code == 0
            assert Path(".agentflow/skills/my-skill/SKILL.md").exists()

    def test_mount_conflict_without_force_fails(self) -> None:
        """同名 Skill がある場合、--force なしで失敗する."""
        runner = CliRunner()
        with runner.isolated_filesystem():
            source = Path("external/my-skill")
            _write_skill(source, "my-skill")

            first = runner.invoke(
                cli,
                ["skills", "mount", str(source), "--scope", "project"],
            )
            second = runner.invoke(
                cli,
                ["skills", "mount", str(source), "--scope", "project"],
            )

            assert first.exit_code == 0
            assert second.exit_code != 0
            assert "Target already exists" in second.output
