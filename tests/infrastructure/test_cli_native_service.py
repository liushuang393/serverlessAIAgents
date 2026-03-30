"""CLI-Native service tests."""

from __future__ import annotations

import json
from typing import TYPE_CHECKING

import pytest

from infrastructure.cli_native.service import CLINativeService, CommandExecutionResult


if TYPE_CHECKING:
    from pathlib import Path


def _create_sample_harness(base_dir: Path) -> Path:
    """テスト用 harness を作成する."""
    harness_dir = base_dir / "libreoffice" / "agent-harness"
    skill_dir = harness_dir / "cli_anything" / "libreoffice" / "skills"
    skill_dir.mkdir(parents=True)
    (harness_dir / "setup.py").write_text(
        (
            "from setuptools import setup\n"
            "setup(\n"
            "    name='cli-anything-libreoffice',\n"
            "    entry_points={'console_scripts': ['cli-anything-libreoffice=demo:main']},\n"
            ")\n"
        ),
        encoding="utf-8",
    )
    (skill_dir / "SKILL.md").write_text(
        (
            "---\n"
            "name: cli-anything-libreoffice\n"
            "description: LibreOffice CLI harness\n"
            "---\n\n"
            "# cli-anything-libreoffice\n"
        ),
        encoding="utf-8",
    )
    return harness_dir


class _FakeCLINativeService(CLINativeService):
    """コマンド実行を固定値で返すテスト用 service."""

    def __init__(self, *, workspace_root: Path) -> None:
        super().__init__(workspace_root=workspace_root)
        self.commands: list[list[str]] = []

    def _run_command(self, command: list[str], cwd: Path | None = None) -> CommandExecutionResult:
        del cwd
        self.commands.append(command)
        command_str = " ".join(command)

        if "pip install -e" in command_str:
            return CommandExecutionResult(return_code=0, stdout="installed", stderr="")
        if command[-1] == "--help":
            if "cli-anything-libreoffice" in command and "document" not in command and "writer" not in command:
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice [OPTIONS] COMMAND [ARGS]...\n\n"
                        "Options:\n"
                        "  --json\n"
                        "  --project TEXT\n\n"
                        "Commands:\n"
                        "  document  Document management commands.\n"
                        "  writer    Writer commands.\n"
                    ),
                    stderr="",
                )
            if len(command) >= 3 and command[-2] == "document":
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice document [OPTIONS] COMMAND [ARGS]...\n\n"
                        "Options:\n"
                        "  --help\n\n"
                        "Commands:\n"
                        "  new   Create a new document.\n"
                        "  open  Open an existing document.\n"
                    ),
                    stderr="",
                )
            if len(command) >= 3 and command[-2] == "writer":
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice writer [OPTIONS] COMMAND [ARGS]...\n\n"
                        "Options:\n"
                        "  --help\n\n"
                        "Commands:\n"
                        "  add-heading  Add heading.\n"
                    ),
                    stderr="",
                )
            if len(command) >= 4 and command[-2] == "new":
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice document new [OPTIONS]\n\n"
                        "Options:\n"
                        "  --type TEXT\n"
                        "  --name TEXT\n"
                        "  --output TEXT\n"
                        "  --help\n"
                    ),
                    stderr="",
                )
            if len(command) >= 4 and command[-2] == "add-heading":
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice writer add-heading [OPTIONS]\n\n"
                        "Options:\n"
                        "  --text TEXT\n"
                        "  --level INTEGER\n"
                        "  --help\n"
                    ),
                    stderr="",
                )

        if "document new" in command_str:
            return CommandExecutionResult(
                return_code=0,
                stdout=json.dumps({"created": True, "type": "writer"}),
                stderr="",
            )

        return CommandExecutionResult(return_code=0, stdout="", stderr="")


def test_import_harness_creates_manifest_and_shim(tmp_path: Path) -> None:
    """import_harness() が manifest と shim skill を生成する."""
    harness_dir = _create_sample_harness(tmp_path)
    service = _FakeCLINativeService(workspace_root=tmp_path)

    manifest = service.import_harness(harness_path=harness_dir)

    assert manifest.harness_id == "cli-anything-libreoffice"
    assert manifest.cli_command == "cli-anything-libreoffice"
    assert manifest.install_state == "installed"
    assert manifest.command_groups["document"] == ["new", "open"]
    assert manifest.command_groups["writer"] == ["add-heading"]
    assert (tmp_path / ".bizcore" / "skills" / manifest.harness_id / "SKILL.md").is_file()
    assert service.get_manifest(manifest.harness_id) is not None


def test_execute_harness_dry_run_returns_validated_command(tmp_path: Path) -> None:
    """dry-run 実行が `--json` 強制コマンドを返す."""
    harness_dir = _create_sample_harness(tmp_path)
    service = _FakeCLINativeService(workspace_root=tmp_path)
    manifest = service.import_harness(harness_path=harness_dir)

    result = service.execute_harness(
        harness_id=manifest.harness_id,
        subcommand="document new",
        args=["--type", "writer", "--name", "Report"],
        project_path="/tmp/report.json",
        dry_run=True,
    )

    assert result["validated"] is True
    assert result["command"][:6] == ["conda", "run", "-n", "agentflow", "cli-anything-libreoffice", "--json"]
    assert "--project" in result["command"]


def test_execute_harness_rejects_unknown_option(tmp_path: Path) -> None:
    """許可されていない option は拒否する."""
    harness_dir = _create_sample_harness(tmp_path)
    service = _FakeCLINativeService(workspace_root=tmp_path)
    manifest = service.import_harness(harness_path=harness_dir)

    with pytest.raises(ValueError, match="option not allowed"):
        service.execute_harness(
            harness_id=manifest.harness_id,
            subcommand="document new",
            args=["--unknown-option"],
            dry_run=True,
        )
