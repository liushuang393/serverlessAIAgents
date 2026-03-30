"""CLI-Native skills router tests."""

from __future__ import annotations

import asyncio
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING

import pytest
from fastapi import FastAPI

from control_plane.routers.skills import init_skill_services
from control_plane.routers.skills import router as skills_router
from control_plane.schemas.cli_native_schemas import CLINativeBuildResponse
from control_plane.services.skill_catalog import SkillCatalogService
from infrastructure.cli_native.service import CLINativeService, CommandExecutionResult
from tests.control_plane.conftest import SyncASGIClient


if TYPE_CHECKING:
    from pathlib import Path


def _create_sample_harness(base_dir: Path) -> Path:
    """Router テスト用 harness を作成する."""
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
    """Router tests 用の fake service."""

    def __init__(self, *, workspace_root: Path) -> None:
        super().__init__(workspace_root=workspace_root)

    def _run_command(self, command: list[str], cwd: Path | None = None) -> CommandExecutionResult:
        del cwd
        command_str = " ".join(command)
        if "pip install -e" in command_str:
            return CommandExecutionResult(return_code=0, stdout="installed", stderr="")
        if command[-1] == "--help":
            if "cli-anything-libreoffice" in command and "document" not in command:
                return CommandExecutionResult(
                    return_code=0,
                    stdout=(
                        "Usage: cli-anything-libreoffice [OPTIONS] COMMAND [ARGS]...\n\n"
                        "Options:\n"
                        "  --json\n"
                        "  --project TEXT\n\n"
                        "Commands:\n"
                        "  document  Document management commands.\n"
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
                        "  new  Create a new document.\n"
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
                        "  --help\n"
                    ),
                    stderr="",
                )
        if command[:2] == ["git", "clone"]:
            return CommandExecutionResult(return_code=0, stdout="cloned", stderr="")
        if command[:2] == ["git", "checkout"]:
            return CommandExecutionResult(return_code=0, stdout="checked out", stderr="")
        return CommandExecutionResult(return_code=0, stdout="", stderr="")


@pytest.fixture
def cli_native_test_client(tmp_path: Path) -> SyncASGIClient:
    """CLI-Native router を注入した test client."""
    app = FastAPI()
    app.include_router(skills_router)

    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    skills_dir = tmp_path / "skills"
    skills_dir.mkdir()
    (skills_dir / "base-skill").mkdir()
    (skills_dir / "base-skill" / "SKILL.md").write_text(
        "---\nname: base-skill\ndescription: base skill\ntags: [base]\n---\n",
        encoding="utf-8",
    )
    catalog = SkillCatalogService(skills_dir=skills_dir)
    asyncio.run(catalog.scan())
    init_skill_services(catalog, cli_native_service=_FakeCLINativeService(workspace_root=tmp_path))

    client = SyncASGIClient(app)
    try:
        yield client
    finally:
        client.close()


def test_cli_native_import_and_list(cli_native_test_client: SyncASGIClient, tmp_path: Path) -> None:
    """import 後に list/detail で取得できる."""
    harness_dir = _create_sample_harness(tmp_path)

    imported = cli_native_test_client.post(
        "/api/studios/framework/skills/cli-native/import",
        json={"harness_path": str(harness_dir)},
    )
    assert imported.status_code == 200
    harness = imported.json()["harness"]
    assert harness["harness_id"] == "cli-anything-libreoffice"

    listed = cli_native_test_client.get("/api/studios/framework/skills/cli-native")
    assert listed.status_code == 200
    payload = listed.json()
    assert payload["total"] == 1
    assert payload["harnesses"][0]["cli_command"] == "cli-anything-libreoffice"

    detail = cli_native_test_client.get("/api/studios/framework/skills/cli-native/cli-anything-libreoffice")
    assert detail.status_code == 200
    assert detail.json()["harness"]["software_name"] == "libreoffice"


def test_cli_native_build_route_returns_plan(cli_native_test_client: SyncASGIClient, tmp_path: Path) -> None:
    """build route が plan を返す."""
    resp = cli_native_test_client.post(
        "/api/studios/framework/skills/cli-native/build",
        json={"software_name": "LibreOffice", "source_path": str(tmp_path), "dry_run": True},
    )
    assert resp.status_code == 200
    parsed = CLINativeBuildResponse.model_validate(resp.json())
    assert parsed.software_name == "LibreOffice"
    assert parsed.dry_run is True
    assert parsed.runtime_cli == "codex"
