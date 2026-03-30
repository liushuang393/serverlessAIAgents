"""CLI-Native gateway integration tests."""

from __future__ import annotations

import asyncio
import json
from typing import TYPE_CHECKING

from apps.messaging_hub.skills_manager import SkillsManager

from contracts.skill import CLIHarnessManifest
from kernel.skills.factory import create_skill_gateway


if TYPE_CHECKING:
    from pathlib import Path


def test_create_skill_gateway_registers_cli_native_skills(tmp_path: Path) -> None:
    """manifest があれば gateway と SkillsManager に露出される."""
    manifests_dir = tmp_path / ".bizcore" / "cli_native" / "manifests"
    manifests_dir.mkdir(parents=True)
    manifest = CLIHarnessManifest(
        harness_id="cli-anything-libreoffice",
        software_name="LibreOffice",
        source_ref="import:test",
        package_dir=str(tmp_path / "managed"),
        cli_command="cli-anything-libreoffice",
        skill_md_path=str(tmp_path / "managed" / "SKILL.md"),
        command_groups={"document": ["new"]},
        install_state="installed",
        risk_profile="high",
    )
    (manifests_dir / "cli-anything-libreoffice.json").write_text(
        json.dumps(manifest.to_payload(), ensure_ascii=False),
        encoding="utf-8",
    )

    gateway = create_skill_gateway(workspace_path=tmp_path)
    available_names = {skill.name for skill in gateway.list_available_skills()}

    assert "cli_native_libreoffice_execute" in available_names

    manager = SkillsManager(gateway=gateway)
    skills = asyncio.run(manager.list_available_skills())
    skill_names = {skill.name for skill in skills}

    assert "cli_native_libreoffice_execute" in skill_names
    cli_skill = next(skill for skill in skills if skill.name == "cli_native_libreoffice_execute")
    assert cli_skill.risk_level == "high"
    assert cli_skill.requires_confirmation is True
