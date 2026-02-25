"""Unit tests for CLIRuntimeManager."""

from __future__ import annotations

import json

import pytest

from agentflow.tools.cli import runtime_manager as runtime_mod
from agentflow.tools.cli.runtime_manager import CLIRuntimeManager


@pytest.mark.asyncio
async def test_detect_codex_with_chat_auth(monkeypatch: pytest.MonkeyPatch) -> None:
    """Detect should parse codex status output."""
    manager = CLIRuntimeManager()

    def _which(cmd: str) -> str | None:
        return "/usr/bin/codex" if cmd == "codex" else None

    async def _run_command(self, cmd: list[str], **_kwargs):
        if cmd[:2] == ["codex", "--version"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="codex-cli 0.1.0", stderr="")
        if cmd[:3] == ["codex", "login", "status"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="Logged in using ChatGPT", stderr="")
        return runtime_mod._CommandResult(command=cmd, return_code=1, stdout="", stderr="unknown command")

    monkeypatch.setattr("agentflow.tools.cli.runtime_manager.shutil.which", _which)
    monkeypatch.setattr(CLIRuntimeManager, "_run_command", _run_command)

    result = await manager.detect()
    codex = result["tools"]["codex"]
    assert codex["detected"] is True
    assert codex["authenticated"] is True
    assert codex["auth_method"] == "chat_auth"


@pytest.mark.asyncio
async def test_ensure_authenticated_supports_api_key_env(monkeypatch: pytest.MonkeyPatch) -> None:
    """API key env should mark tool authenticated when status command is absent."""
    manager = CLIRuntimeManager()
    monkeypatch.setenv("TEST_API_KEY", "secret")
    monkeypatch.setattr(
        "agentflow.tools.cli.runtime_manager.shutil.which",
        lambda cmd: "/usr/bin/claude" if cmd == "claude" else None,
    )

    result = await manager.ensure_authenticated(
        {
            "preferred": ["claude"],
            "claude": {
                "executable": "claude",
                "auth": {
                    "status": None,
                    "api_key_env": "TEST_API_KEY",
                    "api_key_login": None,
                    "interactive_login": ["claude", "auth", "login"],
                },
            },
        }
    )
    assert result["tools"]["claude"]["authenticated"] is True
    assert result["tools"]["claude"]["auth_method"] == "api_key_env"


@pytest.mark.asyncio
async def test_run_diagnostic_prompt_uses_plan_mode_for_claude(monkeypatch: pytest.MonkeyPatch) -> None:
    """Claude diagnostic execution should enforce plan permission mode."""
    manager = CLIRuntimeManager()

    def _which(cmd: str) -> str | None:
        return "/usr/bin/claude" if cmd == "claude" else None

    async def _run_command(self, cmd: list[str], **_kwargs):
        if cmd[:2] == ["claude", "--version"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="2.1.0", stderr="")
        if cmd[:4] == ["claude", "auth", "status", "--json"]:
            return runtime_mod._CommandResult(
                command=cmd,
                return_code=0,
                stdout=json.dumps({"loggedIn": True, "authMethod": "claude.ai"}),
                stderr="",
            )
        if cmd[:2] == ["claude", "-p"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="diagnostic summary", stderr="")
        return runtime_mod._CommandResult(command=cmd, return_code=1, stdout="", stderr="unexpected")

    monkeypatch.setattr("agentflow.tools.cli.runtime_manager.shutil.which", _which)
    monkeypatch.setattr(CLIRuntimeManager, "_run_command", _run_command)

    result = await manager.run_diagnostic_prompt(
        prompt="check startup",
        runtime_cli={"preferred": ["claude"]},
    )
    assert result["success"] is True
    assert result["tool"] == "claude"
    assert "--permission-mode" in result["command"]
    assert "plan" in result["command"]


@pytest.mark.asyncio
async def test_run_repair_prompt_uses_workspace_write_for_codex(monkeypatch: pytest.MonkeyPatch) -> None:
    """Codex repair execution should request workspace-write sandbox."""
    manager = CLIRuntimeManager()

    def _which(cmd: str) -> str | None:
        return "/usr/bin/codex" if cmd == "codex" else None

    async def _run_command(self, cmd: list[str], **_kwargs):
        if cmd[:2] == ["codex", "--version"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="codex-cli 0.1.0", stderr="")
        if cmd[:3] == ["codex", "login", "status"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="Logged in using ChatGPT", stderr="")
        if cmd[:2] == ["codex", "exec"]:
            return runtime_mod._CommandResult(command=cmd, return_code=0, stdout="applied", stderr="")
        return runtime_mod._CommandResult(command=cmd, return_code=1, stdout="", stderr="unexpected")

    monkeypatch.setattr("agentflow.tools.cli.runtime_manager.shutil.which", _which)
    monkeypatch.setattr(CLIRuntimeManager, "_run_command", _run_command)

    result = await manager.run_repair_prompt(
        prompt="fix startup",
        runtime_cli={"preferred": ["codex"]},
    )
    assert result["success"] is True
    assert result["tool"] == "codex"
    assert "--sandbox" in result["command"]
    assert "workspace-write" in result["command"]
