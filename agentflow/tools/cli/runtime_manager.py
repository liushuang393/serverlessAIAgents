"""Runtime manager for external CLIs used by app self-healing flows.

This module provides a safe, structured interface to:
- detect installed CLIs (Codex / Claude Code)
- run declarative install commands
- evaluate and bootstrap authentication (API key + interactive fallback)
- execute diagnostics in read-only/plan-oriented mode
"""

from __future__ import annotations

import asyncio
import json
import os
import shlex
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal


ToolName = Literal["codex", "claude"]
DiagnosticMode = Literal["read_only", "plan"]
RepairMode = Literal["workspace_write", "accept_edits"]

_SUPPORTED_TOOLS: tuple[ToolName, ...] = ("codex", "claude")
_DANGEROUS_TOKENS = {
    "--dangerously-bypass-approvals-and-sandbox",
    "--dangerously-skip-permissions",
    "--allow-dangerously-skip-permissions",
}


@dataclass(slots=True)
class _CommandResult:
    """Result of a single command execution."""

    command: list[str]
    return_code: int | None
    stdout: str
    stderr: str
    timed_out: bool = False
    error: str | None = None

    @property
    def ok(self) -> bool:
        """Whether execution succeeded."""
        return self.return_code == 0 and not self.timed_out and self.error is None

    def to_dict(self) -> dict[str, Any]:
        """Serialize result."""
        return {
            "command": " ".join(shlex.quote(x) for x in self.command),
            "return_code": self.return_code,
            "stdout": self.stdout,
            "stderr": self.stderr,
            "timed_out": self.timed_out,
            "error": self.error,
            "ok": self.ok,
        }


def _default_cli_config() -> dict[str, Any]:
    """Default runtime CLI config."""
    return {
        "preferred": ["codex", "claude"],
        "codex": {
            "executable": "codex",
            "install_commands": [["npm", "install", "-g", "@openai/codex"]],
            "auth": {
                "status": ["codex", "login", "status"],
                "api_key_env": "OPENAI_API_KEY",
                "api_key_login": ["codex", "login", "--with-api-key"],
                "interactive_login": ["codex", "login"],
            },
            "diagnostic_mode": "read_only",
            "diagnostic_command": [
                "codex",
                "exec",
                "--skip-git-repo-check",
                "--sandbox",
                "read-only",
            ],
            "repair_mode": "workspace_write",
            "repair_command": [
                "codex",
                "exec",
                "--skip-git-repo-check",
                "--sandbox",
                "workspace-write",
            ],
        },
        "claude": {
            "executable": "claude",
            "install_commands": [["npm", "install", "-g", "@anthropic-ai/claude-code"]],
            "auth": {
                "status": ["claude", "auth", "status", "--json"],
                "api_key_env": "ANTHROPIC_API_KEY",
                "api_key_login": None,
                "interactive_login": ["claude", "auth", "login"],
            },
            "diagnostic_mode": "plan",
            "diagnostic_command": [
                "claude",
                "-p",
                "--permission-mode",
                "plan",
            ],
            "repair_mode": "accept_edits",
            "repair_command": [
                "claude",
                "-p",
                "--permission-mode",
                "acceptEdits",
            ],
        },
    }


def _merge_runtime_cli_config(override: dict[str, Any] | None) -> dict[str, Any]:
    """Merge app runtime cli override into defaults."""
    base = _default_cli_config()
    if not isinstance(override, dict):
        return base

    preferred = override.get("preferred")
    if isinstance(preferred, list):
        normalized = [str(x).strip().lower() for x in preferred if str(x).strip().lower() in _SUPPORTED_TOOLS]
        if normalized:
            base["preferred"] = normalized

    for tool in _SUPPORTED_TOOLS:
        raw_tool = override.get(tool)
        if not isinstance(raw_tool, dict):
            continue
        target = base[tool]
        if isinstance(raw_tool.get("executable"), str):
            target["executable"] = raw_tool["executable"].strip() or target["executable"]
        if isinstance(raw_tool.get("diagnostic_mode"), str):
            mode = raw_tool["diagnostic_mode"].strip().lower()
            if mode in {"read_only", "plan"}:
                target["diagnostic_mode"] = mode
        if isinstance(raw_tool.get("install_commands"), list):
            cmds: list[list[str]] = []
            for item in raw_tool["install_commands"]:
                if isinstance(item, list) and item:
                    cmd = [str(part) for part in item if str(part).strip()]
                    if cmd:
                        cmds.append(cmd)
            target["install_commands"] = cmds
        if isinstance(raw_tool.get("diagnostic_command"), list):
            diag_cmd = [str(part) for part in raw_tool["diagnostic_command"] if str(part).strip()]
            if diag_cmd:
                target["diagnostic_command"] = diag_cmd
        if isinstance(raw_tool.get("repair_mode"), str):
            repair_mode = raw_tool["repair_mode"].strip().lower()
            if repair_mode in {"workspace_write", "accept_edits"}:
                target["repair_mode"] = repair_mode
        if isinstance(raw_tool.get("repair_command"), list):
            repair_cmd = [str(part) for part in raw_tool["repair_command"] if str(part).strip()]
            if repair_cmd:
                target["repair_command"] = repair_cmd

        raw_auth = raw_tool.get("auth")
        if isinstance(raw_auth, dict):
            auth_target = target["auth"]
            for key in ("status", "api_key_login", "interactive_login"):
                value = raw_auth.get(key)
                if value is None:
                    auth_target[key] = None
                    continue
                if isinstance(value, list):
                    parsed = [str(part) for part in value if str(part).strip()]
                    auth_target[key] = parsed or None
            api_key_env = raw_auth.get("api_key_env")
            if isinstance(api_key_env, str):
                auth_target["api_key_env"] = api_key_env.strip() or None
    return base


class CLIRuntimeManager:
    """Detect / install / authenticate external CLIs and run diagnostics safely."""

    def __init__(
        self,
        *,
        cwd: Path | None = None,
        timeout_seconds: float = 45.0,
    ) -> None:
        self._cwd = cwd or Path.cwd()
        self._timeout_seconds = timeout_seconds

    async def detect(self, runtime_cli: dict[str, Any] | None = None) -> dict[str, Any]:
        """Detect CLI availability and auth status."""
        config = _merge_runtime_cli_config(runtime_cli)
        result: dict[str, Any] = {"preferred": list(config["preferred"]), "tools": {}}
        for tool in _SUPPORTED_TOOLS:
            state = await self._detect_tool(tool, config[tool])
            result["tools"][tool] = state
        result["available_tools"] = [
            tool for tool in config["preferred"] if result["tools"].get(tool, {}).get("detected") is True
        ]
        result["authenticated_tools"] = [
            tool for tool in config["preferred"] if result["tools"].get(tool, {}).get("authenticated") is True
        ]
        return result

    async def ensure_installed(self, runtime_cli: dict[str, Any] | None = None) -> dict[str, Any]:
        """Ensure declared tools are installed by using declarative install commands."""
        config = _merge_runtime_cli_config(runtime_cli)
        report: dict[str, Any] = {"tools": {}}
        for tool in _SUPPORTED_TOOLS:
            tool_cfg = config[tool]
            detected = await self._detect_tool(tool, tool_cfg)
            if detected["detected"] is True:
                report["tools"][tool] = {
                    "detected": True,
                    "installed": True,
                    "install_attempted": False,
                    "install_logs": [],
                    "path": detected.get("path"),
                }
                continue

            install_logs: list[dict[str, Any]] = []
            installed = False
            for command in tool_cfg.get("install_commands", []):
                if not self._is_command_safe(command):
                    install_logs.append(
                        {
                            "command": " ".join(command),
                            "ok": False,
                            "error": "blocked_by_safety_policy",
                        }
                    )
                    continue
                executed = await self._run_command(command)
                install_logs.append(executed.to_dict())
                if executed.ok:
                    redetected = await self._detect_tool(tool, tool_cfg)
                    if redetected["detected"] is True:
                        installed = True
                        break
            report["tools"][tool] = {
                "detected": installed,
                "installed": installed,
                "install_attempted": True,
                "install_logs": install_logs,
            }
        return report

    async def ensure_authenticated(self, runtime_cli: dict[str, Any] | None = None) -> dict[str, Any]:
        """Ensure tools are authenticated, trying API key first then interactive fallback."""
        config = _merge_runtime_cli_config(runtime_cli)
        report: dict[str, Any] = {"tools": {}}
        for tool in _SUPPORTED_TOOLS:
            state = await self._ensure_tool_auth(tool, config[tool])
            report["tools"][tool] = state
        report["authenticated_tools"] = [
            tool for tool in config["preferred"] if report["tools"].get(tool, {}).get("authenticated") is True
        ]
        return report

    async def preflight(self, runtime_cli: dict[str, Any] | None = None) -> dict[str, Any]:
        """Run full preflight: detect -> install -> authenticate."""
        initial = await self.detect(runtime_cli)
        install = await self.ensure_installed(runtime_cli)
        auth = await self.ensure_authenticated(runtime_cli)
        final = await self.detect(runtime_cli)
        return {
            "initial": initial,
            "install": install,
            "auth": auth,
            "final": final,
            "ready": len(final.get("authenticated_tools", [])) > 0,
        }

    async def run_diagnostic_prompt(
        self,
        *,
        prompt: str,
        runtime_cli: dict[str, Any] | None = None,
        preferred_tool: ToolName | None = None,
        mode_override: DiagnosticMode | None = None,
    ) -> dict[str, Any]:
        """Run a diagnostic prompt with an authenticated tool in safe mode."""
        config = _merge_runtime_cli_config(runtime_cli)
        status = await self.detect(config)

        order: list[str] = []
        if preferred_tool is not None:
            order.append(preferred_tool)
        order.extend([tool for tool in config["preferred"] if tool not in order])

        selected: ToolName | None = None
        for tool in order:
            tool_state = status["tools"].get(tool, {})
            if tool_state.get("detected") and tool_state.get("authenticated"):
                selected = tool  # type: ignore[assignment]
                break
        if selected is None:
            for tool in order:
                tool_state = status["tools"].get(tool, {})
                if tool_state.get("detected"):
                    selected = tool  # type: ignore[assignment]
                    break

        if selected is None:
            return {
                "success": False,
                "error": "no_cli_available",
                "status": status,
            }

        tool_cfg = config[selected]
        mode = (mode_override or tool_cfg.get("diagnostic_mode") or "read_only").strip().lower()
        if mode not in {"read_only", "plan"}:
            mode = "read_only"

        command = self._build_diagnostic_command(
            tool=selected,
            tool_cfg=tool_cfg,
            prompt=prompt,
            mode=mode,  # type: ignore[arg-type]
        )
        if not self._is_command_safe(command):
            return {
                "success": False,
                "tool": selected,
                "command": " ".join(shlex.quote(x) for x in command),
                "error": "blocked_by_safety_policy",
            }

        executed = await self._run_command(command, timeout_seconds=max(self._timeout_seconds, 120.0))
        return {
            "success": executed.ok,
            "tool": selected,
            "mode": mode,
            "command": " ".join(shlex.quote(x) for x in command),
            "return_code": executed.return_code,
            "stdout": executed.stdout,
            "stderr": executed.stderr,
            "error": executed.error,
            "timed_out": executed.timed_out,
        }

    async def run_repair_prompt(
        self,
        *,
        prompt: str,
        runtime_cli: dict[str, Any] | None = None,
        preferred_tool: ToolName | None = None,
        mode_override: RepairMode | None = None,
    ) -> dict[str, Any]:
        """Run a write-enabled repair prompt with an available CLI tool."""
        config = _merge_runtime_cli_config(runtime_cli)
        status = await self.detect(config)

        order: list[str] = []
        if preferred_tool is not None:
            order.append(preferred_tool)
        order.extend([tool for tool in config["preferred"] if tool not in order])

        selected: ToolName | None = None
        for tool in order:
            tool_state = status["tools"].get(tool, {})
            if tool_state.get("detected") and tool_state.get("authenticated"):
                selected = tool  # type: ignore[assignment]
                break
        if selected is None:
            for tool in order:
                tool_state = status["tools"].get(tool, {})
                if tool_state.get("detected"):
                    selected = tool  # type: ignore[assignment]
                    break

        if selected is None:
            return {
                "success": False,
                "error": "no_cli_available",
                "status": status,
            }

        tool_cfg = config[selected]
        mode = (mode_override or tool_cfg.get("repair_mode") or "workspace_write").strip().lower()
        if mode not in {"workspace_write", "accept_edits"}:
            mode = "workspace_write"

        command = self._build_repair_command(
            tool=selected,
            tool_cfg=tool_cfg,
            prompt=prompt,
            mode=mode,  # type: ignore[arg-type]
        )
        if not self._is_command_safe(command):
            return {
                "success": False,
                "tool": selected,
                "command": " ".join(shlex.quote(x) for x in command),
                "error": "blocked_by_safety_policy",
            }

        executed = await self._run_command(command, timeout_seconds=max(self._timeout_seconds, 240.0))
        return {
            "success": executed.ok,
            "tool": selected,
            "mode": mode,
            "command": " ".join(shlex.quote(x) for x in command),
            "return_code": executed.return_code,
            "stdout": executed.stdout,
            "stderr": executed.stderr,
            "error": executed.error,
            "timed_out": executed.timed_out,
        }

    async def _detect_tool(self, tool: ToolName, tool_cfg: dict[str, Any]) -> dict[str, Any]:
        executable = str(tool_cfg.get("executable") or tool).strip() or tool
        path = shutil.which(executable)
        state: dict[str, Any] = {
            "tool": tool,
            "executable": executable,
            "detected": path is not None,
            "path": path,
            "version": None,
            "authenticated": False,
            "auth_method": None,
        }
        if path is None:
            return state

        version_cmd = [executable, "--version"]
        version_result = await self._run_command(version_cmd, timeout_seconds=10.0)
        version_text = (version_result.stdout or version_result.stderr).strip()
        state["version"] = version_text.splitlines()[0] if version_text else None

        auth_state = await self._check_auth_state(tool, tool_cfg, executable=executable)
        state["authenticated"] = auth_state["authenticated"]
        state["auth_method"] = auth_state.get("auth_method")
        state["auth_details"] = auth_state.get("details", {})
        return state

    async def _ensure_tool_auth(self, tool: ToolName, tool_cfg: dict[str, Any]) -> dict[str, Any]:
        executable = str(tool_cfg.get("executable") or tool).strip() or tool
        path = shutil.which(executable)
        base: dict[str, Any] = {
            "tool": tool,
            "detected": path is not None,
            "authenticated": False,
            "auth_method": None,
            "interactive_required": False,
            "interactive_command": None,
            "attempts": [],
        }
        if path is None:
            return base

        status_now = await self._check_auth_state(tool, tool_cfg, executable=executable)
        base["attempts"].append({"step": "status", **status_now})
        if status_now.get("authenticated") is True:
            base["authenticated"] = True
            base["auth_method"] = status_now.get("auth_method")
            return base

        auth_cfg = tool_cfg.get("auth", {})
        api_key_env_name = auth_cfg.get("api_key_env")
        api_key = os.getenv(api_key_env_name) if isinstance(api_key_env_name, str) and api_key_env_name else None

        api_key_login = auth_cfg.get("api_key_login")
        if api_key and isinstance(api_key_login, list) and api_key_login:
            if self._is_command_safe(api_key_login):
                login_res = await self._run_command(
                    [str(part) for part in api_key_login],
                    input_text=api_key,
                    timeout_seconds=20.0,
                )
                base["attempts"].append({"step": "api_key_login", **login_res.to_dict()})
                if login_res.ok:
                    status_after_login = await self._check_auth_state(tool, tool_cfg, executable=executable)
                    base["attempts"].append({"step": "status_after_api_key_login", **status_after_login})
                    if status_after_login.get("authenticated") is True:
                        base["authenticated"] = True
                        base["auth_method"] = "api_key_login"
                        return base
            else:
                base["attempts"].append(
                    {
                        "step": "api_key_login",
                        "ok": False,
                        "error": "blocked_by_safety_policy",
                    }
                )
        elif api_key:
            base["authenticated"] = True
            base["auth_method"] = "api_key_env"
            base["attempts"].append(
                {
                    "step": "api_key_env_detected",
                    "ok": True,
                    "api_key_env": api_key_env_name,
                }
            )
            return base

        interactive = auth_cfg.get("interactive_login")
        if isinstance(interactive, list) and interactive:
            base["interactive_required"] = True
            base["interactive_command"] = " ".join(shlex.quote(str(x)) for x in interactive)
        return base

    async def _check_auth_state(
        self,
        tool: ToolName,
        tool_cfg: dict[str, Any],
        *,
        executable: str,
    ) -> dict[str, Any]:
        auth_cfg = tool_cfg.get("auth", {})
        status_cmd = auth_cfg.get("status")
        if not isinstance(status_cmd, list) or not status_cmd:
            api_key_env = auth_cfg.get("api_key_env")
            has_key = bool(api_key_env and os.getenv(str(api_key_env)))
            return {
                "authenticated": has_key,
                "auth_method": "api_key_env" if has_key else None,
                "details": {"status_command": None},
            }

        status_command = [str(x) for x in status_cmd]
        if status_command[0] != executable:
            status_command[0] = executable
        if not self._is_command_safe(status_command):
            return {
                "authenticated": False,
                "auth_method": None,
                "details": {"error": "status_command_blocked_by_safety_policy"},
            }

        result = await self._run_command(status_command, timeout_seconds=12.0)
        parsed = self._parse_auth_status_output(tool, result)
        return {
            "authenticated": parsed["authenticated"],
            "auth_method": parsed.get("auth_method"),
            "details": {
                "command": " ".join(shlex.quote(x) for x in status_command),
                "return_code": result.return_code,
                "stdout": result.stdout,
                "stderr": result.stderr,
            },
        }

    @staticmethod
    def _parse_auth_status_output(tool: ToolName, result: _CommandResult) -> dict[str, Any]:
        stdout = result.stdout.strip()
        stderr = result.stderr.strip()
        merged_lower = f"{stdout}\n{stderr}".lower()

        if tool == "codex":
            authenticated = result.ok and ("logged in" in merged_lower)
            auth_method = "chat_auth" if authenticated else None
            return {"authenticated": authenticated, "auth_method": auth_method}

        if tool == "claude":
            parsed_json: dict[str, Any] | None = None
            if stdout:
                try:
                    maybe = json.loads(stdout)
                    if isinstance(maybe, dict):
                        parsed_json = maybe
                except json.JSONDecodeError:
                    parsed_json = None
            authenticated = bool(parsed_json and parsed_json.get("loggedIn") is True)
            auth_method = "claude_ai_auth" if authenticated else None
            return {"authenticated": authenticated, "auth_method": auth_method}

        return {"authenticated": False, "auth_method": None}

    def _build_diagnostic_command(
        self,
        *,
        tool: ToolName,
        tool_cfg: dict[str, Any],
        prompt: str,
        mode: DiagnosticMode,
    ) -> list[str]:
        default_command = tool_cfg.get("diagnostic_command")
        command: list[str]
        if isinstance(default_command, list) and default_command:
            command = [str(x) for x in default_command]
        elif tool == "codex":
            command = ["codex", "exec", "--skip-git-repo-check", "--sandbox", "read-only"]
        else:
            command = ["claude", "-p", "--permission-mode", "plan"]

        if tool == "codex":
            if "--sandbox" in command:
                index = command.index("--sandbox")
                if index + 1 < len(command):
                    command[index + 1] = "read-only"
            elif mode in {"read_only", "plan"}:
                command.extend(["--sandbox", "read-only"])
        elif "--permission-mode" in command:
            index = command.index("--permission-mode")
            if index + 1 < len(command):
                command[index + 1] = "plan"
        else:
            command.extend(["--permission-mode", "plan"])

        command.append(prompt)
        return command

    def _build_repair_command(
        self,
        *,
        tool: ToolName,
        tool_cfg: dict[str, Any],
        prompt: str,
        mode: RepairMode,
    ) -> list[str]:
        default_command = tool_cfg.get("repair_command")
        command: list[str]
        if isinstance(default_command, list) and default_command:
            command = [str(x) for x in default_command]
        elif tool == "codex":
            command = ["codex", "exec", "--skip-git-repo-check", "--sandbox", "workspace-write"]
        else:
            command = ["claude", "-p", "--permission-mode", "acceptEdits"]

        if tool == "codex":
            sandbox_value = "workspace-write" if mode == "workspace_write" else "read-only"
            if "--sandbox" in command:
                index = command.index("--sandbox")
                if index + 1 < len(command):
                    command[index + 1] = sandbox_value
            else:
                command.extend(["--sandbox", sandbox_value])
        else:
            permission_mode = "acceptEdits" if mode == "accept_edits" else "plan"
            if "--permission-mode" in command:
                index = command.index("--permission-mode")
                if index + 1 < len(command):
                    command[index + 1] = permission_mode
            else:
                command.extend(["--permission-mode", permission_mode])

        command.append(prompt)
        return command

    @staticmethod
    def _is_command_safe(command: list[str]) -> bool:
        lowered = {part.strip().lower() for part in command}
        return not any(token in lowered for token in _DANGEROUS_TOKENS)

    async def _run_command(
        self,
        command: list[str],
        *,
        input_text: str | None = None,
        timeout_seconds: float | None = None,
    ) -> _CommandResult:
        timeout = self._timeout_seconds if timeout_seconds is None else timeout_seconds
        stdin = asyncio.subprocess.PIPE if input_text is not None else None
        try:
            proc = await asyncio.create_subprocess_exec(
                *command,
                cwd=str(self._cwd),
                stdin=stdin,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
        except FileNotFoundError:
            return _CommandResult(
                command=command,
                return_code=None,
                stdout="",
                stderr="",
                error="command_not_found",
            )
        except Exception as exc:  # pragma: no cover - defensive fallback
            return _CommandResult(
                command=command,
                return_code=None,
                stdout="",
                stderr="",
                error=str(exc),
            )

        input_bytes = None if input_text is None else input_text.encode("utf-8")
        try:
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(input=input_bytes),
                timeout=timeout,
            )
            return _CommandResult(
                command=command,
                return_code=proc.returncode,
                stdout=stdout_bytes.decode("utf-8", errors="ignore").strip(),
                stderr=stderr_bytes.decode("utf-8", errors="ignore").strip(),
            )
        except TimeoutError:
            proc.kill()
            stdout_bytes, stderr_bytes = await proc.communicate()
            return _CommandResult(
                command=command,
                return_code=None,
                stdout=stdout_bytes.decode("utf-8", errors="ignore").strip(),
                stderr=stderr_bytes.decode("utf-8", errors="ignore").strip(),
                timed_out=True,
                error=f"timeout ({timeout:.1f}s)",
            )
