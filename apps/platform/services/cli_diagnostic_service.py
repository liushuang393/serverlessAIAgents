"""CLI-based startup diagnostic service for Platform app lifecycle actions."""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

from agentflow.tools.cli.runtime_manager import CLIRuntimeManager


if TYPE_CHECKING:
    from pathlib import Path

    from apps.platform.schemas.app_config_schemas import AppConfig


class CLIDiagnosticService:
    """Build context-rich diagnostics and run external CLI investigation."""

    def __init__(self, runtime_manager: CLIRuntimeManager | None = None) -> None:
        self._runtime_manager = runtime_manager or CLIRuntimeManager()

    async def diagnose_action_failure(
        self,
        *,
        app_config: AppConfig,
        action: str,
        config_path: Path | None,
        command: list[str],
        cwd: str,
        error: str | None,
        stdout: str,
        stderr: str,
        preflight: dict[str, Any] | None,
        command_source: str,
        repair_trace: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Diagnose startup failure and return response-friendly structure."""
        prompt = self._build_prompt(
            app_name=app_config.name,
            action=action,
            command=command,
            cwd=cwd,
            error=error,
            stdout=stdout,
            stderr=stderr,
            config_path=config_path,
            preflight=preflight,
        )
        runtime_cli = app_config.runtime.cli.model_dump()
        diagnostic = await self._runtime_manager.run_diagnostic_prompt(
            prompt=prompt,
            runtime_cli=runtime_cli,
        )

        merged_output = "\n".join(
            part for part in (diagnostic.get("stdout", ""), diagnostic.get("stderr", "")) if isinstance(part, str) and part
        )
        summary = self._build_summary(error=error, stderr=stderr, cli_output=merged_output)
        recommendations = self._build_recommendations(error=error, stderr=stderr, cli_output=merged_output)

        diagnostic: dict[str, Any] = {
            "tool": diagnostic.get("tool"),
            "setup": self._setup_snapshot(preflight),
            "summary": summary,
            "recommendations": recommendations,
            "raw_output": merged_output[-6000:] if merged_output else "",
            "command_source": command_source,
            "diagnostic_success": bool(diagnostic.get("success")),
            "diagnostic_command": diagnostic.get("command"),
            "diagnostic_error": diagnostic.get("error"),
        }
        if repair_trace is not None:
            diagnostic["retry_trace"] = repair_trace
        return diagnostic

    @staticmethod
    def attach_retry_trace(
        diagnostic: dict[str, Any] | None,
        repair_trace: dict[str, Any],
    ) -> dict[str, Any]:
        """既存診断情報に retry trace を付与する."""
        merged = dict(diagnostic or {})
        merged["retry_trace"] = repair_trace
        return merged

    @staticmethod
    def _build_prompt(
        *,
        app_name: str,
        action: str,
        command: list[str],
        cwd: str,
        error: str | None,
        stdout: str,
        stderr: str,
        config_path: Path | None,
        preflight: dict[str, Any] | None,
    ) -> str:
        command_text = " ".join(command)
        config_text = str(config_path) if config_path is not None else "(missing)"
        preflight_hint = "unknown"
        if isinstance(preflight, dict):
            ready = preflight.get("ready")
            preflight_hint = "ready" if ready is True else "not_ready"

        return (
            "You are diagnosing a local startup failure for an AgentFlow app.\n"
            "Return concise root cause bullets and actionable fix commands.\n"
            "Do not propose destructive commands.\n\n"
            f"App: {app_name}\n"
            f"Action: {action}\n"
            f"Config path: {config_text}\n"
            f"Working directory: {cwd}\n"
            f"Preflight: {preflight_hint}\n"
            f"Executed command: {command_text}\n\n"
            f"Error: {error or '(none)'}\n\n"
            "STDERR:\n"
            f"{stderr[:5000]}\n\n"
            "STDOUT:\n"
            f"{stdout[:5000]}\n"
        )

    @staticmethod
    def _setup_snapshot(preflight: dict[str, Any] | None) -> dict[str, Any]:
        if not isinstance(preflight, dict):
            return {"ready": False, "available_tools": [], "authenticated_tools": []}

        final = preflight.get("final")
        if not isinstance(final, dict):
            return {"ready": bool(preflight.get("ready")), "available_tools": [], "authenticated_tools": []}

        return {
            "ready": bool(preflight.get("ready")),
            "available_tools": final.get("available_tools", []),
            "authenticated_tools": final.get("authenticated_tools", []),
        }

    @staticmethod
    def _build_summary(*, error: str | None, stderr: str, cli_output: str) -> str:
        for source in (error or "", stderr, cli_output):
            text = source.strip()
            if not text:
                continue
            first_line = text.splitlines()[0].strip()
            if first_line:
                return first_line[:240]
        return "startup failed; see diagnostic output for details"

    def _build_recommendations(self, *, error: str | None, stderr: str, cli_output: str) -> list[str]:
        merged = "\n".join([x for x in (error or "", stderr, cli_output) if x]).lower()
        recs: list[str] = []

        if "docker" in merged and ("not found" in merged or "command_not_found" in merged):
            recs.append("Install Docker and ensure `docker compose` is available in PATH.")
        if "npm" in merged and "not found" in merged:
            recs.append("Install Node.js/npm, then re-run frontend startup commands.")
        if "module not found" in merged or "no module named" in merged:
            recs.append('Activate the project Python environment and install dependencies (e.g. `pip install -e ".[dev,apps]"`).')
        if "eaddrinuse" in merged or "address already in use" in merged:
            recs.append("Port conflict detected. Stop conflicting process or adjust `ports.*` in app_config.")
        if "permission denied" in merged:
            recs.append("Check file and executable permissions for the startup command.")

        # Try to harvest bullet-style lines from CLI output
        for line in cli_output.splitlines():
            stripped = line.strip()
            if re.match(r"^[-*]\s+\S+", stripped):
                rec = stripped[2:].strip()
                if rec and rec not in recs:
                    recs.append(rec)
            if len(recs) >= 6:
                break

        if not recs:
            recs.append("Review app README startup section and verify backend/frontend/db commands manually.")
        return recs[:8]
