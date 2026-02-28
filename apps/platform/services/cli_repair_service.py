"""CLI による自動修復（ファイル編集 + 再試行）サービス."""

from __future__ import annotations

import shlex
import subprocess
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.tools.cli.runtime_manager import CLIRuntimeManager, ToolName


if TYPE_CHECKING:
    from apps.platform.schemas.app_config_schemas import AppConfig


class CLIRepairService:
    """失敗アクションに対して CLI 修復を実行する."""

    def __init__(
        self,
        runtime_manager: CLIRuntimeManager | None = None,
        *,
        repo_root: Path | None = None,
    ) -> None:
        self._runtime_manager = runtime_manager or CLIRuntimeManager()
        self._repo_root = (repo_root or Path.cwd()).resolve()

    async def attempt_action_repair(
        self,
        *,
        app_config: AppConfig,
        action: str,
        config_path: Path | None,
        tool: ToolName,
        attempt: int,
        command: list[str],
        cwd: str,
        error: str | None,
        stdout: str,
        stderr: str,
    ) -> dict[str, Any]:
        """1 回分の修復試行を実行して履歴情報を返す."""
        before_dirty = self._list_dirty_files()
        prompt = self._build_prompt(
            app_name=app_config.name,
            action=action,
            config_path=config_path,
            command=command,
            cwd=cwd,
            error=error,
            stdout=stdout,
            stderr=stderr,
        )
        runtime_cli = app_config.runtime.cli.model_dump()
        result = await self._runtime_manager.run_repair_prompt(
            prompt=prompt,
            runtime_cli=runtime_cli,
            preferred_tool=tool,
        )

        after_dirty = self._list_dirty_files()
        touched_files = sorted(after_dirty - before_dirty)
        allowed_roots = self._allowed_scope_roots(app_name=app_config.name, config_path=config_path)
        scope_ok, outside_scope = self._validate_scope(touched_files, allowed_roots)
        summary = self._first_line(result.get("stdout"), result.get("stderr"), result.get("error"))

        return {
            "tool": tool,
            "attempt": attempt,
            "success": bool(result.get("success")) and scope_ok,
            "command": result.get("command"),
            "timed_out": bool(result.get("timed_out")),
            "error": result.get("error"),
            "summary": summary,
            "files_touched": touched_files,
            "allowed_roots": sorted(allowed_roots),
            "scope_ok": scope_ok,
            "outside_scope_files": outside_scope,
            "stdout_tail": str(result.get("stdout") or "")[-3000:],
            "stderr_tail": str(result.get("stderr") or "")[-3000:],
        }

    def _allowed_scope_roots(self, *, app_name: str, config_path: Path | None) -> set[str]:
        app_root = config_path.parent if config_path is not None else self._repo_root / "apps" / app_name
        app_relative = self._to_repo_relative(app_root)
        roots = {
            "apps/platform",
            "agentflow/tools/cli",
        }
        if app_relative is not None:
            roots.add(app_relative)
        return roots

    def _build_prompt(
        self,
        *,
        app_name: str,
        action: str,
        config_path: Path | None,
        command: list[str],
        cwd: str,
        error: str | None,
        stdout: str,
        stderr: str,
    ) -> str:
        config_text = str(config_path) if config_path is not None else "(missing)"
        command_text = " ".join(shlex.quote(part) for part in command)
        allowed_roots = ", ".join(sorted(self._allowed_scope_roots(app_name=app_name, config_path=config_path)))
        return (
            "Fix the failing app lifecycle action by editing files directly in this repository.\n"
            "You must only modify files under the allowed scope and avoid destructive git operations.\n"
            "After edits, stop and return a short summary of what changed.\n\n"
            f"App: {app_name}\n"
            f"Action: {action}\n"
            f"Config path: {config_text}\n"
            f"Working directory: {cwd}\n"
            f"Failing command: {command_text}\n"
            f"Allowed scope roots: {allowed_roots}\n\n"
            f"Error: {error or '(none)'}\n\n"
            "STDERR:\n"
            f"{stderr[:4000]}\n\n"
            "STDOUT:\n"
            f"{stdout[:4000]}\n"
        )

    def _list_dirty_files(self) -> set[str]:
        try:
            proc = subprocess.run(
                ["git", "status", "--porcelain"],
                cwd=str(self._repo_root),
                capture_output=True,
                text=True,
                check=False,
                timeout=3,
            )
        except Exception:
            return set()

        if proc.returncode != 0:
            return set()

        files: set[str] = set()
        for line in proc.stdout.splitlines():
            if len(line) < 4:
                continue
            raw_path = line[3:].strip()
            if " -> " in raw_path:
                raw_path = raw_path.split(" -> ", 1)[1].strip()
            normalized = raw_path.replace("\\", "/")
            if normalized:
                files.add(normalized)
        return files

    @staticmethod
    def _first_line(*candidates: Any) -> str:
        for candidate in candidates:
            text = str(candidate or "").strip()
            if not text:
                continue
            line = text.splitlines()[0].strip()
            if line:
                return line[:240]
        return "repair attempt executed"

    def _validate_scope(
        self,
        touched_files: list[str],
        allowed_roots: set[str],
    ) -> tuple[bool, list[str]]:
        outside: list[str] = []
        for path in touched_files:
            if not self._is_within_allowed_roots(path, allowed_roots):
                outside.append(path)
        return len(outside) == 0, outside

    @staticmethod
    def _is_within_allowed_roots(path: str, roots: set[str]) -> bool:
        normalized = path.replace("\\", "/").lstrip("./")
        for root in roots:
            normalized_root = root.replace("\\", "/").strip("/")
            if normalized == normalized_root or normalized.startswith(f"{normalized_root}/"):
                return True
        return False

    def _to_repo_relative(self, path: Path) -> str | None:
        try:
            return path.resolve().relative_to(self._repo_root).as_posix()
        except ValueError:
            return None

