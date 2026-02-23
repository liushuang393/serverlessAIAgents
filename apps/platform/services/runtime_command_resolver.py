"""Resolve app runtime commands with README-first priority.

Priority policy:
1) README default commands
2) runtime.commands from app_config.json
3) lifecycle fallback behavior (compose defaults etc.)
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

from apps.platform.schemas.app_config_schemas import RuntimeCommandsConfig


CommandAction = Literal["backend_dev", "frontend_dev", "publish", "start", "stop"]
_COMMAND_ACTIONS: tuple[CommandAction, ...] = ("backend_dev", "frontend_dev", "publish", "start", "stop")
_CODE_FENCE_PATTERN = re.compile(r"```(?:bash|sh|zsh|shell)?\s*(?P<body>.*?)```", re.DOTALL | re.IGNORECASE)


@dataclass(slots=True)
class ResolvedRuntimeCommands:
    """Resolved command set and source labels per action."""

    backend_dev: str | None
    frontend_dev: str | None
    publish: str | None
    start: str | None
    stop: str | None
    source: dict[CommandAction, str]

    def get(self, action: CommandAction) -> str | None:
        """Return command by action."""
        return {
            "backend_dev": self.backend_dev,
            "frontend_dev": self.frontend_dev,
            "publish": self.publish,
            "start": self.start,
            "stop": self.stop,
        }[action]


class RuntimeCommandResolver:
    """Resolve runtime commands with README-first strategy."""

    def resolve(
        self,
        *,
        app_name: str,
        app_dir: Path,
        runtime_commands: RuntimeCommandsConfig,
    ) -> ResolvedRuntimeCommands:
        """Resolve actions from README -> runtime.commands."""
        readme_commands = self._extract_from_readme(app_name=app_name, app_dir=app_dir)
        runtime_map: dict[CommandAction, str | None] = {
            "backend_dev": self._normalize(runtime_commands.backend_dev),
            "frontend_dev": self._normalize(runtime_commands.frontend_dev),
            "publish": self._normalize(runtime_commands.publish),
            "start": self._normalize(runtime_commands.start),
            "stop": self._normalize(runtime_commands.stop),
        }

        resolved_values: dict[CommandAction, str | None] = {}
        sources: dict[CommandAction, str] = {}
        for action in _COMMAND_ACTIONS:
            readme_value = readme_commands.get(action)
            if readme_value is not None:
                resolved_values[action] = readme_value
                sources[action] = "readme"
                continue
            runtime_value = runtime_map[action]
            if runtime_value is not None:
                resolved_values[action] = runtime_value
                sources[action] = "runtime.commands"
            else:
                resolved_values[action] = None
                sources[action] = "fallback"

        return ResolvedRuntimeCommands(
            backend_dev=resolved_values["backend_dev"],
            frontend_dev=resolved_values["frontend_dev"],
            publish=resolved_values["publish"],
            start=resolved_values["start"],
            stop=resolved_values["stop"],
            source=sources,
        )

    def _extract_from_readme(self, *, app_name: str, app_dir: Path) -> dict[CommandAction, str]:
        readme_path = self._pick_readme_path(app_dir)
        if readme_path is None:
            return {}

        text = readme_path.read_text(encoding="utf-8", errors="ignore")
        blocks = _CODE_FENCE_PATTERN.findall(text)
        candidates: dict[CommandAction, list[str]] = {action: [] for action in _COMMAND_ACTIONS}

        for block in blocks:
            for raw_line in block.splitlines():
                normalized = self._normalize_shell_line(raw_line)
                if normalized is None:
                    continue
                for action in _COMMAND_ACTIONS:
                    if self._matches_action(action=action, command=normalized, app_name=app_name):
                        candidates[action].append(normalized)

        resolved: dict[CommandAction, str] = {}
        for action, lines in candidates.items():
            if lines:
                resolved[action] = lines[0]
        return resolved

    @staticmethod
    def _pick_readme_path(app_dir: Path) -> Path | None:
        for name in ("README.md", "readme.md", "Readme.md"):
            path = app_dir / name
            if path.is_file():
                return path
        return None

    @staticmethod
    def _normalize_shell_line(raw: str) -> str | None:
        line = raw.strip()
        if not line or line.startswith("#"):
            return None
        if line.startswith("$ "):
            line = line[2:].strip()
        return line if line else None

    @staticmethod
    def _normalize(value: str | None) -> str | None:
        if value is None:
            return None
        trimmed = value.strip()
        return trimmed or None

    @staticmethod
    def _matches_action(*, action: CommandAction, command: str, app_name: str) -> bool:
        lower = command.lower()
        app_token = app_name.lower()
        app_module_hint = f"apps.{app_token}"
        app_path_hint = f"apps/{app_token}"

        if action == "backend_dev":
            return ("python -m " in lower and app_module_hint in lower) or ("uvicorn " in lower and app_module_hint in lower)
        if action == "frontend_dev":
            return "npm run dev" in lower and (app_path_hint in lower or "admin_ui" in lower or "frontend" in lower)
        if action == "publish":
            return ("python -m apps.platform.main publish" in lower and app_path_hint in lower) or (
                "docker compose up -d --build" in lower
            )
        if action == "start":
            return (
                ("python -m " in lower and app_module_hint in lower)
                or "docker compose up -d" in lower
                or "python " + app_path_hint + "/scripts/dev.py" in lower
            )
        return "docker compose down" in lower

