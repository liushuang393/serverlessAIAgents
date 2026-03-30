"""CLI-Native harness import/build/runtime service."""

from __future__ import annotations

import json
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from contracts.skill import CLIHarnessManifest


_DEFAULT_CLI_ANYTHING_REPO_URL = "https://github.com/HKUDS/CLI-Anything.git"
_DEFAULT_CLI_ANYTHING_REF = "790a186a4cc3ab9cc5f4807545c13dedfa020d6d"
_FRONTMATTER_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)
_SETUP_NAME_RE = re.compile(r"""name\s*=\s*["'](?P<name>[^"']+)["']""")
_CONSOLE_SCRIPT_BLOCK_RE = re.compile(r"""["']console_scripts["']\s*:\s*\[(?P<body>.*?)\]""", re.DOTALL)
_CONSOLE_SCRIPT_RE = re.compile(r"""["'](?P<command>[^"'\s=]+)\s*=\s*[^"']+["']""")
_HELP_SECTION_RE = re.compile(r"^(?P<header>[A-Z][A-Za-z ]+):\s*$")
_HELP_COMMAND_RE = re.compile(r"^\s{2,}(?P<name>[a-z0-9][a-z0-9\-]*)\s{2,}.*$")
_TABLE_COMMAND_RE = re.compile(r"^\|\s*`(?P<command>[^`]+)`\s*\|")


@dataclass(frozen=True)
class CommandExecutionResult:
    """コマンド実行結果."""

    return_code: int
    stdout: str
    stderr: str


class CLINativeService:
    """CLI-Native harness を import/build/run するサービス."""

    def __init__(
        self,
        *,
        workspace_root: Path | None = None,
        env: dict[str, str] | None = None,
    ) -> None:
        self._workspace_root = (workspace_root or Path.cwd()).resolve()
        self._env = env or {}
        self._project_config_dir = self._workspace_root / ".bizcore"
        self._skills_dir = self._project_config_dir / "skills"
        self._managed_root = self._project_config_dir / "cli_native"
        self._harnesses_dir = self._managed_root / "harnesses"
        self._manifests_dir = self._managed_root / "manifests"
        self._cache_dir = self._managed_root / "cache"
        self._repo_url = self._env.get("CLI_ANYTHING_REPO_URL", _DEFAULT_CLI_ANYTHING_REPO_URL)
        self._repo_ref = self._env.get("CLI_ANYTHING_REF", _DEFAULT_CLI_ANYTHING_REF)
        self._runtime_cli = self._env.get("CLI_ANYTHING_RUNTIME", "codex").strip().lower() or "codex"
        self._ensure_dirs()

    @property
    def skills_dir(self) -> Path:
        """Project-local skills ディレクトリ."""
        return self._skills_dir

    def list_manifests(self) -> list[CLIHarnessManifest]:
        """登録済み manifest 一覧を返す."""
        manifests: list[CLIHarnessManifest] = []
        if not self._manifests_dir.exists():
            return manifests
        for manifest_path in sorted(self._manifests_dir.glob("*.json")):
            try:
                payload = json.loads(manifest_path.read_text(encoding="utf-8"))
                manifests.append(CLIHarnessManifest.model_validate(payload))
            except (OSError, json.JSONDecodeError, ValueError):
                continue
        return manifests

    def get_manifest(self, harness_id: str) -> CLIHarnessManifest | None:
        """単一 manifest を取得する."""
        manifest_path = self._manifest_path(harness_id)
        if not manifest_path.is_file():
            return None
        payload = json.loads(manifest_path.read_text(encoding="utf-8"))
        return CLIHarnessManifest.model_validate(payload)

    def import_harness(
        self,
        *,
        harness_path: Path,
        harness_id: str | None = None,
        software_name: str | None = None,
        force: bool = True,
    ) -> CLIHarnessManifest:
        """生成済み agent-harness を管理対象へ import する."""
        source_dir = harness_path.resolve()
        self._validate_harness_dir(source_dir)

        detected_skill_md = self._discover_skill_md(source_dir)
        skill_meta = self._read_skill_frontmatter(detected_skill_md)
        derived_software_name = software_name or self._derive_software_name(source_dir, skill_meta.get("name"))
        normalized_harness_id = harness_id or self._default_harness_id(derived_software_name)
        managed_harness_dir = self._harnesses_dir / normalized_harness_id
        shim_skill_dir = self._skills_dir / normalized_harness_id

        if managed_harness_dir.exists():
            if not force:
                msg = f"harness already exists: {managed_harness_dir}"
                raise FileExistsError(msg)
            shutil.rmtree(managed_harness_dir)
        if shim_skill_dir.exists():
            shutil.rmtree(shim_skill_dir)

        shutil.copytree(source_dir, managed_harness_dir)

        managed_skill_md = managed_harness_dir / detected_skill_md.relative_to(source_dir)
        cli_command = self._discover_cli_command(managed_harness_dir)
        try:
            self._install_harness(managed_harness_dir)
            install_state = "installed"
        except RuntimeError:
            install_state = "failed"

        command_groups, command_options = self._inspect_command_tree(cli_command, managed_skill_md)
        shim_skill_path = self._write_shim_skill(
            harness_id=normalized_harness_id,
            software_name=derived_software_name,
            manifest_skill_md=managed_skill_md,
            cli_command=cli_command,
            command_groups=command_groups,
        )

        description = str(skill_meta.get("description", "")).strip()
        manifest = CLIHarnessManifest(
            harness_id=normalized_harness_id,
            software_name=derived_software_name,
            source_ref=f"import:{source_dir}",
            package_dir=str(managed_harness_dir),
            cli_command=cli_command,
            skill_md_path=str(managed_skill_md),
            command_groups=command_groups,
            install_state=install_state,
            risk_profile="high",
            description=description,
            shim_skill_path=str(shim_skill_path),
            command_options=command_options,
        )
        self._persist_manifest(manifest)
        return manifest

    def plan_build(
        self,
        *,
        software_name: str,
        source_path: Path,
        runtime_cli: str | None = None,
        dry_run: bool = True,
    ) -> dict[str, Any]:
        """CLI-Anything build ジョブを計画または実行する."""
        repo_dir = self._ensure_cli_anything_repo()
        target_runtime_cli = (runtime_cli or self._runtime_cli).strip().lower() or "codex"
        build_dir = self._managed_root / "builds" / self._slugify(software_name)
        build_dir.mkdir(parents=True, exist_ok=True)

        prompt = (
            "Use the pinned CLI-Anything checkout to generate an agent-native CLI harness. "
            f"software={software_name}; source={source_path.resolve()}; output_dir={build_dir.resolve()}"
        )
        if target_runtime_cli == "claude":
            planned_command = ["claude", "-p", "--permission-mode", "acceptEdits", prompt]
        else:
            planned_command = [
                "codex",
                "exec",
                "--skip-git-repo-check",
                "--sandbox",
                "workspace-write",
                prompt,
            ]

        result: dict[str, Any] = {
            "success": True,
            "software_name": software_name,
            "runtime_cli": target_runtime_cli,
            "repo_url": self._repo_url,
            "repo_ref": self._repo_ref,
            "repo_dir": str(repo_dir),
            "source_path": str(source_path.resolve()),
            "output_dir": str(build_dir.resolve()),
            "planned_command": planned_command,
            "dry_run": dry_run,
        }
        if dry_run:
            return result

        executed = self._run_command(planned_command, cwd=source_path.resolve())
        result["stdout"] = executed.stdout
        result["stderr"] = executed.stderr
        result["return_code"] = executed.return_code
        result["success"] = executed.return_code == 0
        return result

    def execute_harness(
        self,
        *,
        harness_id: str,
        subcommand: str,
        args: list[str] | None = None,
        project_path: str | None = None,
        dry_run: bool = False,
    ) -> dict[str, Any]:
        """登録済み harness を安全に実行する."""
        manifest = self.get_manifest(harness_id)
        if manifest is None:
            msg = f"unknown harness_id: {harness_id}"
            raise FileNotFoundError(msg)

        command_parts = [part for part in subcommand.strip().split() if part]
        if len(command_parts) < 2:
            msg = "subcommand must be in '<group> <command>' format"
            raise ValueError(msg)
        group_name, command_name = command_parts[0], command_parts[1]
        self._validate_subcommand(manifest, group_name, command_name)
        arg_list = args or []
        self._validate_options(manifest, command_parts, arg_list)

        command: list[str] = [
            "conda",
            "run",
            "-n",
            "agentflow",
            manifest.cli_command,
            "--json",
        ]
        if project_path:
            command.extend(["--project", project_path])
        command.extend(command_parts)
        command.extend(arg_list)

        if dry_run:
            return {
                "validated": True,
                "harness_id": harness_id,
                "command": command,
                "install_state": manifest.install_state,
            }

        executed = self._run_command(command, cwd=self._workspace_root)
        payload = self._decode_output(executed.stdout)
        return {
            "harness_id": harness_id,
            "command": command,
            "return_code": executed.return_code,
            "stdout": executed.stdout,
            "stderr": executed.stderr,
            "payload": payload,
            "success": executed.return_code == 0,
        }

    def _ensure_dirs(self) -> None:
        """管理ディレクトリを作成する."""
        self._skills_dir.mkdir(parents=True, exist_ok=True)
        self._harnesses_dir.mkdir(parents=True, exist_ok=True)
        self._manifests_dir.mkdir(parents=True, exist_ok=True)
        self._cache_dir.mkdir(parents=True, exist_ok=True)

    def _manifest_path(self, harness_id: str) -> Path:
        """Manifest ファイルパスを返す."""
        return self._manifests_dir / f"{harness_id}.json"

    def _persist_manifest(self, manifest: CLIHarnessManifest) -> None:
        """Manifest を保存する."""
        self._manifest_path(manifest.harness_id).write_text(
            json.dumps(manifest.to_payload(), ensure_ascii=False, indent=2),
            encoding="utf-8",
        )

    def _validate_harness_dir(self, harness_dir: Path) -> None:
        """Import 対象の基本構造を検証する."""
        if not harness_dir.is_dir():
            msg = f"harness path is not a directory: {harness_dir}"
            raise FileNotFoundError(msg)
        has_setup = (harness_dir / "setup.py").is_file() or (harness_dir / "pyproject.toml").is_file()
        if not has_setup:
            msg = f"missing setup.py or pyproject.toml: {harness_dir}"
            raise FileNotFoundError(msg)
        self._discover_skill_md(harness_dir)

    def _discover_skill_md(self, harness_dir: Path) -> Path:
        """Harness 内の SKILL.md を検出する."""
        candidates = sorted(harness_dir.glob("**/skills/SKILL.md"))
        if candidates:
            return candidates[0]
        top_level = harness_dir / "SKILL.md"
        if top_level.is_file():
            return top_level
        msg = f"SKILL.md not found in harness: {harness_dir}"
        raise FileNotFoundError(msg)

    def _read_skill_frontmatter(self, skill_md_path: Path) -> dict[str, Any]:
        """SKILL.md frontmatter を読み取る."""
        content = skill_md_path.read_text(encoding="utf-8")
        match = _FRONTMATTER_RE.match(content)
        if not match:
            return {}
        data = yaml.safe_load(match.group(1))
        return data if isinstance(data, dict) else {}

    def _derive_software_name(self, harness_dir: Path, skill_name: str | None) -> str:
        """ソフトウェア名を導出する."""
        if skill_name:
            cleaned = str(skill_name).replace("cli-anything-", "").strip()
            if cleaned:
                return cleaned
        parent_name = harness_dir.parent.name.strip()
        if parent_name and parent_name != harness_dir.name:
            return parent_name
        return harness_dir.name.replace("agent-harness", "").strip("-_ ") or "cli-native"

    def _default_harness_id(self, software_name: str) -> str:
        """既定 harness_id を返す."""
        return f"cli-anything-{self._slugify(software_name)}"

    def _discover_cli_command(self, harness_dir: Path) -> str:
        """setup.py / pyproject.toml から console script 名を抽出する."""
        setup_path = harness_dir / "setup.py"
        if setup_path.is_file():
            setup_text = setup_path.read_text(encoding="utf-8")
            block_match = _CONSOLE_SCRIPT_BLOCK_RE.search(setup_text)
            if block_match:
                for match in _CONSOLE_SCRIPT_RE.finditer(block_match.group("body")):
                    command = match.group("command").strip()
                    if command:
                        return command
            name_match = _SETUP_NAME_RE.search(setup_text)
            if name_match:
                return name_match.group("name").strip()

        pyproject_path = harness_dir / "pyproject.toml"
        if pyproject_path.is_file():
            for line in pyproject_path.read_text(encoding="utf-8").splitlines():
                if "=" not in line:
                    continue
                key, _, value = line.partition("=")
                if key.strip().startswith(("cli-anything-", "cli_anything_", "project.scripts")):
                    return value.strip().strip('"').strip("'")

        msg = f"console script not found in harness: {harness_dir}"
        raise ValueError(msg)

    def _install_harness(self, harness_dir: Path) -> None:
        """Harness package を conda 環境へ editable install する."""
        command = [
            "conda",
            "run",
            "-n",
            "agentflow",
            "pip",
            "install",
            "-e",
            str(harness_dir),
        ]
        executed = self._run_command(command, cwd=harness_dir)
        if executed.return_code != 0:
            msg = f"failed to install harness: {executed.stderr or executed.stdout}"
            raise RuntimeError(msg)

    def _inspect_command_tree(
        self,
        cli_command: str,
        skill_md_path: Path,
    ) -> tuple[dict[str, list[str]], dict[str, list[str]]]:
        """CLI の command tree と option 一覧を抽出する."""
        try:
            root_help = self._run_cli_help([cli_command, "--help"])
            group_names = self._parse_help_commands(root_help.stdout)
            command_options: dict[str, list[str]] = {"__global__": self._parse_help_options(root_help.stdout)}
            command_groups: dict[str, list[str]] = {}

            for group_name in group_names:
                group_help = self._run_cli_help([cli_command, group_name, "--help"])
                command_groups[group_name] = self._parse_help_commands(group_help.stdout)
                command_options[group_name] = self._parse_help_options(group_help.stdout)
                for command_name in command_groups[group_name]:
                    leaf_help = self._run_cli_help([cli_command, group_name, command_name, "--help"])
                    command_options[f"{group_name} {command_name}"] = self._parse_help_options(leaf_help.stdout)

            if command_groups:
                return command_groups, command_options
        except RuntimeError:
            pass

        return self._parse_command_tree_from_skill_md(skill_md_path)

    def _run_cli_help(self, command: list[str]) -> CommandExecutionResult:
        """CLI help を conda 経由で実行する."""
        full_command = ["conda", "run", "-n", "agentflow", *command]
        executed = self._run_command(full_command, cwd=self._workspace_root)
        if executed.return_code != 0:
            msg = f"help inspection failed: {' '.join(command)}"
            raise RuntimeError(msg)
        return executed

    def _parse_command_tree_from_skill_md(
        self,
        skill_md_path: Path,
    ) -> tuple[dict[str, list[str]], dict[str, list[str]]]:
        """SKILL.md の表から command tree を復元する."""
        groups: dict[str, list[str]] = {}
        current_group: str | None = None
        for raw_line in skill_md_path.read_text(encoding="utf-8").splitlines():
            line = raw_line.strip()
            if line.startswith("### "):
                current_group = self._slugify(line.removeprefix("### ").strip())
                groups.setdefault(current_group, [])
                continue
            if current_group is None:
                continue
            match = _TABLE_COMMAND_RE.match(line)
            if match:
                groups[current_group].append(match.group("command"))
        return groups, {}

    def _write_shim_skill(
        self,
        *,
        harness_id: str,
        software_name: str,
        manifest_skill_md: Path,
        cli_command: str,
        command_groups: dict[str, list[str]],
    ) -> Path:
        """Project-local shim skill を生成する."""
        shim_dir = self._skills_dir / harness_id
        shim_dir.mkdir(parents=True, exist_ok=True)

        source_text = manifest_skill_md.read_text(encoding="utf-8")
        source_meta = self._read_skill_frontmatter(manifest_skill_md)
        description = str(source_meta.get("description", "")).strip() or f"CLI-native skill for {software_name}"
        group_list = ", ".join(sorted(command_groups.keys())) or "none"
        execute_tool_name = self._tool_name(software_name)
        shim_content = (
            "---\n"
            f"name: {harness_id}\n"
            f"description: {description}\n"
            "version: 1.0.0\n"
            "author: AgentFlow\n"
            "tags:\n"
            "  - cli-native\n"
            f"  - {self._slugify(software_name)}\n"
            "triggers:\n"
            f"  - {software_name}\n"
            f"  - {harness_id}\n"
            "allowed-tools:\n"
            f"  - {execute_tool_name}\n"
            f"argument-hint: \"subcommand='<group> <command>' args=[...]\"\n"
            "---\n\n"
            f"# {harness_id}\n\n"
            f"この Skill は `{cli_command}` を AgentFlow から安全に実行する shim です。\n\n"
            "## 実行ルール\n\n"
            "- 直接シェル実行せず、必ず登録済み execute tool を使う\n"
            "- `--json` は常に強制される\n"
            "- `project_path` には絶対パスを渡す\n"
            f"- 利用可能 group: {group_list}\n\n"
            "## 元 Skill 参照\n\n"
            f"```markdown\n{source_text.strip()}\n```\n"
        )
        skill_md_path = shim_dir / "SKILL.md"
        skill_md_path.write_text(shim_content, encoding="utf-8")
        return skill_md_path

    def _ensure_cli_anything_repo(self) -> Path:
        """Pinned CLI-Anything checkout を確保する."""
        repo_dir = self._cache_dir / f"cli-anything-{self._repo_ref[:12]}"
        if (repo_dir / ".git").is_dir():
            return repo_dir
        clone = self._run_command(["git", "clone", self._repo_url, str(repo_dir)], cwd=self._cache_dir)
        if clone.return_code != 0:
            msg = f"failed to clone CLI-Anything: {clone.stderr or clone.stdout}"
            raise RuntimeError(msg)
        checkout = self._run_command(["git", "checkout", self._repo_ref], cwd=repo_dir)
        if checkout.return_code != 0:
            msg = f"failed to checkout ref {self._repo_ref}: {checkout.stderr or checkout.stdout}"
            raise RuntimeError(msg)
        return repo_dir

    def _run_command(self, command: list[str], cwd: Path | None = None) -> CommandExecutionResult:
        """コマンドを同期実行する."""
        completed = subprocess.run(
            command,
            cwd=str(cwd or self._workspace_root),
            text=True,
            capture_output=True,
            check=False,
        )
        return CommandExecutionResult(
            return_code=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
        )

    def _validate_subcommand(self, manifest: CLIHarnessManifest, group_name: str, command_name: str) -> None:
        """許可 subcommand か検証する."""
        allowed_commands = manifest.command_groups.get(group_name)
        if allowed_commands is None:
            msg = f"unknown command group: {group_name}"
            raise ValueError(msg)
        if allowed_commands and command_name not in allowed_commands:
            msg = f"unknown command in group '{group_name}': {command_name}"
            raise ValueError(msg)

    def _validate_options(
        self,
        manifest: CLIHarnessManifest,
        command_parts: list[str],
        args: list[str],
    ) -> None:
        """許可 option だけを通す."""
        if not args or not manifest.command_options:
            return
        allowed_options = set(manifest.command_options.get("__global__", []))
        allowed_options.update(manifest.command_options.get(command_parts[0], []))
        allowed_options.update(manifest.command_options.get(" ".join(command_parts[:2]), []))
        allowed_options.add("--json")
        allowed_options.add("--project")
        allowed_options.add("--help")

        for arg in args:
            if not arg.startswith("-"):
                continue
            normalized = arg.split("=", 1)[0]
            if normalized not in allowed_options:
                msg = f"option not allowed for {' '.join(command_parts[:2])}: {normalized}"
                raise ValueError(msg)

    def _parse_help_commands(self, help_text: str) -> list[str]:
        """Click help から command 名を抽出する."""
        commands: list[str] = []
        in_commands_section = False
        for raw_line in help_text.splitlines():
            line = raw_line.rstrip()
            section_match = _HELP_SECTION_RE.match(line)
            if section_match:
                in_commands_section = section_match.group("header") == "Commands"
                continue
            if not in_commands_section:
                continue
            command_match = _HELP_COMMAND_RE.match(line)
            if command_match:
                commands.append(command_match.group("name").strip())
                continue
            if line.strip() == "":
                in_commands_section = False
        return commands

    def _parse_help_options(self, help_text: str) -> list[str]:
        """Click help から option 名を抽出する."""
        options: list[str] = []
        in_options_section = False
        for raw_line in help_text.splitlines():
            line = raw_line.rstrip()
            section_match = _HELP_SECTION_RE.match(line)
            if section_match:
                in_options_section = section_match.group("header") == "Options"
                continue
            if not in_options_section:
                continue
            stripped = line.strip()
            if stripped.startswith("-"):
                prefix = stripped.split("  ", 1)[0]
                for token in prefix.split(","):
                    option = token.strip().split(" ", 1)[0]
                    if option.startswith("-"):
                        options.append(option)
                continue
            if line.strip() == "":
                in_options_section = False
        return sorted(set(options))

    def _decode_output(self, stdout: str) -> dict[str, Any]:
        """stdout を JSON 優先で decode する."""
        text = stdout.strip()
        if not text:
            return {}
        try:
            payload = json.loads(text)
        except json.JSONDecodeError:
            return {"raw": text}
        return payload if isinstance(payload, dict) else {"result": payload}

    def _slugify(self, value: str) -> str:
        """識別子向けに slug 化する."""
        normalized = re.sub(r"[^a-z0-9]+", "-", value.strip().lower())
        normalized = normalized.strip("-")
        return normalized or "cli-native"

    def _tool_name(self, software_name: str) -> str:
        """生成する execute tool 名を返す."""
        return f"cli_native_{self._slugify(software_name).replace('-', '_')}_execute"
