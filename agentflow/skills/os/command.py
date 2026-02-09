"""コマンド実行スキル.

安全なコマンド実行APIを提供。ホワイトリスト制限付き。

Example:
    >>> cmd = CommandSkill(config)
    >>> result = await cmd.run_command("linux", "ls", ["-la"], timeout=10)
"""

from __future__ import annotations

import asyncio
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any, Literal

from agentflow.skills.os.base import CommandSecurityError, OSSkillBase, OSSkillError


if TYPE_CHECKING:
    from agentflow.skills.os.config import OSSkillConfig


@dataclass
class CommandResult:
    """コマンド実行結果."""

    command: str
    args: list[str]
    stdout: str = ""
    stderr: str = ""
    exit_code: int = 0
    duration_ms: float = 0.0
    dry_run: bool = False
    executed_at: datetime = field(default_factory=datetime.now)

    @property
    def success(self) -> bool:
        """実行成功かどうか."""
        return self.exit_code == 0

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "command": self.command,
            "args": self.args,
            "stdout": self.stdout,
            "stderr": self.stderr,
            "exit_code": self.exit_code,
            "success": self.success,
            "duration_ms": self.duration_ms,
            "dry_run": self.dry_run,
            "executed_at": self.executed_at.isoformat(),
        }


OSType = Literal["linux", "macos", "windows"]


class CommandSkill(OSSkillBase):
    """コマンド実行スキル.

    ホワイトリストに基づく安全なコマンド実行を提供。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        self._running_processes: dict[int, asyncio.subprocess.Process] = {}

    async def run_command(
        self,
        os_type: OSType,
        command: str,
        args: list[str] | None = None,
        cwd: str | None = None,
        timeout: float | None = None,
        dry_run: bool = False,
        env: dict[str, str] | None = None,
    ) -> CommandResult:
        """コマンドを実行.

        Args:
            os_type: OS タイプ（linux/macos/windows）
            command: 実行するコマンド
            args: コマンド引数
            cwd: 作業ディレクトリ（ワークスペース相対）
            timeout: タイムアウト秒
            dry_run: True の場合は実行せずに検証のみ
            env: 追加環境変数

        Returns:
            実行結果

        Raises:
            CommandSecurityError: コマンドが許可されていない場合
            OSSkillError: 実行エラーの場合
        """
        args = args or []
        timeout = timeout or self._config.max_timeout_seconds

        # コマンド検証
        full_cmd = f"{command} {' '.join(args)}".strip()
        self._validate_command(full_cmd)

        # 作業ディレクトリ検証
        work_dir = self._config.workspace_path
        if cwd:
            work_dir = self._validate_path(cwd)

        # dry_run モードの場合は検証のみ
        if dry_run:
            self._audit_log("run_command_dry_run", {"command": command, "args": args})
            return CommandResult(
                command=command,
                args=args,
                stdout=f"[dry_run] Command validated: {command} {' '.join(args)}",
                dry_run=True,
            )

        self._audit_log("run_command", {
            "command": command,
            "args": args,
            "cwd": str(work_dir),
            "timeout": timeout,
        })

        # コマンド実行
        start_time = asyncio.get_event_loop().time()
        try:
            proc = await asyncio.create_subprocess_exec(
                command,
                *args,
                cwd=work_dir,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                env=env,
            )

            stdout, stderr = await asyncio.wait_for(
                proc.communicate(),
                timeout=timeout,
            )

            duration_ms = (asyncio.get_event_loop().time() - start_time) * 1000

            return CommandResult(
                command=command,
                args=args,
                stdout=stdout.decode("utf-8", errors="replace"),
                stderr=stderr.decode("utf-8", errors="replace"),
                exit_code=proc.returncode or 0,
                duration_ms=duration_ms,
            )

        except TimeoutError:
            msg = f"コマンドタイムアウト: {command} ({timeout}秒)"
            raise OSSkillError(msg, skill_name="CommandSkill")

        except FileNotFoundError:
            msg = f"コマンドが見つかりません: {command}"
            raise OSSkillError(msg, skill_name="CommandSkill")

        except PermissionError:
            msg = f"実行権限がありません: {command}"
            raise OSSkillError(msg, skill_name="CommandSkill")

    async def run_shell(
        self,
        script: str,
        timeout: float | None = None,
        dry_run: bool = False,
    ) -> CommandResult:
        """シェルスクリプトを実行（制限付き）.

        注意: シェル実行は real_machine モードでのみ許可。

        Args:
            script: シェルスクリプト
            timeout: タイムアウト秒
            dry_run: True の場合は検証のみ

        Returns:
            実行結果
        """
        # real_machine モード必須
        self._check_real_machine_mode()

        # 危険なパターンチェック
        dangerous_patterns = ["|", ">", "<", "&&", "||", ";", "`", "$(",]
        for pattern in dangerous_patterns:
            if pattern in script:
                msg = f"シェルスクリプトに危険なパターン '{pattern}' が含まれています"
                raise CommandSecurityError(msg, skill_name="CommandSkill")

        # スクリプトを個別コマンドに分解して検証
        commands = script.strip().split("\n")
        for cmd in commands:
            cmd = cmd.strip()
            if cmd and not cmd.startswith("#"):
                self._validate_command(cmd)

        if dry_run:
            return CommandResult(
                command="shell",
                args=[script],
                stdout=f"[dry_run] Shell script validated: {len(commands)} commands",
                dry_run=True,
            )

        # bash -c で実行
        return await self.run_command(
            "linux",
            "bash",
            ["-c", script],
            timeout=timeout,
        )

