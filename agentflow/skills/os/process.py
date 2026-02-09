"""プロセス制御スキル.

安全なプロセス管理APIを提供。real_machine モード専用。

Example:
    >>> proc = ProcessSkill(config)
    >>> info = await proc.start_process("python", ["-m", "http.server"])
    >>> await proc.stop_process(info.pid)
"""

from __future__ import annotations

import asyncio
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.skills.os.base import OSSkillBase, OSSkillError


if TYPE_CHECKING:
    from agentflow.skills.os.config import OSSkillConfig


@dataclass
class ProcessInfo:
    """プロセス情報."""

    pid: int
    command: str
    args: list[str]
    status: str = "running"
    started_at: datetime = field(default_factory=datetime.now)
    exit_code: int | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "pid": self.pid,
            "command": self.command,
            "args": self.args,
            "status": self.status,
            "started_at": self.started_at.isoformat(),
            "exit_code": self.exit_code,
        }


class ProcessSkill(OSSkillBase):
    """プロセス制御スキル.

    Agent が起動したプロセスのみを管理。
    real_machine モードでのみ使用可能。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        # このスキルが起動したプロセスのみを追跡
        self._managed_processes: dict[int, tuple[asyncio.subprocess.Process, ProcessInfo]] = {}

    async def start_process(
        self,
        command: str,
        args: list[str] | None = None,
        background: bool = True,
        cwd: str | None = None,
        env: dict[str, str] | None = None,
    ) -> ProcessInfo:
        """プロセスを起動.

        Args:
            command: コマンド
            args: 引数
            background: バックグラウンド実行
            cwd: 作業ディレクトリ
            env: 環境変数

        Returns:
            プロセス情報
        """
        self._check_real_machine_mode()
        args = args or []

        # コマンド検証
        full_cmd = f"{command} {' '.join(args)}".strip()
        self._validate_command(full_cmd)

        # 作業ディレクトリ
        work_dir = self._config.workspace_path
        if cwd:
            work_dir = self._validate_path(cwd)

        self._audit_log("start_process", {
            "command": command,
            "args": args,
            "background": background,
        })

        proc = await asyncio.create_subprocess_exec(
            command,
            *args,
            cwd=work_dir,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=env,
        )

        info = ProcessInfo(
            pid=proc.pid,
            command=command,
            args=args,
            status="running",
        )

        self._managed_processes[proc.pid] = (proc, info)
        self._logger.info(f"プロセス起動: PID={proc.pid}, command={command}")

        return info

    async def stop_process(self, pid: int, force: bool = False) -> bool:
        """プロセスを停止.

        Args:
            pid: プロセスID（このスキルが起動したもののみ）
            force: 強制終了

        Returns:
            成功した場合 True
        """
        self._check_real_machine_mode()

        if pid not in self._managed_processes:
            msg = f"管理対象外のプロセスです: PID={pid}"
            raise OSSkillError(msg, skill_name="ProcessSkill")

        proc, info = self._managed_processes[pid]

        self._audit_log("stop_process", {"pid": pid, "force": force})

        try:
            if force:
                proc.kill()
            else:
                proc.terminate()

            await asyncio.wait_for(proc.wait(), timeout=5.0)
            info.status = "stopped"
            info.exit_code = proc.returncode

            del self._managed_processes[pid]
            self._logger.info(f"プロセス停止: PID={pid}")
            return True

        except TimeoutError:
            proc.kill()
            info.status = "killed"
            del self._managed_processes[pid]
            return True

    async def list_managed_processes(self) -> list[ProcessInfo]:
        """管理中のプロセス一覧を取得."""
        result = []
        for _pid, (proc, info) in list(self._managed_processes.items()):
            # 状態更新
            if proc.returncode is not None:
                info.status = "exited"
                info.exit_code = proc.returncode
            result.append(info)
        return result

    async def stop_all(self) -> int:
        """全ての管理プロセスを停止（Kill Switch）.

        Returns:
            停止したプロセス数
        """
        count = 0
        for pid in list(self._managed_processes.keys()):
            try:
                await self.stop_process(pid, force=True)
                count += 1
            except Exception as e:
                self._logger.exception(f"プロセス停止失敗: PID={pid}, error={e}")
        return count

    async def __aenter__(self) -> ProcessSkill:
        """async with サポート."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """クリーンアップ: 全プロセス停止."""
        await self.stop_all()

