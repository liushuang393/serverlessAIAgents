"""OS スキル基底クラス.

全ての OS スキルが継承する共通基盤。
パス検証、セキュリティチェック、ログ出力を提供。

Example:
    >>> class MyOSSkill(OSSkillBase):
    ...     async def my_operation(self, path: str) -> str:
    ...         validated = self._validate_path(path)
    ...         return await self._do_something(validated)
"""

from __future__ import annotations

import logging
from abc import ABC
from pathlib import Path
from typing import Any

from agentflow.skills.os.config import ExecutionMode, OSSkillConfig


class OSSkillError(Exception):
    """OS スキルエラー基底クラス."""

    def __init__(self, message: str, skill_name: str = "", details: dict[str, Any] | None = None) -> None:
        """初期化."""
        super().__init__(message)
        self.skill_name = skill_name
        self.details = details or {}


class PathSecurityError(OSSkillError):
    """パスセキュリティ違反."""


class CommandSecurityError(OSSkillError):
    """コマンドセキュリティ違反."""


class ExecutionModeError(OSSkillError):
    """実行モード違反."""


class OSSkillBase(ABC):
    """OS スキル基底クラス.

    セキュリティ検証とログ出力の共通処理を提供。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化.

        Args:
            config: OS スキル設定（None の場合はデフォルト）
        """
        self._config = config or OSSkillConfig()
        self._logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")

    @property
    def config(self) -> OSSkillConfig:
        """設定を取得."""
        return self._config

    @property
    def execution_mode(self) -> ExecutionMode:
        """実行モードを取得."""
        return self._config.execution_mode

    def _validate_path(self, path: str | Path) -> Path:
        """パスを検証・正規化.

        Args:
            path: 検証するパス

        Returns:
            正規化されたパス

        Raises:
            PathSecurityError: パスがワークスペース外の場合
        """
        target = Path(path)

        # 相対パスの場合はワークスペースからの相対パスとして解決
        if not target.is_absolute():
            target = self._config.workspace_path / target

        target = target.resolve()

        # ワークスペース外へのアクセスを禁止
        if not self._config.is_path_in_workspace(target):
            msg = f"アクセス拒否: パス '{target}' はワークスペース外です"
            self._logger.warning(msg)
            raise PathSecurityError(
                msg,
                skill_name=self.__class__.__name__,
                details={"path": str(target), "workspace": str(self._config.workspace_path)},
            )

        return target

    def _validate_command(self, command: str) -> str:
        """コマンドを検証.

        Args:
            command: 検証するコマンド

        Returns:
            検証済みコマンド

        Raises:
            CommandSecurityError: コマンドが許可されていない場合
        """
        if not self._config.is_command_allowed(command):
            base_cmd = command.split(maxsplit=1)[0] if command else ""
            msg = f"コマンド拒否: '{base_cmd}' は許可されていません"
            self._logger.warning(msg)
            raise CommandSecurityError(
                msg,
                skill_name=self.__class__.__name__,
                details={"command": command, "whitelist": self._config.command_whitelist},
            )
        return command

    def _check_write_permission(self) -> None:
        """書き込み権限をチェック.

        Raises:
            ExecutionModeError: 書き込みが許可されていない場合
        """
        if not self._config.allow_write:
            msg = "書き込み操作は許可されていません"
            raise ExecutionModeError(msg, skill_name=self.__class__.__name__)

    def _check_delete_permission(self) -> None:
        """削除権限をチェック.

        Raises:
            ExecutionModeError: 削除が許可されていない場合
        """
        if not self._config.allow_delete:
            msg = "削除操作は許可されていません"
            raise ExecutionModeError(msg, skill_name=self.__class__.__name__)

    def _check_real_machine_mode(self) -> None:
        """real_machine モード専用操作かチェック.

        Raises:
            ExecutionModeError: isolated モードの場合
        """
        if self._config.execution_mode != ExecutionMode.REAL_MACHINE:
            msg = "この操作は real_machine モードでのみ許可されています"
            raise ExecutionModeError(msg, skill_name=self.__class__.__name__)

    def _audit_log(self, operation: str, details: dict[str, Any]) -> None:
        """監査ログを出力.

        Args:
            operation: 操作名
            details: 詳細情報
        """
        self._logger.info(
            "AUDIT: skill=%s, operation=%s, mode=%s, details=%s",
            self.__class__.__name__,
            operation,
            self._config.execution_mode.value,
            details,
        )
