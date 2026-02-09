"""システム情報スキル.

OSおよびリソース使用状況の読み取り専用APIを提供。

Example:
    >>> info = SystemInfoSkill(config)
    >>> os_info = await info.get_os_info()
    >>> usage = await info.get_resource_usage()
"""

from __future__ import annotations

import os
import platform
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.skills.os.base import OSSkillBase


if TYPE_CHECKING:
    from agentflow.skills.os.config import OSSkillConfig


@dataclass
class OSInfo:
    """OS情報."""

    system: str  # Linux, Darwin, Windows
    release: str
    version: str
    machine: str  # x86_64, arm64
    processor: str
    hostname: str
    python_version: str

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "system": self.system,
            "release": self.release,
            "version": self.version,
            "machine": self.machine,
            "processor": self.processor,
            "hostname": self.hostname,
            "python_version": self.python_version,
        }


@dataclass
class ResourceUsage:
    """リソース使用状況."""

    cpu_percent: float = 0.0
    memory_total_mb: float = 0.0
    memory_used_mb: float = 0.0
    memory_percent: float = 0.0
    disk_total_gb: float = 0.0
    disk_used_gb: float = 0.0
    disk_percent: float = 0.0
    measured_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "cpu_percent": self.cpu_percent,
            "memory_total_mb": self.memory_total_mb,
            "memory_used_mb": self.memory_used_mb,
            "memory_percent": self.memory_percent,
            "disk_total_gb": self.disk_total_gb,
            "disk_used_gb": self.disk_used_gb,
            "disk_percent": self.disk_percent,
            "measured_at": self.measured_at.isoformat(),
        }


class SystemInfoSkill(OSSkillBase):
    """システム情報スキル.

    読み取り専用のシステム情報取得を提供。
    isolated/real_machine 両モードで使用可能。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)

    async def get_os_info(self) -> OSInfo:
        """OS情報を取得."""
        self._audit_log("get_os_info", {})

        return OSInfo(
            system=platform.system(),
            release=platform.release(),
            version=platform.version(),
            machine=platform.machine(),
            processor=platform.processor(),
            hostname=platform.node(),
            python_version=platform.python_version(),
        )

    async def get_env(self, key: str, default: str = "") -> str:
        """環境変数を取得.

        Args:
            key: 環境変数名
            default: デフォルト値

        Returns:
            環境変数の値
        """
        # セキュリティ上、一部の環境変数は取得禁止
        forbidden_keys = {
            "AWS_SECRET_ACCESS_KEY", "OPENAI_API_KEY", "ANTHROPIC_API_KEY",
            "DATABASE_URL", "PASSWORD", "SECRET", "TOKEN", "PRIVATE_KEY",
        }

        if any(forbidden in key.upper() for forbidden in forbidden_keys):
            self._logger.warning(f"環境変数 '{key}' の取得は禁止されています")
            return default

        self._audit_log("get_env", {"key": key})
        return os.environ.get(key, default)

    async def get_resource_usage(self) -> ResourceUsage:
        """リソース使用状況を取得."""
        self._audit_log("get_resource_usage", {})

        try:
            import psutil

            cpu = psutil.cpu_percent(interval=0.1)
            mem = psutil.virtual_memory()
            disk = psutil.disk_usage(str(self._config.workspace_path))

            return ResourceUsage(
                cpu_percent=cpu,
                memory_total_mb=mem.total / (1024 * 1024),
                memory_used_mb=mem.used / (1024 * 1024),
                memory_percent=mem.percent,
                disk_total_gb=disk.total / (1024 * 1024 * 1024),
                disk_used_gb=disk.used / (1024 * 1024 * 1024),
                disk_percent=disk.percent,
            )

        except ImportError:
            # psutil がない場合は基本情報のみ
            self._logger.warning("psutil が未インストール。基本情報のみ返します")
            return ResourceUsage()

    async def get_workspace_info(self) -> dict[str, Any]:
        """ワークスペース情報を取得."""
        ws = self._config.workspace_path

        return {
            "path": str(ws),
            "exists": ws.exists(),
            "is_dir": ws.is_dir() if ws.exists() else False,
            "execution_mode": self._config.execution_mode.value,
            "allow_write": self._config.allow_write,
            "allow_delete": self._config.allow_delete,
        }

