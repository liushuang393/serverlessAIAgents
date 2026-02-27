"""OS/Browserスキル設定モデル.

セキュリティ制約を定義する設定クラス群。

Example:
    >>> config = OSSkillConfig(
    ...     workspace_path=Path("./workspace"),
    ...     command_whitelist=["ls", "cat", "grep"],
    ... )
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import Any


class ExecutionMode(StrEnum):
    """実行モード.

    Attributes:
        ISOLATED: サンドボックス実行（デフォルト、安全）
        REAL_MACHINE: 真実機実行（高リスク、要人工確認）
    """

    ISOLATED = "isolated"
    REAL_MACHINE = "real_machine"


# デフォルトのコマンドホワイトリスト（安全なコマンドのみ）
DEFAULT_COMMAND_WHITELIST: list[str] = [
    # ファイル閲覧系
    "ls",
    "cat",
    "head",
    "tail",
    "less",
    "more",
    "find",
    "grep",
    "wc",
    "file",
    "stat",
    # ディレクトリ操作
    "pwd",
    "cd",
    "mkdir",
    # テキスト処理
    "echo",
    "printf",
    "sort",
    "uniq",
    "cut",
    "awk",
    "sed",
    # システム情報
    "date",
    "whoami",
    "hostname",
    "uname",
    "env",
    "df",
    "du",
    "free",
    "uptime",
    "ps",
    # 開発ツール
    "python",
    "python3",
    "pip",
    "node",
    "npm",
    "git",
    # ネットワーク（読み取りのみ）
    "curl",
    "wget",
    "ping",
]

# 絶対禁止コマンド
DEFAULT_COMMAND_BLACKLIST: list[str] = [
    # 危険なシステム操作
    "rm",
    "rmdir",
    "dd",
    "mkfs",
    "fdisk",
    # 権限昇格
    "sudo",
    "su",
    "chmod",
    "chown",
    "chgrp",
    # システム制御
    "shutdown",
    "reboot",
    "init",
    "systemctl",
    "kill",
    "killall",
    "pkill",
    # ネットワーク変更
    "iptables",
    "ip",
    "ifconfig",
    "route",
    # 危険なシェル操作
    "eval",
    "exec",
    "source",
]


@dataclass
class OSSkillConfig:
    """OS スキル設定.

    セキュリティ制約とリソース制限を定義。

    Attributes:
        workspace_path: 許可されたワークスペースパス
        execution_mode: 実行モード（isolated/real_machine）
        command_whitelist: 許可コマンドリスト
        command_blacklist: 禁止コマンドリスト
        domain_whitelist: 許可ドメイン（HTTP リクエスト用）
        allowed_paths: 追加許可パスのホワイトリスト（workspace_path に加えてアクセス可能）
        max_file_size_mb: 最大ファイルサイズ（MB）
        max_timeout_seconds: 最大タイムアウト（秒）
        allow_write: 書き込み許可（デフォルト禁止）
        allow_delete: 削除許可（デフォルト禁止）
        require_human_confirmation: 危険操作時に人工確認必須
    """

    workspace_path: Path = field(default_factory=Path.cwd)
    execution_mode: ExecutionMode = ExecutionMode.ISOLATED
    command_whitelist: list[str] = field(default_factory=lambda: list(DEFAULT_COMMAND_WHITELIST))
    command_blacklist: list[str] = field(default_factory=lambda: list(DEFAULT_COMMAND_BLACKLIST))
    domain_whitelist: list[str] = field(default_factory=list)
    allowed_paths: list[str] = field(default_factory=list)
    max_file_size_mb: int = 10
    max_timeout_seconds: int = 60
    allow_write: bool = False
    allow_delete: bool = False
    require_human_confirmation: bool = True

    def __post_init__(self) -> None:
        """初期化後処理: パス正規化."""
        self.workspace_path = self.workspace_path.resolve()

    def is_command_allowed(self, command: str) -> bool:
        """コマンドが許可されているか判定."""
        base_cmd = command.split(maxsplit=1)[0] if command else ""
        if base_cmd in self.command_blacklist:
            return False
        return base_cmd in self.command_whitelist

    def is_domain_allowed(self, domain: str) -> bool:
        """ドメインが許可されているか判定."""
        if not self.domain_whitelist:
            return False  # ホワイトリスト空の場合は全て拒否
        return any(domain == allowed or domain.endswith(f".{allowed}") for allowed in self.domain_whitelist)

    def is_path_in_workspace(self, path: Path) -> bool:
        """パスがワークスペース内か、または allowed_paths のいずれかのサブパスか判定."""
        try:
            resolved = path.resolve()
            # workspace_path チェック
            if resolved.is_relative_to(self.workspace_path):
                return True
            # allowed_paths ホワイトリストチェック
            for allowed_str in self.allowed_paths:
                allowed = Path(allowed_str).resolve()
                if resolved.is_relative_to(allowed):
                    return True
            return False
        except (ValueError, RuntimeError):
            return False

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "workspace_path": str(self.workspace_path),
            "execution_mode": self.execution_mode.value,
            "command_whitelist": self.command_whitelist,
            "command_blacklist": self.command_blacklist,
            "domain_whitelist": self.domain_whitelist,
            "allowed_paths": self.allowed_paths,
            "max_file_size_mb": self.max_file_size_mb,
            "max_timeout_seconds": self.max_timeout_seconds,
            "allow_write": self.allow_write,
            "allow_delete": self.allow_delete,
            "require_human_confirmation": self.require_human_confirmation,
        }
