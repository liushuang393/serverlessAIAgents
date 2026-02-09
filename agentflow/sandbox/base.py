"""サンドボックス基底クラス.

全てのサンドボックスプロバイダが実装すべきインターフェースを定義。
Daytona設計を参考にライフサイクル管理とリソース監視機能を追加。

設計原則（Daytonaより学習）:
- ライフサイクル管理: CREATED → STARTED → STOPPED → ARCHIVED → DELETED
- リソース制限: CPU/Memory/IO/ネットワークの統一管理
- 自動クリーンアップ: 非アクティブ時の自動停止・アーカイブ

Example:
    >>> class MyProvider(SandboxProvider):
    ...     async def execute(self, code: str, **kwargs) -> ExecutionResult:
    ...         # 実行ロジック
    ...         return ExecutionResult(stdout="result", exit_code=0)
    >>>
    >>> # ライフサイクル管理付きサンドボックス
    >>> from agentflow.sandbox import ManagedSandbox
    >>> sandbox = ManagedSandbox(provider="docker")
    >>> await sandbox.start()
    >>> result = await sandbox.execute("print('Hello')")
    >>> await sandbox.stop()
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any


class SandboxState(str, Enum):
    """サンドボックス状態.

    Daytonaのライフサイクル設計を参考に定義。

    状態遷移:
        CREATED → STARTED → STOPPED → ARCHIVED → DELETED
                     ↑         ↓
                     └─────────┘ (再起動可能)

    Attributes:
        CREATED: 作成済み、未起動
        STARTED: 実行中、コード実行可能
        STOPPED: 停止中、再起動可能
        ARCHIVED: アーカイブ済み、復元可能
        DELETED: 削除済み
    """

    CREATED = "created"
    STARTED = "started"
    STOPPED = "stopped"
    ARCHIVED = "archived"
    DELETED = "deleted"

    @classmethod
    def can_transition(cls, from_state: SandboxState, to_state: SandboxState) -> bool:
        """状態遷移が許可されているかチェック.

        Args:
            from_state: 現在の状態
            to_state: 遷移先の状態

        Returns:
            遷移可能かどうか
        """
        valid_transitions: dict[SandboxState, set[SandboxState]] = {
            cls.CREATED: {cls.STARTED, cls.DELETED},
            cls.STARTED: {cls.STOPPED, cls.DELETED},
            cls.STOPPED: {cls.STARTED, cls.ARCHIVED, cls.DELETED},
            cls.ARCHIVED: {cls.STARTED, cls.DELETED},
            cls.DELETED: set(),
        }
        return to_state in valid_transitions.get(from_state, set())


@dataclass
class ResourceUsage:
    """リソース使用状況.

    サンドボックスのリソース消費を監視するためのデータクラス。

    Attributes:
        cpu_percent: CPU使用率（0.0-100.0）
        memory_mb: メモリ使用量（MB）
        disk_mb: ディスク使用量（MB）
        network_rx_bytes: ネットワーク受信バイト数
        network_tx_bytes: ネットワーク送信バイト数
        measured_at: 測定時刻
    """

    cpu_percent: float = 0.0
    memory_mb: float = 0.0
    disk_mb: float = 0.0
    network_rx_bytes: int = 0
    network_tx_bytes: int = 0
    measured_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "cpu_percent": self.cpu_percent,
            "memory_mb": self.memory_mb,
            "disk_mb": self.disk_mb,
            "network_rx_bytes": self.network_rx_bytes,
            "network_tx_bytes": self.network_tx_bytes,
            "measured_at": self.measured_at.isoformat(),
        }


@dataclass
class ResourceLimits:
    """リソース制限設定.

    Daytonaのリソース制限機能を参考に設計。

    Attributes:
        max_cpu_percent: 最大CPU使用率
        max_memory_mb: 最大メモリ（MB）
        max_disk_mb: 最大ディスク（MB）
        network_enabled: ネットワーク有効化
        allowed_hosts: 許可されたホスト（空の場合は全て許可）
    """

    max_cpu_percent: float = 100.0
    max_memory_mb: int = 1024
    max_disk_mb: int = 1024
    network_enabled: bool = False
    allowed_hosts: list[str] = field(default_factory=list)


@dataclass
class SandboxConfig:
    """サンドボックス設定.

    Daytonaの設定モデルを参考に、ライフサイクル管理とリソース制限を追加。

    Attributes:
        timeout: デフォルトタイムアウト秒
        memory_mb: メモリ制限（MB）
        cpus: CPU コア数
        image: ベースイメージ名
        server_url: サーバー URL（microsandbox 用）
        api_key: API キー（e2b 用）
        auto_stop_seconds: 非アクティブ時の自動停止時間（0で無効）
        auto_archive_seconds: 停止後の自動アーカイブ時間（0で無効）
        resource_limits: リソース制限設定
    """

    timeout: float = 60.0
    memory_mb: int = 1024
    cpus: int = 1
    image: str = "python"
    server_url: str | None = None
    api_key: str | None = None
    extra: dict[str, Any] = field(default_factory=dict)
    # ライフサイクル管理（Daytonaより学習）
    auto_stop_seconds: int = 0  # 0 = 無効
    auto_archive_seconds: int = 0  # 0 = 無効
    resource_limits: ResourceLimits | None = None


@dataclass
class ExecutionResult:
    """コード実行結果.

    Attributes:
        stdout: 標準出力
        stderr: 標準エラー出力
        exit_code: 終了コード
        duration_ms: 実行時間（ミリ秒）
        files: 生成されたファイル
        error: エラーメッセージ
    """

    stdout: str = ""
    stderr: str = ""
    exit_code: int = 0
    duration_ms: float = 0.0
    files: dict[str, bytes] = field(default_factory=dict)
    error: str | None = None
    executed_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    @property
    def success(self) -> bool:
        """実行成功かどうか."""
        return self.exit_code == 0 and self.error is None

    @property
    def output(self) -> str:
        """stdout と stderr を結合."""
        parts = []
        if self.stdout:
            parts.append(self.stdout)
        if self.stderr:
            parts.append(f"[stderr]\n{self.stderr}")
        return "\n".join(parts)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "stdout": self.stdout,
            "stderr": self.stderr,
            "exit_code": self.exit_code,
            "duration_ms": self.duration_ms,
            "success": self.success,
            "error": self.error,
            "executed_at": self.executed_at.isoformat(),
        }


class SandboxProvider(ABC):
    """サンドボックスプロバイダ基底クラス.

    全てのプロバイダが実装すべきインターフェースを定義。
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化.

        Args:
            config: サンドボックス設定
        """
        self._config = config or SandboxConfig()

    @property
    def config(self) -> SandboxConfig:
        """設定を取得."""
        return self._config

    @abstractmethod
    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ExecutionResult:
        """コードを実行.

        Args:
            code: 実行する Python コード
            timeout: タイムアウト秒（None でデフォルト使用）
            packages: インストールするパッケージ
            env: 環境変数
            files: サンドボックスに配置するファイル

        Returns:
            ExecutionResult
        """
        ...

    async def close(self) -> None:
        """リソースを解放.

        オーバーライド可能。デフォルトは何もしない。
        """

    async def __aenter__(self) -> SandboxProvider:
        """async with サポート."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with サポート."""
        await self.close()

