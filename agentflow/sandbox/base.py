# -*- coding: utf-8 -*-
"""サンドボックス基底クラス.

全てのサンドボックスプロバイダが実装すべきインターフェースを定義。

Example:
    >>> class MyProvider(SandboxProvider):
    ...     async def execute(self, code: str, **kwargs) -> ExecutionResult:
    ...         # 実行ロジック
    ...         return ExecutionResult(stdout="result", exit_code=0)
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import Any


@dataclass
class SandboxConfig:
    """サンドボックス設定.

    Attributes:
        timeout: デフォルトタイムアウト秒
        memory_mb: メモリ制限（MB）
        cpus: CPU コア数
        image: ベースイメージ名
        server_url: サーバー URL（microsandbox 用）
        api_key: API キー（e2b 用）
    """

    timeout: float = 60.0
    memory_mb: int = 1024
    cpus: int = 1
    image: str = "python"
    server_url: str | None = None
    api_key: str | None = None
    extra: dict[str, Any] = field(default_factory=dict)


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
        pass

    async def __aenter__(self) -> "SandboxProvider":
        """async with サポート."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with サポート."""
        await self.close()

