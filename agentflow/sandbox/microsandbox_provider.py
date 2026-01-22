# -*- coding: utf-8 -*-
"""Microsandbox プロバイダ.

セルフホスト可能な microsandbox を使用したサンドボックス実行。
microVM ベースの強力な隔離を提供。Daytonaスタイルの生命周期管理を統合。

前提条件:
    - microsandbox サーバーが起動していること
    - pip install microsandbox

環境変数:
    MICROSANDBOX_SERVER: サーバー URL（デフォルト: http://localhost:5555）

使用例:
    >>> from agentflow.sandbox import get_sandbox
    >>>
    >>> sandbox = get_sandbox(provider="microsandbox")
    >>> result = await sandbox.execute("print('Hello from microVM!')")
    >>> print(result.stdout)
"""

from __future__ import annotations

import logging
import os
import time
from datetime import UTC, datetime
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    ResourceUsage,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)

logger = logging.getLogger(__name__)


class MicrosandboxProvider(SandboxProvider):
    """Microsandbox プロバイダ.

    microVM ベースのセキュアなサンドボックス。
    https://github.com/zerocore-ai/microsandbox
    Daytonaスタイルの生命周期追跡を含む。

    Attributes:
        server_url: microsandbox サーバー URL
        state: 現在の状態
        execution_count: 実行回数
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化.

        Args:
            config: サンドボックス設定

        Raises:
            ImportError: microsandbox パッケージがインストールされていない
        """
        super().__init__(config)

        # サーバー URL
        self._server_url = (
            config.server_url
            if config and config.server_url
            else os.getenv("MICROSANDBOX_SERVER", "http://localhost:5555")
        )

        # 生命周期追跡（Daytonaスタイル）
        self._state = SandboxState.CREATED
        self._created_at = datetime.now(UTC)
        self._last_activity_at = self._created_at
        self._execution_count = 0
        self._total_execution_ms = 0.0

        # SDK インポートチェック
        try:
            import microsandbox  # noqa: F401
            self._sdk_available = True
        except ImportError:
            self._sdk_available = False
            logger.warning(
                "microsandbox パッケージがインストールされていません。"
                "pip install microsandbox を実行してください。"
            )

        logger.info(f"Microsandbox provider initialized: {self._server_url}")

    @property
    def state(self) -> SandboxState:
        """現在の状態."""
        return self._state

    @property
    def execution_count(self) -> int:
        """実行回数."""
        return self._execution_count

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "provider": "microsandbox",
            "state": self._state.value,
            "server_url": self._server_url,
            "sdk_available": self._sdk_available,
            "created_at": self._created_at.isoformat(),
            "last_activity_at": self._last_activity_at.isoformat(),
            "execution_count": self._execution_count,
            "total_execution_ms": self._total_execution_ms,
        }

    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ExecutionResult:
        """コードを microsandbox で実行.

        Args:
            code: Python コード
            timeout: タイムアウト秒
            packages: インストールするパッケージ
            env: 環境変数
            files: サンドボックスに配置するファイル

        Returns:
            ExecutionResult
        """
        if not self._sdk_available:
            return ExecutionResult(
                exit_code=1,
                error="microsandbox SDK がインストールされていません",
            )

        # 状態を STARTED に遷移（初回実行時）
        if self._state == SandboxState.CREATED:
            self._state = SandboxState.STARTED

        start_time = time.time()
        self._last_activity_at = datetime.now(UTC)

        try:
            from microsandbox import PythonSandbox

            # サンドボックス作成・実行
            async with PythonSandbox.create(
                name=f"agentflow-{int(time.time())}",
            ) as sb:
                # パッケージインストール
                if packages:
                    pip_cmd = f"import subprocess; subprocess.run(['pip', 'install', '-q', {', '.join(repr(p) for p in packages)}])"
                    await sb.run(pip_cmd)

                # ファイル配置
                if files:
                    for filename, content in files.items():
                        # Base64 エンコードで転送
                        import base64
                        b64 = base64.b64encode(content).decode()
                        await sb.run(
                            f"import base64; open('{filename}', 'wb').write(base64.b64decode('{b64}'))"
                        )

                # メインコード実行
                exec_result = await sb.run(code)
                output = await exec_result.output()

            duration_ms = (time.time() - start_time) * 1000

            # 実行統計を更新
            self._execution_count += 1
            self._total_execution_ms += duration_ms
            self._last_activity_at = datetime.now(UTC)

            return ExecutionResult(
                stdout=output.strip() if output else "",
                stderr="",
                exit_code=0,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._execution_count += 1
            self._total_execution_ms += duration_ms
            logger.error(f"Microsandbox execution error: {e}")
            return ExecutionResult(
                stderr=str(e),
                exit_code=1,
                duration_ms=duration_ms,
                error=f"{type(e).__name__}: {e}",
            )

    async def close(self) -> None:
        """リソース解放."""
        self._state = SandboxState.STOPPED
        logger.info("Microsandbox provider closed")

