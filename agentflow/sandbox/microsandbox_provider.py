# -*- coding: utf-8 -*-
"""Microsandbox プロバイダ.

セルフホスト可能な microsandbox を使用したサンドボックス実行。
microVM ベースの強力な隔離を提供。

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
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    SandboxConfig,
    SandboxProvider,
)

logger = logging.getLogger(__name__)


class MicrosandboxProvider(SandboxProvider):
    """Microsandbox プロバイダ.

    microVM ベースのセキュアなサンドボックス。
    https://github.com/zerocore-ai/microsandbox

    Attributes:
        server_url: microsandbox サーバー URL
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

        start_time = time.time()
        effective_timeout = timeout or self._config.timeout

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

            return ExecutionResult(
                stdout=output.strip() if output else "",
                stderr="",
                exit_code=0,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            logger.error(f"Microsandbox execution error: {e}")
            return ExecutionResult(
                stderr=str(e),
                exit_code=1,
                duration_ms=duration_ms,
                error=f"{type(e).__name__}: {e}",
            )

    async def close(self) -> None:
        """リソース解放."""
        # microsandbox は async with で自動解放されるため、特に何もしない
        pass

